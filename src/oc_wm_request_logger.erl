%% -*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
%% @author Matthew Peck <matthew@opscode.com>
%% @copyright 2013 Opscode Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%
%% @doc Per-Request Logger for Webmachine
%%
%% Webmachine Loggers are configured with a list which is passed to init/1
%% For our logger this argument is a proplist. It can be configured in sys.config as
%% follows:
%%
%% {webmachine, [
%%   {log_handlers, [
%%     {oc_wm_request_logger, [
%%                                 {file, "/tmp/requests.log"},
%%                                 {file_size, 100},  %% Size in MB
%%                                 {files, 5}
%%                                ]
%%     }]
%%   } ]
%% }
%%
%% Available configuration keys:
%%
%% file        - Base file name to log to
%% file_size   - Maximum size of log files in rotation, in MB
%% files       - Number of log files in rotation
%% annotations - (optional) Values to pull out of the Notes section. 
%%
%%               Example:
%%
%%                 [{req_id, <<"req-id">>]
%%
%%               will append
%%
%%                 req-id=something;
%%
%%               to each log line.


-module(oc_wm_request_logger).

-behaviour(gen_event).

%% gen_event API Functions
-export([init/1,
         handle_call/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include_lib("kernel/src/disk_log.hrl").
-include_lib("webmachine/include/webmachine_logger.hrl").

-define(DEFAULT_MAX_FILE_SIZE, 100). %% in MB
-define(DEFAULT_NUM_FILES, 3).
-define(DEFAULT_ANNOTATIONS, []). %% By default, add nothing extra to the log output

-ifdef(TEST).
-compile(export_all).
-endif.

-type log_annotation() :: {atom(), binary()}.

-record(state, {
                log_handle :: #continuation{},
                annotations :: [log_annotation()]
               }).

%% gen_server API Functions
-spec init([{string(), any()}]) -> {ok, #state{}}.
%% Initialize the log handler
init(LogConfig) ->
    FileName = proplists:get_value(file, LogConfig),
    FileSize = proplists:get_value(file_size, LogConfig, ?DEFAULT_MAX_FILE_SIZE),
    FileCount = proplists:get_value(files, LogConfig, ?DEFAULT_NUM_FILES),
    Annotations = proplists:get_value(annotations, LogConfig, ?DEFAULT_ANNOTATIONS),
    {ok, LogHandle} = oc_wm_request_writer:open("request_log", FileName, FileCount, FileSize),
    {ok, #state{log_handle = LogHandle, annotations = Annotations}}.

handle_call(_Msg, State) ->
    {ok, noreply, State}.

handle_event({log_access, LogData},
             #state{log_handle = LogHandle, annotations = Annotations} = State) ->
    Msg = generate_msg(LogData, Annotations),
    %% TODO - Should I really be checking return value here and crash on fast_log error ?
    ok = oc_wm_request_writer:write(LogHandle, Msg),
    {ok, State}.

handle_info(_Msg, State) ->
    {ok, noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions
generate_msg(#wm_log_data{response_code = ResponseCode,
                          method = Method,
                          headers = Headers,
                          path = Path,
                          notes = Notes}, AnnotationFields) ->
    User = mochiweb_headers:get_value("x-ops-userid", Headers),
    %% Our list of things to log, manually extracted from our log_data record
    %% This format is suitable for splunk parsing.
    LogList = [
        <<"method=">>, as_io(Method), <<"; ">>,
        <<"path=">>, as_io(Path), <<"; ">>,
        <<"status=">>, as_io(ResponseCode), <<"; ">>,
        <<"user=">>, as_io(User), <<"; ">>],

    %% Extract annotations logging from notes
    Annotations = message_annotations(AnnotationFields, Notes),

    %% Extract the rest from perf_stats in notes.
    PerfList = case note(perf_stats, Notes) of
                   undefined -> [];
                   PerfStats ->
                       [[Key, <<"=">>, as_io(Value), <<"; ">>] ||
                           {Key, Value} <- PerfStats]
                end,
    [LogList, Annotations, PerfList].

%% @doc Helper function to format extra information from log notes
%% This will take a list of annotation fields, pull it from notes,
%% then formatted into an iolist. The output format is 'key=val; '
%%
%% Example:
%%
%%   [{req_id, <<"req-id">>]
%%
%% will append
%%
%%   req-id=something;
%%
%% to each log line.
%%
message_annotations([], _) ->
    [];
message_annotations(Annotations, Notes) ->
    message_annotations(Annotations, Notes, []).
message_annotations([{Key, Header} | Rest], Notes, A) ->
    message_annotations(Rest, Notes, [[Header, <<"=">>, as_io(note(Key, Notes)), <<"; ">>] | A]);
message_annotations([], _, A) ->
    A.

%% @doc Utility method for extracting a value from a Webmachine
%% request's notes... just to cut down on the verbosity a bit.
%%
%% We have to handle the case where Notes is undefined.  Requests that
%% make it through to finish_request/2 will have a notes
%% list (we stuff information we carry around in our #base_state{}
%% record into the notes there so they are available in this logger).
%% However, a request to a path that does not match the dispatch rules
%% (e.g., '/foo/bar/baz') will not flow through finish_request/2,
%% since there is no resource module to handle it. Webmachine does not
%% set a request's notes to the empty list by default, so it will be
%% 'undefined' in this case.  Enough 404s like this in rapid
%% succession would cause the whole system to restart (via a cascade
%% of supervisor restarts), which could be used for a DOS attack.
note(Key, Notes) when is_list(Notes) ->
    proplists:get_value(Key, Notes);
note(_Key, undefined) ->
    undefined.

%% Cargo-culted from fast_log.erl
%% Convert input to iolist. Handles atoms, lists, binaries, integers, floats, and tuples of
%% the form `{Fmt, Args}' suitable for passing to `io_lib:format/2'. Tuples marked as `{raw,
%% term()}' will be formated using `"~256P"'.
%% This has no dependancies and could be extracted
as_io(X) when is_atom(X) ->
    erlang:atom_to_binary(X, utf8);
as_io(X) when X =:= ""; X =:= <<"">> ->
    <<"empty_string">>;
as_io(X) when is_binary(X); is_list(X) ->
    X;
as_io(X) when is_integer(X) ->
    integer_to_list(X);
as_io(X) when is_float(X) ->
    io_lib:format("~f", [X]);
as_io(X) when is_pid(X) orelse is_reference(X) ->
    io_lib:format("~p", [X]);
as_io({raw, X}) ->
    %% this is last-ditch effort, but may give acceptable results.
    io_lib:format("~256P", [X, 100]);
as_io({Fmt, Args}) ->
    io_lib:format(Fmt, Args).

