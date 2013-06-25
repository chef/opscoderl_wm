%% -*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
%% @author Matthew Peck <matthew@opscode.com>
%% @copyright 2013 Opscode Inc.
%%
%% @doc Request Logger
%% Per request logging for webmachine


-module(reporting_request_logger).

-compile([{parse_transform, lager_transform}]).

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

-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {
                log_handle :: #continuation{}
               }).

%% gen_server API Functions
-spec init([{string(), any()}]) -> {ok, #state{}}.
%% Initialize the log handler
%%
%% Webmachine Loggers are configured with a list which is passed to init/1
%% For our logger this argument is a proplist. It can be configured in sys.config as
%% follows:
%%
%% {webmachine, [
%%   {log_handlers, [
%%     {reporting_request_logger, [
%%                                 {file, "/tmp/reporting.log",
%%                                  file_size, 100,  %% Size in MB
%%                                  files, 5}
%%                                ]
%%     }]
%%   } ]
%% }
%%
init(LogConfig) ->
    FileName = proplists:get_value(file, LogConfig),
    FileSize = proplists:get_value(file_size, LogConfig, 100),
    FileCount = proplists:get_value(files, LogConfig, 3),
    {ok, LogHandle} = reporting_request_writer:open("request_log", FileName, FileCount, FileSize),
    lager:info("Configured Webmachine Request Logger : ~p", [FileName]),
    {ok, #state{log_handle = LogHandle}}.

handle_call(_Msg, State) ->
    {ok, noreply, State}.

handle_event({log_access, LogData},
             #state{log_handle = LogHandle} = State) ->
    Msg = generate_msg(LogData),
    %% TODO - Should I really be checking return value here and crash on fast_log error ?
    ok = reporting_request_writer:write(LogHandle, Msg),
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
                          notes = Notes}) ->
    OrgName = note(org_name, Notes),
    ReqId = note(reqid, Notes),
    User = mochiweb_headers:get_value("x-ops-userid", Headers),
    %% Our list of things to log, manually extracted from our log_data record
    LogList = [<<"org_name=">>, as_io(OrgName), <<"; ">>,
               <<"req_id=">>, as_io(ReqId), <<"; ">>,
               <<"status=">>, as_io(ResponseCode), <<"; ">>,
               <<"method=">>, as_io(Method), <<"; ">>,
               <<"path=">>, as_io(Path), <<"; ">>,
               <<"user=">>, as_io(User), <<"; ">>],
    %% Extract the rest from perf_stats in notes.
    PerfList = case note(perf_stats, Notes) of
                   undefined -> [];
                   PerfStats ->
                       [[Key, <<"=">>, as_io(Value), <<"; ">>] ||
                           {Key, Value} <- PerfStats]
                end,
    lists:flatten([LogList, PerfList]).

%% @doc Utility method for extracting a value from a Webmachine
%% request's notes... just to cut down on the verbosity a bit.
%%
%% We have to handle the case where Notes is undefined.  Requests that
%% make it through to finish_request/2 in reporting will have a notes
%% list (we stuff information we carry around in our #base_state{}
%% record into the notes there so they are available in this logger).
%% However, a request to a path that does not match the dispatch rules
%% (e.g., '/foo/bar/baz') will not flow through finish_request/2,
%% since there is no resource module to handle it. Webmachine does not
%% set a request's notes to the empty list by default, so it will be
%% 'undefined' in this case.  Enough 404s like this in rapid
%% succession would cause the whole system to restart (via a cascade
%% of supervisor restarts), which could be used for a DOS attack.
%% This patch was ported from bifrost.
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

