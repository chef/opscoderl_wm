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
%% @author Matthew Peck <matthew@opscode.com>
-module(oc_wm_request_logger_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine_logger.hrl").
-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("webmachine/include/wm_reqstate.hrl").
-include_lib("sample_requests.hrl").
-compile([export_all, nowarn_export_all]).

valid_log_data() ->
  #wm_log_data{response_code = 200,
               method = 'GET',
               headers = ?SAMPLE_HEADERS,
               path = <<"this/is/the-path">>,
               notes = [{org, <<"bobs_org">>},
                        {user, <<"bob">>},
                        {req_id, <<"request_id">>},
                        {perf_stats, [{<<"perf1">>, 1},
                                       {<<"perf2">>, 2}]
                        }]
              }.

valid_webmachine_request() ->
    ReqData = #wm_reqdata{method = 'GET', path = "/this/is/the/path", req_headers = mochiweb_headers:make([]),
                          notes = [{msg, {raw, valid}}]},
    {ok, ReqState0} = webmachine_request:set_reqdata(ReqData, #wm_reqstate{}),
    {webmachine_request, ReqState0}.

valid_message_format_test_() ->
  [{"without annotations, generate_msg/1 should return the correct message",
      fun() ->
          ExpectedMsg = iolist_to_binary([<<"method=">>,<<"GET">>,<<"; ">>,
                      <<"path=">>,<<"this/is/the-path">>,<<"; ">>,
                      <<"status=">>,<<"200">>,<<"; ">>]),
          AnnotationFields = [],
          ActualMsg = oc_wm_request_logger:generate_msg(valid_log_data(), AnnotationFields),

          ?assertEqual(ExpectedMsg, iolist_to_binary(ActualMsg))
      end
    },
  {"with simple annotation, generate_msg/1 should return the correct message",
      fun() ->
          ExpectedMsg = iolist_to_binary([<<"method=">>,<<"GET">>,<<"; ">>,
                      <<"path=">>,<<"this/is/the-path">>,<<"; ">>,
                      <<"status=">>,<<"200">>,<<"; ">>,
                      <<"req_id">>,<<"=">>,<<"request_id">>,<<"; ">>]),
          AnnotationFields = [req_id],
          ActualMsg = oc_wm_request_logger:generate_msg(valid_log_data(), AnnotationFields),

          ?assertEqual(ExpectedMsg, iolist_to_binary(ActualMsg))
      end
   },
   {"with proplist annotation, generate_msg/1 should return the correct message",
       fun() ->
           ExpectedMsg = iolist_to_binary([<<"method=">>,<<"GET">>,<<"; ">>,
                       <<"path=">>,<<"this/is/the-path">>,<<"; ">>,
                       <<"status=">>,<<"200">>,<<"; ">>,
                       <<"perf1">>,<<"=">>,<<"1">>,<<"; ">>,
                       <<"perf2">>,<<"=">>,<<"2">>,<<"; ">>
                   ]),
           AnnotationFields = [perf_stats],
           ActualMsg = oc_wm_request_logger:generate_msg(valid_log_data(), AnnotationFields),


           ?assertEqual(ExpectedMsg, iolist_to_binary(ActualMsg))
       end
   },
  {"with invalid annotation key, generate_msg/1 should return the correct message",
      fun() ->
          ExpectedMsg = iolist_to_binary([<<"method=">>,<<"GET">>,<<"; ">>,
                      <<"path=">>,<<"this/is/the-path">>,<<"; ">>,
                      <<"status=">>,<<"200">>,<<"; ">>]),
          AnnotationFields = [invalid_key],
          ActualMsg = oc_wm_request_logger:generate_msg(valid_log_data(), AnnotationFields),

          ?assertEqual(ExpectedMsg, iolist_to_binary(ActualMsg))
      end
   },
   %% This is important because we get horrible failures if it doesn't
   {"note/2 should return undefined for nonexistant keys",
      fun() ->
          LogData = valid_log_data(),
          Notes = LogData#wm_log_data.notes,

          ?assertEqual(undefined, oc_wm_request_logger:note(notreal, Notes))
      end
    },
   {"Handles annotated response codes from webmachine",
    %% wm 1.10.5 introduced customizable response status
    %% messages. This means that the logger receives `{Code, Msg}'
    fun() ->
            LogData0 = valid_log_data(),
            LogData = LogData0#wm_log_data{response_code = {200, undefined}},
            ExpectedMsg = iolist_to_binary([<<"method=">>,<<"GET">>,<<"; ">>,
                                            <<"path=">>,<<"this/is/the-path">>,<<"; ">>,
                                            <<"status=">>,<<"200">>,<<"; ">>]),
            AnnotationFields = [],
            ActualMsg = oc_wm_request_logger:generate_msg(LogData, AnnotationFields),
            ?assertEqual(ExpectedMsg, iolist_to_binary(ActualMsg))
    end
   }
  ].

format_test_() ->
    [
     {"formatter returns empty list when value is undefined",
      ?_assertEqual([], oc_wm_request_logger:format_note(key, undefined))
     },
     {"formatter returns empty list when value is empty string",
      ?_assertEqual([], oc_wm_request_logger:format_note(key, ""))
     },
     {"formatter returns empty list when value is empty list",
      ?_assertEqual([], oc_wm_request_logger:format_note(key, []))
     },
     {"formatter removes empty and undefined values from proplist",
      fun() ->
              PropList = [{key1, undefined},
                          {key2, ""},
                          {key3, "value3"}],
              Result = iolist_to_binary(oc_wm_request_logger:format_note(proplist, PropList)),
              ?assertEqual(<<"key3=value3; ">>, Result)
      end
     }
    ].

integration_test_() ->
    Inputs = [ { "request logger should create a log file and write to disk",  % Description,
                 log_access, [valid_log_data()],  % test function and input
                 "[\\d-]+T[\\d:]+Z .* method=.*; path=.*; status=.*; req_id=.*; perf1=.*; perf2=.*;" % expected log output
               },
               { "request logger should NOT log content for log_error/1 event",
                 log_error, ["why"],
                 ""
               },
               { "request logger should NOT log content for log_error/3 event",
                log_error, [401, valid_webmachine_request(), unauthorized],
                 ""
               },
               { "request logger should NOT log content for log_error/3 event with bogus Req",
                log_error, [401, bogus, unauthorized],
                 ""
               },
               { "request logger should NOT log content for log_info/1 event",
                 log_info, ["why"],
                 ""
               }
             ],
    [ { Description,
        fun() ->
            File = add_wm_log_handler(),
            apply(webmachine_log, LogFunction, LogArgs) ,
            %% webmachine_log:log_access(valid_log_data()),
            webmachine_log:delete_handler(oc_wm_request_logger), % clear cache
            {ok, ActualLog} = file:read_file(File),
            file:delete(File), % clean up at least some of our mess
            ?assertMatch({match, _}, re:run(ActualLog, ExpectedLogLine))
        end
       } || { Description, LogFunction, LogArgs, ExpectedLogLine} <- Inputs ].

add_wm_log_handler() ->
    %% GIVEN a valid log file name
    {A, B, C} = now(),
    LogBasename = io_lib:format("/tmp/opscoderl_wm-test-~p-~p-~p/request.log", [A, B, C]),
    ExpectedLogFilename = io_lib:format("~s.1", [LogBasename]),
    filelib:ensure_dir(LogBasename),
    gen_event:start_link({local, ?EVENT_LOGGER}), %% EVENT_LOGGER is defined in webmachine_logger.hrl
    webmachine_log:add_handler(oc_wm_request_logger, [{file, LogBasename},
                                                  {file_size, 10},
                                                  {files, 1},
                                                  {annotations, [req_id, perf_stats, msg]}]),
    ExpectedLogFilename.

add_wm_log_handler_rotate() ->
    %% Same as add_wm_log_handler/0 but uses rotate mode.
    %% In rotate mode the active file is always the plain basename (no .1 suffix).
    %% Uses a tiny file_size (1 byte effectively, since disk_log rounds up) so
    %% that a rotation can be forced by writing enough entries.
    {A, B, C} = now(),
    LogBasename = io_lib:format("/tmp/opscoderl_wm-test-rotate-~p-~p-~p/request.log", [A, B, C]),
    filelib:ensure_dir(LogBasename),
    gen_event:start_link({local, ?EVENT_LOGGER}),
    webmachine_log:add_handler(oc_wm_request_logger, [{file, LogBasename},
                                                  {file_size, 1},  %% 1 MB — will rotate after enough writes
                                                  {files, 3},
                                                  {log_rotation_type, rotate},
                                                  {annotations, [req_id, perf_stats, msg]}]),
    LogBasename. %% plain basename IS the active file in rotate mode

integration_rotate_test_() ->
    [{"request logger (rotate mode) plain basename is always the active file and rotation produces .0.gz archive",
        fun() ->
            File = add_wm_log_handler_rotate(),
            ArchiveFile = File ++ ".0.gz",
            %% Write enough data to exceed 1 MB and force at least one rotation.
            %% Each log line is ~120 bytes; 12000 writes ~ 1.4 MB.
            lists:foreach(fun(_) ->
                webmachine_log:log_access(valid_log_data())
            end, lists:seq(1, 12000)),
            webmachine_log:delete_handler(oc_wm_request_logger),
            %% The plain basename must exist and contain log entries (it is the active file)
            {ok, ActiveLog} = file:read_file(File),
            ?assertMatch({match, _},
                         re:run(ActiveLog, "[\\d-]+T[\\d:]+Z .* method=.*; path=.*; status=.*")),
            %% A rotated archive must exist — this is the key proof that rotation occurred
            %% and that the plain basename remained the active file throughout
            ?assertEqual(true, filelib:is_regular(ArchiveFile)),
            %% The archive must be non-empty gzip data (magic bytes 1f 8b)
            {ok, ArchiveData} = file:read_file(ArchiveFile),
            <<16#1f, 16#8b, _/binary>> = ArchiveData,
            file:delete(File)
        end
    }].

as_io_test_() ->
    ExactTests = [
                  {an_atom, <<"an_atom">>},
                  {"", <<"empty_string">>},
                  {<<>>, <<"empty_string">>},
                  {"a string", "a string"},
                  {<<"a bin">>, <<"a bin">>},
                  {123, "123"}
                 ],
    FlattenTests = [
                    {1.234, "1.234"},
                    {self(), erlang:pid_to_list(self())},
                    {{raw, [{a, 1}, {b, 2}]}, <<"[{a,1},{b,2}]">>},
                    {{"~B", [42]}, "42"}
                   ],
    [
     [ ?_assertEqual(Expect, oc_wm_request_logger:as_io(In))
       || {In, Expect} <- ExactTests ],

     [ ?_assertEqual(i2b(Expect), i2b(oc_wm_request_logger:as_io(In)))
       || {In, Expect} <- FlattenTests ]
    ].

i2b(X) ->
    erlang:iolist_to_binary(X).
