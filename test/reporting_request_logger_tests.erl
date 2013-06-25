-module(reporting_request_logger_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine_logger.hrl").
-include_lib("request_logger.hrl").

valid_log_data() ->
  #wm_log_data{response_code = <<"200">>,
               method = 'GET',
               headers = ?SAMPLE_HEADERS,
               path = <<"this/is/the-path">>,
               notes = [{org_name, <<"bobs_org">>},
                        {reqid, <<"request_id">>},
                        {perf_stats, [{<<"perf1">>, 1},
                                      {<<"perf2">>, 2}]
                        }]
              }.

valid_message_format_test_() ->
  [{"generate_msg/1 should return the correct message",
      fun() ->
          ValidMsg = [<<"org_name=">>,<<"bobs_org">>,<<"; ">>,<<"req_id=">>,
            <<"request_id">>,<<"; ">>,<<"status=">>,<<"200">>,<<"; ">>,
            <<"method=">>,<<"GET">>,<<"; ">>,<<"path=">>,
            <<"this/is/the-path">>,<<"; ">>,<<"user=">>,<<"undefined">>,
            <<"; ">>,<<"perf1">>,<<"=">>,49,<<"; ">>,<<"perf2">>,<<"=">>,50,
            <<"; ">>],
          ActualMsg = reporting_request_logger:generate_msg(valid_log_data()),

          ?assertEqual(ValidMsg, ActualMsg)
      end
    },
   %% This is important because we get horrible failures if it doesn't
   {"note/2 should return undefined for nonexistant keys",
      fun() ->
          LogData = valid_log_data(),
          Notes = LogData#wm_log_data.notes,

          ?assertEqual(undefined, reporting_request_logger:note(notreal, Notes))
      end
    }
  ].
