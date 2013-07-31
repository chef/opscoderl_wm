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
%% @author Ho-Sheng Hsiao <hosh@opscode.com>
-module(oc_wm_request_writer_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("webmachine/include/webmachine_logger.hrl").
-include_lib("sample_requests.hrl").

valid_write_test_() ->
    [{"write/2 should append a timestamped log line to a log file",
        fun() ->
                {A, B, C} = now(),
                Filename = io_lib:format("/tmp/request-log-test-~p-~p-~p", [A, B, C]),
                {ok, LogHandle} = disk_log:open([{name, "temp_log"},
                                                {file, Filename},
                                                {type, halt},
                                                {format, external}
                    ]),
                oc_wm_request_writer:write(LogHandle, "something"),
                disk_log:close(LogHandle),

                {ok, ActualLog} = file:read_file(Filename),
                ?assertMatch({match, _}, re:run(ActualLog, "^[\\d-]+T[\\d:]+Z .* something"))
        end
    }].
