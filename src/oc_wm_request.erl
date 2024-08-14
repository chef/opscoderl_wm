%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% Copyright 2013 Opscode, Inc.
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

-module(oc_wm_request).

-export([add_notes/2,
         make_req_id/0,
         read_req_id/2
        ]).

-include_lib("webmachine/include/wm_reqdata.hrl").

%% @doc Helper function to annotate requests for logging
%%
%% This adds additional data to the request. The handler can then be configured to
%% extract annotations and append them to the log line being emitted.
%%
%% The annotation key *must* be an atom.
-spec add_notes([] | [{atom(), term()}], #wm_reqdata{}) -> #wm_reqdata{}.
add_notes([], Req) ->
    Req;
add_notes([{Key, Value} | Rest], Req) ->
    add_notes(Rest, wrq:add_note(Key, Value, Req)).

%% @doc Generate a new random identifier for requests.
-spec make_req_id() -> <<_:192>>. %% 24 bytes
make_req_id() ->
    base64:encode(erlang:md5(term_to_binary(make_ref(), [{minor_version, 1}]))).

%% @doc Helper function to get req_id, usually set upstream. (Usually X-Request-Id)
%% If no req id is set in the header, then one is randomly generated
read_req_id(ReqHeaderName, Req) ->
    case wrq:get_req_header(ReqHeaderName, Req) of
        undefined ->
            make_req_id();
        HV ->
            iolist_to_binary(HV)
    end.
