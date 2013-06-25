%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Kevin Smith <kevin@opscode.com>
%% Copyright 2011-2012 Opscode, Inc. All Rights Reserved.
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


-module(oc_wm_request_writer).

-include_lib("kernel/src/disk_log.hrl").

-export([open/4,
         write/2]).

-type calendar_time() :: { non_neg_integer(),  non_neg_integer(),  non_neg_integer() }.
-type calendar_date() :: { integer(),  1..12, 1..31 }.
-type erlang_time() :: {calendar_date(), calendar_time()}.

-spec(open(string(), string(), pos_integer(), pos_integer()) ->
             {ok, #continuation{}} | {error, any()}).
open(Name, FileName, MaxFiles, MaxFileSize) ->
    disk_log:open([{name, Name},
                   {file, FileName},
                   {size, {MaxFileSize * 1024 * 1024, MaxFiles}},
                   {type, wrap},
                   {format, external}]).

-spec write(Log :: #continuation{},
            Output :: string()) -> ok | {error, term()}.
write(Log, Output) ->
    Timestamp = time_iso8601(),
    Node = atom_to_list(node()),
    Prefix = io_lib:format("~s ~s ", [Timestamp, Node]),
    Msg = iolist_to_binary([Prefix, Output, $\n]),
    disk_log:blog(Log, Msg).

%% @doc Converts Erlang time-tuple to iso8601 formatted date string.
%%
%% Example output looks like <<"2003-12-13T18:30:02Z">>
-spec(time_iso8601() -> string()).
time_iso8601() ->
    time_iso8601(calendar:universal_time()).

-spec(time_iso8601(erlang_time()) -> string()).
time_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    % Is there a way to build a binary straight away?
    Fmt = "~4B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
    lists:flatten(io_lib:format(Fmt, [Year, Month, Day, Hour, Min, Sec])).


