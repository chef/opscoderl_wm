

# Module oc_wm_request_logger #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Per-Request Logger for Webmachine.
Copyright (c) 2013 Opscode Inc.

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.

__Behaviours:__ [`gen_event`](gen_event.md).

__Authors:__ Matthew Peck ([`matthew@opscode.com`](mailto:matthew@opscode.com)), Ho-Sheng Hsiao ([`hosh@opscode.com`](mailto:hosh@opscode.com)).
<a name="description"></a>

## Description ##



Webmachine Loggers are configured with a list which is passed to init/1
For our logger this argument is a proplist. It can be configured in sys.config as
follows:



{webmachine, [
{log_handlers, [
{oc_wm_request_logger, [
{file, "/tmp/requests.log"},
{file_size, 100},  %% Size in MB
{files, 5}
]
}]
} ]
}



Available configuration keys:



file        - Base file name to log to
file_size   - Maximum size of log files in rotation, in MB
files       - Number of log files in rotation
annotations - (optional) Values to pull out of the Notes section. This is a list of atoms



Example:



[req_id, org]



will append



req_id=something; org=something;


to each log line.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-2">handle_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-2">handle_event/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Extra) -> any()`


<a name="handle_call-2"></a>

### handle_call/2 ###

`handle_call(Msg, State) -> any()`


<a name="handle_event-2"></a>

### handle_event/2 ###

`handle_event(X1, State) -> any()`


<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, State) -> any()`


<a name="init-1"></a>

### init/1 ###


<pre><code>
init(LogConfig::[{string(), any()}]) -&gt; {ok, #state{}}
</code></pre>

<br></br>



<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`


