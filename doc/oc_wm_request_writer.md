

# Module oc_wm_request_writer #
* [Function Index](#index)
* [Function Details](#functions)

__Authors:__ Kevin Smith ([`kevin@opscode.com`](mailto:kevin@opscode.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#open-4">open/4</a></td><td>Helper function to open a wrap-type disk log.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="open-4"></a>

### open/4 ###


<pre><code>
open(Name::string(), FileName::string(), MaxFiles::pos_integer(), MaxFileSize::pos_integer()) -&gt; {ok, #continuation{}} | {error, any()}
</code></pre>

<br></br>



Helper function to open a wrap-type disk log


Arguments:
Name     - Name of the log, referred internally
FileName - Base filename on disk
MaxFiles - Maximum number of log files in rotation
MaxSize  - Maximum size of each log file in rotation

<a name="write-2"></a>

### write/2 ###


<pre><code>
write(Log::#continuation{}, Output::iolist()) -&gt; ok | {error, term()}
</code></pre>

<br></br>



