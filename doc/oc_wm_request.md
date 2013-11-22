

# Module oc_wm_request #
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_notes-2">add_notes/2</a></td><td>Helper function to annotate requests for logging.</td></tr><tr><td valign="top"><a href="#make_req_id-0">make_req_id/0</a></td><td>Generate a new random identifier for requests.</td></tr><tr><td valign="top"><a href="#read_req_id-2">read_req_id/2</a></td><td>Helper function to get req_id, usually set upstream.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_notes-2"></a>

### add_notes/2 ###


<pre><code>
add_notes(Rest::[] | [{atom(), term()}], Wm_reqdata::#wm_reqdata{}) -&gt; #wm_reqdata{}
</code></pre>

<br></br>



Helper function to annotate requests for logging



This adds additional data to the request. The handler can then be configured to
extract annotations and append them to the log line being emitted.


The annotation key *must* be an atom.
<a name="make_req_id-0"></a>

### make_req_id/0 ###


<pre><code>
make_req_id() -&gt; &lt;&lt;_:192&gt;&gt;
</code></pre>

<br></br>


Generate a new random identifier for requests.
<a name="read_req_id-2"></a>

### read_req_id/2 ###

`read_req_id(ReqHeaderName, Req) -> any()`

Helper function to get req_id, usually set upstream. (Usually X-Request-Id)
If no req id is set in the header, then one is randomly generated
