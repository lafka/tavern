

#Module tavern_http#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

##Data Types##




###<a name="type-http_int_status">http_int_status()</a>##



<pre>http_int_status() = 100..599</pre>



###<a name="type-key">key()</a>##



<pre>key() = atom()</pre>



###<a name="type-mime">mime()</a>##



<pre>mime() = {binary(), binary()}</pre>



###<a name="type-mime_charset">mime_charset()</a>##



<pre>mime_charset() = {binary(), binary(), binary()}</pre>



###<a name="type-mime_options">mime_options()</a>##



<pre>mime_options() = {binary(), binary(), [{binary(), binary()}]}</pre>



###<a name="type-request_method">request_method()</a>##



<pre>request_method() = atom()</pre>



###<a name="type-returnstatus">returnstatus()</a>##



<pre>returnstatus() = <a href="#type-http_int_status">http_int_status()</a> | atom()</pre>



###<a name="type-tree">tree()</a>##



<pre>tree() = [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]</pre>



###<a name="type-value">value()</a>##



<pre>value() = binary() | number() | string() | <a href="#type-tree">tree()</a></pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-3">handle/3</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="handle-3"></a>

###handle/3##


<pre>handle(Handler::module(), Http_req::#http_req{}, Tavern::#tavern{}) -&gt; {ok, #http_req{}, #tavern{}}</pre>
<br></br>


<a name="init-3"></a>

###init/3##


<pre>init(Transport::module(), Req::#http_req{}, X3::[module()]) -&gt; {ok, #http_req{}, #tavern{}}</pre>
<br></br>


<a name="status-1"></a>

###status/1##


<pre>status(X1::atom()) -> <a href="#type-http_int_status">http_int_status()</a></pre>
<br></br>


<a name="terminate-3"></a>

###terminate/3##


<pre>terminate(Handler::module(), Http_req::#http_req{}, Tavern::#tavern{}) -&gt; ok</pre>
<br></br>


