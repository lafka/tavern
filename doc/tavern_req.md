

#Module tavern_req#
* [Function Index](#index)
* [Function Details](#functions)


<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accepts-1">accepts/1</a></td><td></td></tr><tr><td valign="top"><a href="#accepts_charset-1">accepts_charset/1</a></td><td></td></tr><tr><td valign="top"><a href="#accepts_language-1">accepts_language/1</a></td><td></td></tr><tr><td valign="top"><a href="#content_type-1">content_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#content_type_charset-1">content_type_charset/1</a></td><td></td></tr><tr><td valign="top"><a href="#validate_req-2">validate_req/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="accepts-1"></a>

###accepts/1##


<pre>accepts(Http_req::#http_req{}) -&gt; [{binary(), binary(), float()}]</pre>
<br></br>


<a name="accepts_charset-1"></a>

###accepts_charset/1##


<pre>accepts_charset(Http_req::#http_req{}) -&gt; binary()</pre>
<br></br>


<a name="accepts_language-1"></a>

###accepts_language/1##


<pre>accepts_language(Http_req::#http_req{}) -&gt; binary()</pre>
<br></br>


<a name="content_type-1"></a>

###content_type/1##


<pre>content_type(Http_req::#http_req{}) -> <a href="tavern_http.md#type-mime_options">tavern_http:mime_options()</a></pre>
<br></br>


<a name="content_type_charset-1"></a>

###content_type_charset/1##


<pre>content_type_charset(Http_req::#http_req{}) -&gt; binary()</pre>
<br></br>


<a name="validate_req-2"></a>

###validate_req/2##


<pre>validate_req(Req::#http_req{}, State::#tavern{}) -> {true, #http_req{}, #tavern{}} | {{<a href="tavern_http.md#type-return_status">tavern_http:return_status()</a>, <a href="tavern_http.md#type-tree">tavern_http:tree()</a>}, #http_req{}, #tavern{}}</pre>
<br></br>


