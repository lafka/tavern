# Tavern is a RESTfull toolkit wrapped around cowboy

The main goal is to provide easy handling of different content 
types through common a common marshalling structure. There is 
aslo a API that can be hooked onto to control what content-types
to handle along with which HTTP verbs to expose and authorization
for those.

Some parts of it looks alot similar to [Webmachine](http://github.com/basho/webmachine)
and [Cowboy](http://github.com/extend/cowboy). But differs on 2 points:
 1. Providing a consistent namingscheme for HTTP verb methods (_handle_\__{get,post,put,delete}_).
 1. Transparent handling of input data, all data is transformed to/from a internal data struct.

The former point allows Tavern to focus on marshalling payloads and you to focus
on your API's logic.

# Handler API example
To start of just create a `cowboy_http_handler` implementation and include
`tavern/include/rest_module.hrl`. This will provide some request validation,
payload marshalling and automagically map cowboy callbacks to your methods.

```erlang
-module(resource_handler).

-behaviour(cowboy_http_handler).

-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2,
	handle_put/2, handle_get/2]).

-include_lib("tavern/include/rest_module.hrl").

%% Expose these HTTP methods, default will only expose ['HEAD', 'GET]
%% The OPTIONS is handled by handle_default_options/2
allowed_methods(Req, State) ->
	{['GET', 'PUT', 'OPTIONS'], Req, State}.

%% The Accept types, default will allow for xml, json, html and text
content_types_provided(Req, State) ->
	{ [ {<<"application">>, <<"json">>,[]}
	  , {<<"octet">>, <<"stream">>, []}]
	, Req
	, State}.

%% The allowed Content-Types
content_types_accepted(Req, State) -> default will allow for xml, json, html and text
	{ [ {<<"application">>, <<"json">>}
	  , {<<"octet">>, <<"stream">>}]
	, Req
	, State}.

handle_put(Req, #tavern{body = _Body} = State) ->
	%% The return signature is {httpheader(), #http_req{}, #tavern{}, tree()}
	%% httpheader() is either an atom with the status text, or integer representing
	%% the status code.
	{'Not Implemented', Req, State,[{result, [{message, 'not implemend'}]}]}.

handle_get(Req, #tavern{} = State) ->
	%% The entire cowboy is available if needed
	{Resource, Req} = cowboy_http_req:binding(resource, Req),
	
	%% The return data is a tree structure where leafes are string, binary or atom.
	%% There is no support for handling XML attributes or self-closing elements and
	%% the structure is more akine to JSON.
	{'Ok', Req, State,[{result, [{message, <<"Look im binary!!!">>}]}]}.
```

# Overrideable methods
You can override the initial config in the `#tavern{}` record by exposing the
correct methods:

###### allowed_methods/2 -> {[atom()], #http_req{}, #tavern{}}.
The methods that is accepted, if this returns an empty list it will allow all methods.
If request method is not allowed a `405 Method Not Allowed` is returned.

Default is to accept `HEAD`, `GET` and `OPTIONS`. 

###### method_handlers/2 -> {[{atom(), atom()}], #http_req{}, #tavern{}}.
Mapping of HTTP request methods and the function that should handle the
request. Default maps to `handle_<method>` except for `OPTIONS` which 
maps to `handle_default_options`.

###### content_types_provided/2 -> {[tavern_http:mime()], #http_req{}, #tavern{}}.
A list of mime-types that the resource is capable of writing, default is to
provide `application/json`, `application/xml`, `text/html` and `text/plain`..

All items here __MUST__ have a corresponding type in `tavern_marshal_*`.

###### content_types_accepted/2  -> {[tavern_http:mime_charset() | tavern_http:mime()], #http_req{}, #tavern{}}.
Provides with mime-type tuples that can be written to the resource, this is
matched with the `Content-Type` header for the mime & charset.

