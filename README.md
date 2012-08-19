# Tavern is a RESTfull toolkit wrapped around cowboy[1]

The main goal is to provide easy handling of different content 
types through common a common marshalling structure. There is 
aslo a API that can be hooked onto to control what content-types
to handle along with which HTTP verbs to expose and authorization
for those.

# Flow

## Accept header
## Request method
## Authorization
## Content-Type

# Handler API example
```erlang
-module(resource_handler).

-behaviour(cowboy_http_handler).

-export([methods/1, provides/2, consumes/2, handle_put/2, handle_get/2]).

-include_lib("tavern/include/rest_module.hrl").

%% Expose these HTTP methods, default will only expose ['HEAD', 'GET]
%% The OPTIONS is handled by handle_default_options/2
methods(_Req) ->
	['GET', 'PUT', 'OPTIONS'].

%% The Accept types, default will allow for xml, json, html and text
provides(_Req) ->
	[ {<<$*>>, <<"json">>,[]}
	, {<<"octet">>, <<"stream">>, []}].

%% The allowed Content-Types
provides(_Req) -> default will allow for xml, json, html and text
	[ {<<$*>>, <<"json">>,[]}
	, {<<"octet">>, <<"stream">>, []}].

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

[1] http://github.com/extend/cowboy

