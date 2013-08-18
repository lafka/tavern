# What's this?

Tavern is a RESTfull toolkit wrapped around Cowboy. It's main goal is
to provide clean interfaces for writing HTTP APIs

Features:
+ Consistent mapping from HTTP verbs -> Erlang fun's
+ Transparent handling of input/output data

Travis-CI build status: [![Build Status](https://secure.travis-ci.org/lafka/tavern.png)](http://travis-ci.org/lafka/tavern)

# Handler API example
To start of create a new handler. Each module acts as a
static interface that Tavern invokes.

```erlang
-module(resource_handler).

-export([handle_put/2, handle_get/2]).

handle_put(Req, Body) ->
	%% The return signature is {httpheader(), #http_req{}, #tavern{}, tree()}
	%% httpheader() is either an atom with the status text, or integer representing
	%% the status code.
	{<<"Not Implemented">>, {result, [{message, 'not implemend'}]}, Req}.

handle_get(Req, Body) ->
	{<<"OK">>, [{result, [{message, <<"Look im binary!!!">>}]}], Req}.
```

# Overrideable methods

__more to come__

#The tavern application#

##Error codes

<table width="100%" border="0" summary="list of error codes">
	<tr>
		<td>1000</td>
		<td>An unknown error, most likely in the tavern code</td>
	</tr>
	<tr>
		<td>1001</td>
		<td>An empty response was encountered, this should not happend without a ```Empty Reponse``` header</td>
	</tr>
	<tr>
		<td>1002</td>
		<td>No request handlers assigned for the given request method</td>
	</tr>
	<tr>
		<td>1003</td>
		<td>An error occured with the conversion of content to/from the internal format</td>
	</tr>
</table>

