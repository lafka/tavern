-include("types.hrl").

-record(tavern, {
	module              :: module(),
	handlers            :: [{request_method(), module()}],
	methods             :: [{request_method()}],
	provides            :: [{mime_charset()}],
	consumes            :: [{mime_charset()}],
	charset             :: [{binary(), atom()}],
	accept              :: [{binary(), binary()}],
	content_type        :: binary(),
%%	last_modified       :: undefined | {call, function()} | timestamp(),
%%	expires             :: undefined | timestamp(),
	status              :: 'Bad Request' | atom(),
	body                :: undefined | tree()
}).

-define(DEFAULT_CHARSET,      <<"utf8">>).
