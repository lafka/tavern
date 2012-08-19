-type key()            :: atom().
-type value()          :: binary() | tree().
-type tree()           :: [{key(), value()}].
-type tree(Root)       :: [{Root, [{key(), value()}]}].
-type timestamp()      :: non_neg_integer().
-type mime()           :: {binary(), binary(), []}.
-type request_method() :: atom().
-type mime_charset()   :: {binary(), binary(), binary()}.
-type mime_options()   :: {binary(), binary(), [{binary(), binary()}]}.

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
