-record(tavern, {
	module                      :: module(),
	method_handlers = []        :: [{tavern_http:request_method(), atom()}] | [],
	allowed_methods = []        :: [tavern_http:request_method()] | [],
	content_types_provided = [] :: [tavern_http:mime()] | [],
	content_types_accepted = [] :: [tavern_http:mime()] | [],
	charset = <<"utf8">>        :: binary(),
	accept = {<<$*>>, <<$*>>}   :: tavern_http:mime(),
	content_type                :: tavern_http:mime(),
	status = 'Bad Request'      :: atom(),
	body = []                   :: tavern_http:tree(),
	state = undefined           :: any()
}).

-define(DEFAULT_CHARSET,      <<"utf8">>).
