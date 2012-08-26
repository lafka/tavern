-record(tavern, {
	module                           :: module(),
	handlers      = []               :: [{tavern_http:request_method(), atom()}] | [],
	methods       = []               :: [tavern_http:request_method()] | [],
	provides      = []               :: [tavern_http:mime()] | [],
	consumes      = []               :: [tavern_http:mime()] | [],
	charset       = <<"utf8">>       :: binary(),
	accept        = {<<$*>>, <<$*>>} :: tavern_http:mime(),
	content_type                     :: tavern_http:mime(),
	status        = 'Bad Request'    :: atom(),
	body          = []               :: tavern_http:tree()
}).

-define(DEFAULT_CHARSET,      <<"utf8">>).
