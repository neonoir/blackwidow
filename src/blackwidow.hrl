-record(bw_response, {
	url="" :: string(), 
	status=200 :: integer(), 
	headers=[] :: [{string(), string()}], 
	body="" :: string()
	}).

-record(bw_request, {
	  url="" :: string(),
	  method = get :: atom(),
	  headers = [] :: list(),
	  payload = <<>> :: bitstring(),
	  options = [] :: list()
	 }).
