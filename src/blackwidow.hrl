-record(bw_response, {
	url="" :: string(), 
	status=200 :: integer(), 
	headers=[] :: [{string(), string()}], 
	body="" :: string()
	}).
