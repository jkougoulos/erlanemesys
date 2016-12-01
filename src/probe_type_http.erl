-module(probe_type_http).

-export([specific_touch/1]).

-record(result, { timestamp, got } ).

-define(DEF_TIMEOUT, 2000 ).

specific_touch( Attrs ) ->
        { fqdn_ip, Host } = lists:keyfind( fqdn_ip, 1, Attrs ),
        UTCStart = os:timestamp(),
	
	GotBack =
		try httpc:request(get, { "http://" ++ Host ++ "/", []}, [{autoredirect, false}],[]) of
			HttpOut -> HttpOut
		catch
	        	Exception:Reason ->
       		                { error, [ { reason , {Exception, Reason} } ] }
        	end,

	UTCStop = os:timestamp(),
	Delay = timer:now_diff(UTCStop, UTCStart) / 1000,
	case GotBack of
		{ ok, Response } ->
			{ Status, _Headers , _Body } = Response,
			{ _Httpver, StatusCode, ReasonPhrase } = Status,
			Reply = { ok, [ { status , StatusCode }, { delay , Delay }, { reasontxt , ReasonPhrase } ] };
		{ error, Why } ->
			Reply = { error, [ { reason , Why } ] }
	end,
	Result = #result{ timestamp = UTCStart, got = Reply },
	{ ok, Attrs, Result }.
