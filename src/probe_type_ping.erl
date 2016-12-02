-module(probe_type_ping).

-export([specific_touch/1, normalize_pstate_attrs/1, normalize_state_attrs/1 ]).

-record(result, { timestamp, got } ).

-define(DEF_TIMEOUT, 2000 ).

normalize_pstate_attrs( Attrs ) ->   % called before saving pstate
	case lists:keyfind( timeout, 1, Attrs ) of
		{ timeout, _StoredTimeout } ->
			NewAttrs = Attrs;
		false ->
			NewAttrs = Attrs ++ [ { timeout, ?DEF_TIMEOUT } ]
	end,
	{ ok, NewAttrs }.

normalize_state_attrs( Attrs ) ->    % called before at initialization of probe_worker, adds runtime attributes
	AddAttrs = [ { echo , 0 }, { replies, 0 } ],
	{ ok,  Attrs ++ AddAttrs }.


specific_touch( Attrs ) ->
        { fqdn_ip, Host } = lists:keyfind( fqdn_ip, 1, Attrs ),
        { timeout, Timeout } = lists:keyfind( timeout, 1, Attrs ),
        { echo, Echo } = lists:keyfind( echo, 1, Attrs ),
        { replies, Replies } = lists:keyfind( replies, 1, Attrs ),
        NewEcho = Echo + 1, % TODO
%	Replies = 99,  % TODO
        Seq = NewEcho rem 65536,
        UTCNow = os:timestamp(),
        PingRes =
         try gen_icmp:ping([Host], [ { timeout, Timeout}, { sequence, Seq } ]) of
           [PingOut] -> PingOut
         catch
               Exception:Reason ->
                       { error, exception, {Exception, Reason} }
         end,

        NewReplies =
         case PingRes of
               { ok, _, _,  _, {_, _, _, Delay}, _} ->
                       Reply = { ok, { { sequence , Seq }, {delay, Delay} } },
                       Replies + 1;
               { error, timeout, _,  _ }  ->
                       Reply = { error, timeout },
                       Replies;
               { error, unreach_net, _,  _, _, _, _} ->
                       Reply = { error, unreach_net },
                       Replies;
               { error, exception, Exc } ->
                       Reply = { error, exception, Exc },
                       Replies;
               Other ->
                       Reply = { error, unknown, Other },
                       Replies
         end,
        Result = #result{ timestamp = UTCNow, got = Reply },
%        timer:sleep(12000),
	NewAttrs1 = lists:keydelete( echo, 1, Attrs),
	NewAttrs2 = lists:keydelete( replies, 1, NewAttrs1),
	NewAttrs = NewAttrs2 ++ [ { echo, NewEcho }, { replies, NewReplies } ],	
	{ ok, NewAttrs, Result }.
	
