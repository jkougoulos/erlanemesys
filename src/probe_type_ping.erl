-module(probe_type_ping).

-export([specific_touch/1]).

-record(result, { timestamp, got } ).

-define(DEF_TIMEOUT, 2000 ).

specific_touch( Attrs ) ->
        { fqdn_ip, Host } = lists:keyfind( fqdn_ip, 1, Attrs ),
        NewEcho = 1000, % TODO
	Replies = 99,  % TODO
        Seq = NewEcho rem 65536,
        UTCNow = os:timestamp(),
        PingRes =
         try gen_icmp:ping([Host], [{ timeout, ?DEF_TIMEOUT},{ sequence, Seq } ]) of
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
	NewAttrs = Attrs,
	{ ok, NewAttrs, Result }.
	
