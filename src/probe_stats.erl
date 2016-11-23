-module(probe_stats).

-export([report/1]).

-record(result, { timestamp, got } ).

report( Pid ) when is_pid( Pid ) ->
        { ok, Status, Sent, Rcvd, Results } = probe_worker:get_status( Pid ),
        StrOut1 = io_lib:format("Probe is ~p~n",[Status]),
        StrOut2 = io_lib:format("Overall stats: Sent: ~p Rcvd: ~p~n",[Sent,Rcvd]),
        { ok, StrOut3 } = all_results_to_txt( Results ),
        { ok, StrOut1 ++ StrOut2 ++ StrOut3 } ;

report( Host ) ->
	GetPid = probe_store:get_host_pid( Host ),
	case GetPid of
		{ ok, Pid } ->
			report( Pid );
		Other -> { ok, io_lib:format("Got ~p~n", [Other] ) }
	end.

all_results_to_txt( [] ) ->
        { ok, [] };

all_results_to_txt( [ Result | Results ] ) ->
        { _Host, #result{ timestamp = Time, got = Got }} = Result,
        Left = io_lib:format("Time is ",[]),
        Center = get_timestamp_to_txt( Time ),
        Right = io_lib:format(" result is ~p~n", [Got] ),
        { ok, Rest } = all_results_to_txt( Results ),
        { ok, Left ++ Center ++ Right ++ Rest } .

get_timestamp_to_txt(Timestamp) ->
        {Mega,Secs,Micro} = Timestamp,
        Moufa = {Mega,Secs+3600,Micro}, % Ugly way to show CET summer time :)
        {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(Moufa),
        io_lib:format("~2.10.0B/~2.10.0B/~4B ~2B:~2.10.0B:~2.10.0B.~6.10.0B ",
                         [Day, Month, Year, Hour, Min, Sec, Micro]).

