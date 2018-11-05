%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_msgs_virtual_v2).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([do_get_msg_info/7]).

-include("logger.hrl").
-include("http_req.hrl").
-define(HOST, <<"ejb_http_server">>).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_msgs_domain">>,1),
	case Method of 
	<<"GET">> ->
	{Host,_} =  cowboy_req:host(Req),
	{ok, Req2} = get_echo(Method,Host,Req),
	{ok, Req2, State};
	<<"POST">> ->
    HasBody = cowboy_req:has_body(Req),
	{ok, Req2} = post_echo(Method,HasBody,Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
post_echo(_,_,Req) ->
	cowboy_req:reply(405, Req).

get_echo(<<"GET">>,_,Req) ->
    {From,_} = cowboy_req:qs_val(<<"from">>, Req),
    {To,_} = cowboy_req:qs_val(<<"to">>, Req),
    {D,_} = cowboy_req:qs_val(<<"domain">>, Req),
    {Virtual,_} = cowboy_req:qs_val(<<"virtual">>, Req),
    {U,_} = cowboy_req:qs_val(<<"u">>, Req),
    {Direction,_} = cowboy_req:qs_val(<<"direction">>,Req),
    {Num,_} = cowboy_req:qs_val(<<"limitnum">>,Req),
    {T,_} = cowboy_req:qs_val(<<"timestamp">>, Req),
    Timestamp = ejb_public:format_time(T),

    Host = case D of
        undefined ->
                ejb_cache:get_host();
        _ ->
                D
        end,
    
    Res = case http_utils:verify_user_key(Req)  of
	  true ->
		get_msg_info(From,Host, To, Virtual, Timestamp, Num, Direction, ejb_public:remvote_domain_user(U));
	_ ->
		http_utils:gen_result(false, 1, <<"Tkey  check error">>,<<"">>)
	end,
    cowboy_req:reply(200, [
            {<<"content-type">>, <<"text/plain; charset=utf-8">>}
        ], Res, Req);

get_echo(_,_,Req) ->
    cowboy_req:reply(405, Req).


echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.


get_msg_info(From,D, To, Virtual, _Timestamp, _Num, _Direction, _U) 
    when undefined =:= From; undefined =:= To; undefined =:= Virtual; "" =:= From; "" =:= To; "" =:= Virtual ->
        http_utils:gen_result(true ,0,<<"">>,<<"">>);
get_msg_info(From,D, To, Virtual, Timestamp, Num, Direction, U)
    when U =:= From; U =:= To ->
    Res =   case catch do_get_msg_info(pg_odbc:escape(From),pg_odbc:escape(To),
				pg_odbc:escape(D), pg_odbc:escape(Virtual), pg_odbc:escape(Timestamp),pg_odbc:escape(Num), pg_odbc:escape(Direction)) of
        {selected,  [<<"m_from">>,<<"m_to">>,<<"m_body">>, <<"read_flag">>], SRes} when is_list(SRes) ->
            NewRes = sort_res(SRes, Direction),
            lists:map(fun([Lfrom,Lto,Lbody, RFlag]) ->
                        {obj,[{"F",Lfrom},{"T",Lto},{"B",Lbody}, {"R", get_read_flag(RFlag)}]}
                end ,NewRes);
        Err ->
            ?ERROR("query history error ~p~n", [Err]),
            []
    end,
    http_utils:gen_result(true ,0,<<"">>,Res).


sort_res(Res, <<"1">>) ->
    Res;
sort_res(Res, _) ->
    lists:reverse(Res).


get_read_flag(<<"0">>) ->
    <<"0">>;
get_read_flag(_) ->
    <<"1">>.


do_get_msg_info(From,To,Host, Virtual, Timestamp, Num, <<"1">>) ->
    pg_odbc:sql_query(?HOST,
        [<<"select m_from,m_to,m_body,read_flag from msg_history where create_time > '">>, Timestamp, <<"' and ((m_from = '">>, From,
            <<"' and m_to = '">>, Virtual, <<"' and xpath('/message/@realto', m_body::xml)::text = '{">>, To,
                 <<"@">>,Host,<<"}') or (m_from = '">>, Virtual,<<"' and m_to = '">>, From, 
                 <<"' and xpath('/message/@realfrom', m_body::xml)::text = '{">>, To, 
                    <<"@">>,Host,<<"}')) order by create_time limit ">>, Num,<<";">>]);
do_get_msg_info(From,To, Host, Virtual, Timestamp, Num, <<"0">>) ->
    pg_odbc:sql_query(?HOST,
        [<<"select m_from,m_to,m_body,read_flag from msg_history where create_time < '">>, Timestamp, <<"' and ((m_from = '">>, From,
            <<"' and m_to = '">>, Virtual, <<"' and xpath('/message/@realto', m_body::xml)::text = '{">>, To,
                 <<"@">>,Host,<<"}') or (m_from = '">>, Virtual,<<"' and m_to = '">>, From, 
                 <<"' and xpath('/message/@realfrom', m_body::xml)::text = '{">>, To, 
                    <<"@">>,Host,<<"}')) order by create_time desc limit ">>, Num,<<";">>]).

