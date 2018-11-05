%% Feel free to use, reuse and abuse the code in this file.

-module(http_get_history).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"http_get_history">>,1),
    do_handle(Req, State).

do_handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_ } = cowboy_req:host(Req),
		{ok, Req1} = get_echo(Method,Host,Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req1} = post_echo(Method, HasBody, Req),
		{ok, Req1, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args},[]} -> 
		?DEBUG("Get_history Args ~p ~n",[Args]),
		case http_utils:verify_user_key(Req)  of
		true ->
			{User,_} = cowboy_req:qs_val(<<"u">>, Req),
        		Ret = http_get_history( ejb_public:remvote_domain_user(User),Args),
			Rslt = http_utils:gen_result(true,0,<<"">>,Ret),
			cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
		_ ->
			Rslt = http_utils:gen_result(false, 1, <<"Tkey  check error">>,<<"">>),
			cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req)
		end;
	_ ->
		cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req)
	end;
post_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing Post body.">>, Req);
post_echo(_, _, Req) ->
	cowboy_req:reply(405, Req).
										

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/json; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

http_get_history(Req_User,Args)->
	User = pg_odbc:escape(case proplists:get_value("User",Args) of
		   	undefined ->
				<<"null">>;
		   	U ->
				U
			end),
	Host = proplists:get_value("Host",Args),
	Num = proplists:get_value("Num",Args,<<"2500">>),
	Time = case catch User of 
        <<"jinhua.zhang">> ->
            ejb_public:format_time(mod_time:get_timestamp() - 86400*3);
        _ ->
            ejb_public:format_time(proplists:get_value("Time",Args))
        end,
	case User == Req_User of
	true ->	
		case catch ejb_odbc_query:get_msg_info6(User,Host,Time,<<"asc">>,Num) of
		{selected,[<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>],SRes} 
		when is_list(SRes) ->
			Res1 = lists:map(fun([Lfrom,FHost,Lto,THost,Lbody,<<"0">>]) ->
				{obj,[{"F",Lfrom},{"FH",FHost},{"T",Lto},{"TH",THost},{"B",Lbody},{"R",<<"0">>}]};
			                    ([Lfrom,FHost,Lto,THost,Lbody,_]) ->
				{obj,[{"F",Lfrom},{"FH",FHost},{"T",Lto},{"TH",THost},{"B",Lbody},{"R",<<"1">>}]}
				 end ,SRes),
			Res2 = get_warn_history(User,Host,Time,<<"asc">>),
                        Res2 ++ Res1;
		_  ->
			[]
		end;
	_ ->
		[]
	end.

get_warn_history(User,Host,Time,Dir) ->
        case catch ejb_odbc_query:get_warn_history(User,Host,Time,<<"asc">>) of
        {selected,[<<"m_from">>,<<"from_host">>,<<"m_to">>,<<"to_host">>,<<"m_body">>,<<"read_flag">>],SRes}
        when is_list(SRes) ->
                lists:map(fun([Lfrom,FHost,Lto,THost,Lbody,<<"0">>]) ->
                                        {obj,[{"F",Lfrom},{"FH",FHost},{"T",Lto},{"TH",THost},{"B",Lbody},{"R",<<"0">>}]};
                             ([Lfrom,FHost,Lto,THost,Lbody,_]) ->
                                        {obj,[{"F",Lfrom},{"FH",FHost},{"T",Lto},{"TH",THost},{"B",Lbody},{"R",<<"1">>}]}
                                end,SRes);
        _ ->
                []
        end.
