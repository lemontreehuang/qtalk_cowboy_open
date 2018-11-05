-module(http_get_host_alive).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

-record(route, {domain, server_host, pid, local_hint}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} =  cowboy_req:host(Req),
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
	Ret =
		case rfc4627:decode(Body) of
		{ok,Args,[]}  ->
			rpc_get_hosts_info(Args);
		_ ->
			http_utils:gen_result(false, 1, <<"Josn parse error">>)
		end,
	cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], Ret, Req);
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

rpc_get_hosts_info(Args) ->
    case ejb_cache:get_random_node() of
    N when is_atom(N) ->
		case catch rpc:call(N,ets,tab2list,['route']) of
		{badrpc,Reason} ->
			?DEBUG("Reason ~p ~n",[Reason]),
			http_utils:gen_result(false, 2, <<"Http Node Rpc error">>);
		L when is_list(L) ->
			L1 = lists:map(fun(R) ->
				{R#route.domain,R#route.server_host} end,L),
			Ret  = check_nodes_alive(Args,L1),
			http_utils:gen_result(true,0,{obj,Ret});
		_ ->
			http_utils:gen_result(false, 2, <<"Http Node unknown error">>)
		end;
	_ ->
		http_utils:gen_result(false, 2, <<"Http Node down">>)
	end.

check_nodes_alive(Args,L) ->
	lists:flatmap(fun(I) ->
		case proplists:get_value(I , L)	 of
		I ->
			[{I , 1}];
		_ ->
			[{I , 0}]
		end end,Args).
		
				
