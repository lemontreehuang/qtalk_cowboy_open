%% Feel free to use, reuse and abuse the code in this file.

-module(http_gc_http_server).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
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
    	
post_echo(<<"POST">>, true, Req) ->
	{ok, Body, _} = cowboy_req:body(Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
		Num = proplists:get_value("num",Args),
		memory_free:memory_free(http_utils:to_integer(Num,20)),	
		Rslt = http_utils:gen_result(true,<<"0">>,<<"gc ok">>,[]),
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Rslt, Req);
	_ ->
		cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}],
				http_utils:gen_result(false, <<"-1">>, <<"Json parse error">>,[]), Req)
	end;
post_echo(_,_,Req) ->
	cowboy_req:reply(405, Req).

get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).
echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

