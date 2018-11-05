%% Feel free to use, reuse and abuse the code in this file.

-module(http_getservertime).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_get_server_time">>,1),
	case Method of 
	<<"GET">> ->
	{Host,_} =  cowboy_req:host(Req),
	{ok, Req2} = get_echo(Method,Host,Req),
	{ok, Req2, State};
   	<<"POST">> ->
   	HasBody = cowboy_req:has_body(Req),
   	{ok, Req2} = post_echo(Method, HasBody, Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Host,Req) ->
		Res = 	integer_to_binary(mod_time:get_timestamp()), 
		?DEBUG("Res ~p ~n",[Res]), 
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/plain; charset=utf-8">>}
		], Res, Req);
get_echo(<<"Get">>,_,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).


post_echo(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.


