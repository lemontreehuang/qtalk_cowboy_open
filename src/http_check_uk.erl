%% Feel free to use, reuse and abuse the code in this file.
-module(http_check_uk).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").
-include("ejb_http_server.hrl").

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
    {User,_} = cowboy_req:qs_val(<<"user">>, Req),
    Ret = 
        case catch iplimit_util:check_ips_limit(Req,<<"20">>,User) of
       % case true of
        true ->
            case catch http_utils:verify_user_key(Req)  of
            true ->
                http_utils:gen_result(true, 0,<<"success">>,<<"">>);
            _  ->
               http_utils:gen_result(false, 1,<<"failed">>,<<"">>)
            end;
        _ ->
            http_utils:gen_result(false, 2,<<"IP limit">>,<<"">>)
        end,
	cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], Ret, Req).

post_echo(<<"POST">>, _, Req) ->
    cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"No POST method">>, Req);
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
