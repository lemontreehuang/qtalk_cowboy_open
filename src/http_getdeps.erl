%% Feel free to use, reuse and abuse the code in this file.

-module(http_getdeps).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("ejb_http_server.hrl").
-include("logger.hrl").
-include("http_req.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	{Method, _} = cowboy_req:method(Req),
	{ok, Req2} = get_echo(Method,Req),
	{ok, cowboy_req:compact(Req2), State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
        Req_compress = Req#http_req{resp_compress = true},
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Domain,_} = cowboy_req:qs_val(<<"d">>, Req),
	Rslt = 
		case http_utils:verify_user_key(Req)  of
		true ->
			get_departments(Domain,ejb_public:remvote_domain_user(User));
		_ ->
			[]
		end,
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Rslt, Req_compress);
get_echo(<<"Get">>,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_departments(undefined,User) ->
	Domain = ejb_cache:get_host(),
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select host_id from host_users where host_id  in (select id from host_info where host = '">>,Domain,<<"')  and user_id = '">>,User,<<"'">>]) of
	{selected,_,[[ID]]}  ->
		case ets:lookup(cache_info,str:concat(ID,<<"_json">>)) of
		[Json_tree_depts] when is_record(Json_tree_depts,cache_info) ->
			Json_tree_depts#cache_info.cache;
		_ ->
			[]
		end;	
	_ ->
		[]
	end;
get_departments(Domain,User) ->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
		 [<<"select host_id from host_users where host_id  in (select id from host_info where host = '">>,Domain,<<"')  and user_id = '">>,User,<<"'">>]) of
	{selected,_,[[ID]]}  ->
		case ets:lookup(cache_info,str:concat(ID,<<"_json">>)) of
		[Json_tree_depts] when is_record(Json_tree_depts,cache_info) ->
			Json_tree_depts#cache_info.cache;
		_ ->
			[]
		end;
	_ ->
		[]
	end.	
