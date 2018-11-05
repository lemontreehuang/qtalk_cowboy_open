-module(http_getusers).

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
	catch ejb_monitor:monitor_count(<<"http_get_users">>,1),
	case Method of 
	<<"GET">> ->
	{ok, Req2} = get_echo(Method,Req),
	{ok, Req2, State};
   	<<"POST">> ->
   	HasBody = cowboy_req:has_body(Req),
   	{ok, Req2} = post_echo(Method, HasBody, Req),
	{ok, Req2, State};
	_ ->
	{ok,Req2} = echo(undefined, Req),
	{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,Req) ->
	Req_compress = Req#http_req{resp_compress = true},
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	{Domain,_} = cowboy_req:qs_val(<<"d">>, Req,ejb_cache:get_host()),
	Res = 
		case http_utils:verify_user_key(Req) of
		true ->
			get_department(Domain,ejb_public:remvote_domain_user(User));
		_ ->
		%	http_utils:gen_result(false, 1, <<"Tkey check error">>,<<"">>)
			[]
		end,
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req_compress);
get_echo(<<"Get">>,Req) ->
	cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing parameter.">>, Req);
echo(Echo, Req) ->
    cowboy_req:reply(200, [
			        {<<"content-type">>, <<"text/plain; charset=utf-8">>}
	    			    ], Echo, Req).

post_echo(<<"POST">>, true, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.

get_department(undefined,User) ->
	Domain = ejb_cache:get_host(),
        case catch pg_odbc:sql_query(<<"ejb_http_server">>,
%                        [<<"select host_id from host_users where user_id = '">>,User,<<"'">>]) of
		[<<"select host_id from host_users where host_id  in (select id from host_info where host = '">>,Domain,<<"')  and user_id = '">>,User,<<"'">>]) of
        {selected,_,[[ID]]}  ->
                case ets:lookup(cache_info,str:concat(ID,<<"_dep">>)) of
                [Json_tree_depts] when is_record(Json_tree_depts,cache_info) ->
                        Json_tree_depts#cache_info.cache;
                _ ->
                        []
                end;
        _ ->
                []
        end;
get_department(Domain,User) ->
        case catch pg_odbc:sql_query(<<"ejb_http_server">>,
                 [<<"select host_id from host_users where host_id  in (select id from host_info where host = '">>,Domain,<<"')  and user_id = '">>,User,<<"'">>]) of
        {selected,_,[[ID]]}  ->
		case ets:lookup(cache_info,str:concat(ID,<<"_dep">>)) of
		[Json_tree_depts] when is_record(Json_tree_depts,cache_info) ->
			Json_tree_depts#cache_info.cache;
		_ ->
			[]
		end;
	_ ->
		[]
	end.
