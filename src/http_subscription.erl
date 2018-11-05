%% Feel free to use, reuse and abuse the code in this file.

-module(http_subscription).

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
    catch ejb_monitor:monitor_count(<<"http_subscription">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_Req} =  cowboy_req:host(Req),
		 Req1 = cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], 
					http_utils:gen_result(false, <<"-1">>, <<"No Get Method">>,<<"">>) , Req),
		{ok, Req1, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method,HasBody,Req),
		{ok, Req2, State};
	_ ->
		{ok,Req1} = echo(undefined, Req),
		{ok, Req1, State}
	end.
    	
post_echo(<<"POST">>, true, Req) ->
	{ok, Body,_} = cowboy_req:body(Req),
	{Host,_} =  cowboy_req:host(Req),
	{U,_} = cowboy_req:qs_val(<<"u">>, Req),
	Res = 
		case rfc4627:decode(Body) of
		{ok,Json,[]} -> 
			case http_utils:verify_user_key(Req) of
			true ->
				subscription(ejb_public:remvote_domain_user(U),Json);
			false ->
				http_utils:gen_result(false, <<"-1">>, <<"Not found Mac_Key">>,<<"">>)
			end;
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"Json format error">>,<<"">>)
		end,
        cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Res, Req);
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

%%--------------------------------------------------------------------
%% @date 2015-07-10
%% 处理订阅消息
%%--------------------------------------------------------------------
subscription(U,Json) ->
	{obj,Args } = Json ,
	case check_user(U,Args) of
	true ->
		case parse_method(Args) of 
		1 ->
			set_subscription(Args);
		2 ->
			del_subscription(Args);
		3 ->
			get_subscription(Args);
		_ ->
			http_utils:gen_result(false, <<"-1">>, <<"No find Method">>,<<"">>)
		end;
	false ->
		http_utils:gen_result(false, <<"-1">>, <<"User not match u">>,<<"">>)
	end.
		
%%--------------------------------------------------------------------
%% @date 2015-07-10
%% 解析方法
%%--------------------------------------------------------------------

parse_method(Args) ->
	case proplists:get_value("method",Args) of
	<<"add">> ->
		1;
	<<"del">> ->
		2;
	<<"get">> ->
		3;
	_ ->
		0
	end.

check_user(U,Json) ->
%%	User =  proplists:get_value("user",Json),
%%	U =:= User.
	true.

%%--------------------------------------------------------------------
%% @date 2015-07-10
%% 添加用户订阅
%%--------------------------------------------------------------------
set_subscription(Args) ->
	Server = <<"ejb_http_server">>,
	User =  proplists:get_value("user",Args),
	Rbt_name = proplists:get_value("rbt",Args),	
	Uhost =  proplists:get_value("uhost",Args,ejb_cache:get_host()),
	Rhost =  proplists:get_value("rhost",Args,ejb_cache:get_host()),
	case catch ets:lookup(rbt_info,Rbt_name) of
	[] ->
		http_utils:gen_result(false, <<"-1">>,<<"Rbt not exit">>,<<"">>);
	_ ->
		case catch pg_odbc:sql_query(Server,
			[<<"insert into robot_pubsub (rbt_name,rbt_host,user_name,user_host) values ('">>,Rbt_name,<<"','">>,
				Rhost,<<"','">>,User,<<"','">>,Uhost,<<"');">>]) of
		{updated,1} ->
			ets:insert(user_rbts,#user_rbts{user = {User,Uhost},rbt = {Rbt_name,Rhost}}), 
			send_node_notify_msg({User,Uhost},{Rbt_name,Rhost},<<"add">>),
			http_utils:gen_result(true, <<"0">>,<<"Add subscription sucess">>,<<"">>);
		_ ->
			http_utils:gen_result(false, <<"-1">>,<<"Add subscription failed">>,<<"">>)
		end
	end.
	
%%--------------------------------------------------------------------
%% @date 2015-07-10
%% 删除用户订阅
%%--------------------------------------------------------------------
del_subscription(Args) ->
	Server = <<"ejb_http_server">>,
	User =  proplists:get_value("user",Args),
	Rbt_name = proplists:get_value("rbt",Args),	
	Uhost =  proplists:get_value("uhost",Args,ejb_cache:get_host()),
	Rhost =  proplists:get_value("rhost",Args,ejb_cache:get_host()),
	case catch pg_odbc:sql_query(Server,
		[<<"delete from robot_pubsub where rbt_name = '">>,Rbt_name, <<"' and rbt_host = '">>,
					Rhost,<<"' and user_name = '">>,User,<<"' and user_host= '">>,Uhost,<<"';">>]) of
	{updated,1} ->
	 	ets:delete_object(user_rbts,#user_rbts{user = {User,Uhost},rbt = {Rbt_name,Rhost}}), 
		send_node_notify_msg({User,Uhost},{Rbt_name,Rhost},<<"del">>),
		http_utils:gen_result(true, <<"0">>,<<"Del subscription sucess">>,<<"">>);
	_ ->
		http_utils:gen_result(false, <<"-1">>,<<"Del subscription failed">>,<<"">>)
	end.

%%--------------------------------------------------------------------
%% @date 2015-07-10
%% 获取用户所有订阅
%%--------------------------------------------------------------------
get_subscription(Args) ->	
	User =  proplists:get_value("user",Args),
	Uhost =  proplists:get_value("uhost",Args,ejb_cache:get_host()),
	case catch ets:select(user_rbts,[{#user_rbts{user = {User,Uhost},rbt = '$1', _ = '_'},[], ['$1']}]) of
	L when is_list(L) ->
		case proplists:get_value("uhost",Args,<<"null">>) of
		<<"null">> ->
			R = lists:map(fun({User,Uhost}) ->
			%	ejb_public:concat(User,<<"@">>,Uhost) end,L),
				User end,L),
			http_utils:gen_result(true, <<"0">>,<<"">>,R);
		_ ->
			R = lists:map(fun({User,Uhost}) ->
			%T	ejb_public:concat(User,<<"@">>,Uhost) end,L),
				User end,L),
			http_utils:gen_result(true, <<"0">>,<<"">>,R)
		end;
				
	_ ->
		http_utils:gen_result(false, <<"-1">>,<<"Get user robots failed">>,<<"">>)
	end.
	

send_node_notify_msg(User,Rbt,Method) ->
    L = ejb_cache:get_http_other_node(),
    lists:foreach(fun(N) ->
		catch rpc:call(N,ejb_cache,update_user_rbt,[User,Rbt,Method]) end,L).
