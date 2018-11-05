%% Feel free to use, reuse and abuse the code in this file.

-module(http_setvcardinfo).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([send_http_node_update_user_vcard/1]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
    catch ejb_monitor:monitor_count(<<"http_set_vcard_info">>,1),
	case Method of 
	<<"GET">> ->
	    {Host,_ } =  cowboy_req:host(Req),
		{ok, Req2 } = get_echo(Method,Host,Req),
		{ok, Req2, State};
	<<"POST">> ->
		HasBody = cowboy_req:has_body(Req),
		{ok, Req2} = post_echo(Method, HasBody, Req),
		{ok, Req2, State};
	_ ->
		{ok,Req2} = echo(undefined, Req),
		{ok, Req2, State}
	end.
    	
get_echo(<<"GET">>,_,Req) ->
		cowboy_req:reply(200, [
			{<<"content-type">>, <<"text/json; charset=utf-8">>}
		], <<"No GET method">>, Req).

post_echo(<<"POST">>, true, Req) ->
    {ok, Body, _} = cowboy_req:body(Req),
    {Host,_ } =  cowboy_req:host(Req),
	?DEBUG("Req ~p ~n",[Req]),
	case rfc4627:decode(Body) of
	{ok,Json,[]} -> 
		Rslt = 
			case http_utils:verify_user_key(Req)  of
			true ->
	       			set_update_version(Json);	
			_ ->
				http_utils:gen_result(false, 1, <<"Tkey  check error">>,<<"">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req);
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

set_update_version(Json)->
        UserStatus = 
			lists:flatmap(fun({obj,Args}) ->
				User = proplists:get_value("user",Args),
				Url = proplists:get_value("url",Args),
				Domain = proplists:get_value("domain",Args,ejb_cache:get_host()),
				?DEBUG("Domain ~p ~n",[Domain]),
				case catch ejb_odbc_query:update_vcard_version(User,Domain,Url) of
				{updated, 1} ->
					case ejb_odbc_query:get_vcard_version_by_user(User,Domain) of
					{selected,[<<"version">>],[[null]] }  ->
						[{obj,[{"user",User},{"version",<<"-1">>},{"domain",Domain}]}];	
					{selected,[<<"version">>],[[V]] }  ->
                            			update_ets_vcard_version(User,Domain,Url,V),
							[{obj,[{"user",User},{"version",V},{"domain",Domain}]}];
					_ ->
							[{obj,[{"user",User},{"version",<<"-1">>},{"domain",Domain}]}]
					end;
				_ ->
					case catch ejb_odbc_query:insert_vcard_version(User,Domain,<<"2">>,Url) of
					{updated, 1} ->
                            update_ets_vcard_version(User,Domain,Url,<<"2">>),
							[{obj,[{"user",User},{"version",<<"2">>},{"domain",Domain}]}];
					_ ->
							[{obj,[{"user",User},{"version",<<"-1">>},{"domain",Domain}]}]
					end
				end end,Json),
       list_to_binary(rfc4627:encode( {obj,[{"data",UserStatus}]})).

send_http_node_update_user_vcard(Vcard) ->
    L = ejb_cache:get_http_other_node(),
    lists:foreach(fun(N) ->
		    catch gen_server:cast({'ejb_cache',N},{update_user_vcard,Vcard}) end,L).

update_ets_vcard_version(User,Host,Url,Version) ->
    case catch ets:lookup(vcard_version,{User,Host}) of
    [Vv] when is_record(Vv,vcard_version) ->
        ets:insert(vcard_version,Vv#vcard_version{user = {User,Host},url = Url,version = Version}),
	    spawn(?MODULE, send_http_node_update_user_vcard,[Vv#vcard_version{user = {User,Host},url = Url,version = Version}]);
    _ ->
        case ets:lookup(user_name,{User,Host}) of
        [{_,N}] when N =/= undefined  ->
            ets:insert(vcard_version,#vcard_version{user = {User,Host},url = Url,version = Version,name = N,gender = <<"0">> }),
	        spawn(?MODULE, send_http_node_update_user_vcard,
                [#vcard_version{user = {User,Host},url = Url,version = Version,name = N,gender = <<"0">>}]);
        _ ->
            ets:insert(vcard_version,#vcard_version{user = {User,Host},url = Url,version = Version,gender = <<"0">>}),
            spawn(?MODULE, send_http_node_update_user_vcard,
                     [#vcard_version{user = {User,Host},url = Url,version = Version,gender = <<"0">>}])
        end
    end.
   
 
