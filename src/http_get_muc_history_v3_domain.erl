-module(http_get_muc_history_v3_domain).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("logger.hrl").
-include("http_req.hrl").

-record(domain_info,{max_time = 20000000000 ,users = [],args = []}).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    catch ejb_monitor:monitor_count(<<"domain_http_get_muc_history">>,1),
    do_handle(Req, State).

do_handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of 
	<<"GET">> ->
	    {Host,_} = cowboy_req:host(Req),
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
	 Req_compress = Req#http_req{resp_compress = true},
    {ok, PBody, _} = cowboy_req:body(Req),
    Header = cowboy_req:get(headers,Req),
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	Uv = case User of 
		undefined ->
			<<"null">>;
		_ ->
			User
		end,
    Body =
		case catch proplists:get_value(<<"content-encoding">>,Header) of 
		<<"gzip">> ->
			zlib:gunzip(PBody);
		_ ->
		    PBody
		end,    
	case rfc4627:decode(Body) of
	{ok,Json,[]}  ->
		Rslt = 
%			case http_utils:verify_user_key(Req) of
            case true of
			true ->
				http_utils:gen_result(true, 0,<<"Sucesss">>,http_get_muc_history(Json,pg_odbc:escape(Uv)));
			_ ->
				http_utils:gen_result(false, 1, <<"Interface IP limit">>,<<"">>)
			end,
		cowboy_req:reply(200, [	{<<"content-type">>, <<"text/json; charset=utf-8">>}], Rslt, Req_compress);
	_ ->
		cowboy_req:reply(200, [ {<<"content-type">>, <<"text/json; charset=utf-8">>}], <<"Josn parse error">>, Req_compress)
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

get_domain_info([],Res) ->
	Res;
get_domain_info([{obj,Args}|Left],Res) ->
	Muc = pg_odbc:escape(proplists:get_value("M",Args,<<"null">>)),
	Domain  = proplists:get_value("D",Args),

	Ls = case ejb_public:checek_domain(Domain) of
		true ->
			Infos = proplists:get_value(Domain,Res,#domain_info{}),
			T =  to_integer(proplists:get_value("T",Args,<<"0">>)),
			Max_time = 
				case Infos#domain_info.max_time < T of
				true ->
					Infos#domain_info.max_time;
				_ ->
					T
				end,
			Users = 
				case Muc of
				<<"">> ->
					Infos#domain_info.users;	
				_ ->
					[Muc] ++ Infos#domain_info.users 
				end,
			LR = lists:keydelete(Domain, 1, Res),
			[{Domain,#domain_info{users = Users,max_time = Max_time}}] ++ LR;
		_ ->
			Infos = proplists:get_value(Domain,Res,#domain_info{}),
			NArgs = [{obj,Args}] ++ Infos#domain_info.args,
			LR = lists:keydelete(Domain, 1, Res),
			[{Domain,#domain_info{args  = NArgs}}] ++ LR
		end,
	get_domain_info(Left,Ls);
get_domain_info([_|Left],Res) ->
	get_domain_info(Left,Res).

get_muc_min_time(User,Host) ->
	case catch  pg_odbc:sql_query(<<"ejb_http_server">>,
            [<<"select min(date) from muc_room_users where username = '">>,User,<<"' and host = '">>,Host,<<"' and date > 0;">>]) of 
	{selected, _ , [[T]]} when T =/= null ->
            T;
	 _ ->
	    <<"0">>
	end.

handle_time(User,Host,Args) ->
    T = proplists:get_value("T",Args),
    case T of
    <<"-1">> ->
        get_muc_min_time(User,Host);
    _->
        T
    end. 
	
http_get_muc_history(Json,User)->
	{obj,Args } = Json,
        Domain = proplists:get_value("D",Args),
        Dnoc = case str:str(Domain,<<"conference.">>) of
                0 ->
                        Domain;
                Ni when is_integer(Ni) ->
                        str:substr(Domain,12,size(Domain)-11)
                end,
        Host1 = proplists:get_value("H",Args,Dnoc),
        Num = proplists:get_value("N",Args,<<"35">>),
        Time1 = proplists:get_value("T",Args),
%        Time1 = handle_time(User,Host1,Args),
        case proplists:get_value("U",Args) of 
    	User ->
	    	Loacl_Res = 
                case Time1 of
                -1  ->
                    ?DEBUG("Muc ~p ~n",[Args]),
    		        R = get_user_muc_and_last(User,Domain,Time1),
                    lists:flatmap(fun({Muc,D}) ->
                             case catch ejb_odbc_query:get_muc_msg_info4(Muc,D,<<"1">>) of
                             {selected, _, [[Name,Nick,Host,Packet]]} ->
                                Msgs = [{obj,[{"M",Name},{"N",Nick},{"D",Host},{"B",Packet}]}],
                                [{obj,[{"Domain",<<"ejabhost1">>},{"ID",Name},{"Msg",Msgs},{"Time",<<"0">>}]}] ;
                             _ ->
                                []
                              end end,R);
                _ ->
	                T =  ejb_public:handle_max_time(Time1,<<"1">>),
    				case catch ejb_odbc_query:get_muc_history_v3(User,T,Num) of
	    				{selected, _, SRes}  when is_list(SRes)  ->
                            Msgs = lists:foldr(fun([Name,Nick,Host,Packet],Acc) ->
                                case lists:keyfind(Name,1,Acc) of
                                false ->
                                        Acc ++ [{Name,[{obj,[{"M",Name},{"N",Nick},{"D",Host},{"B",Packet}]}]}];
                                {N,K}  ->
                                        NewL = {N,[{obj,[{"M",Name},{"N",Nick},{"D",Host},{"B",Packet}]}] ++ K},
                                     lists:keyreplace(Name,1,Acc,NewL)
                             end end,[],SRes),
                            lists:flatmap(fun({M,Msgs}) ->
                                    [{obj,[{"Domain",<<"ejabhost1">>},{"ID",M},{"Msg",Msgs},{"Time",<<"0">>}]}] end,Msgs);
		    			Error ->
			    			?DEBUG("ERror ~p ~n",[Error]),
				    		[]
					    end
                end,
	    Loacl_Res;
	_ ->
		[]
	end.	

get_user_muc_and_last(User,Domain,Time) ->
	case catch  pg_odbc:sql_query(<<"ejb_http_server">>,
		[<<"select muc_name,date from muc_room_users  where username = '">>,User,<<"' ;">>]) of
	{selected, _ , Res}  when is_list(Res) ->
		    lists:map(fun([M,D]) ->
                {M,D} end,Res);
	 _ ->
	    []
	end.

get_muc_time(M,L) ->
        case lists:keyfind(M,1,L) of
        false ->
                <<"0">>;
        {_,T} ->
                T
        end.

to_integer(V) when is_binary(V) ->
	binary_to_integer(V);
to_integer(V) when is_integer(V) ->
	V;
to_integer(_) ->
	0.
