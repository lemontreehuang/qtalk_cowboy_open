%% Feel free to use, reuse and abuse the code in this file.
-module(http_get_notice_msg).

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
	{User,_} = cowboy_req:qs_val(<<"u">>, Req),
	case rfc4627:decode(Body) of
	{ok,{obj,Args} ,[]}  ->
        Rslt = 
            get_notice_msgs(Req,Args,ejb_public:remvote_domain_user(User)),
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

get_notice_msgs(Req,Args,User) ->
	{Platform,_} = cowboy_req:qs_val(<<"p">>, Req),
    Ret = 
	    %case http_utils:verify_user_key(Req) of
        case true of
		true ->
		    Timestamp1 = proplists:get_value("timestamp",Args),	
			Num1 = proplists:get_value("limitnum",Args),	
			Direction = proplists:get_value("direction",Args),	
			Host = proplists:get_value("domain",Args,ejb_cache:get_host()),	
			Time = handle_time(Timestamp1,Direction),	
			Timestamp = ejb_public:format_time(Time),
			Num = handle_num(Num1),
	    	get_chat_msg(Direction,User,Timestamp,Num,Host);
        _ ->
            []
        end,
	http_utils:gen_result(true ,<<"0">>,<<"">>,Ret).

get_chat_msg(<<"1">>,To,Timestamp,Num,Host) ->
	get_msg_info(To,Timestamp,http_utils:to_binary(Num,<<"10">>),Host);
get_chat_msg(_,To,Timestamp,Num,Host) ->
	get_msg_info1(To,Timestamp,http_utils:to_binary(Num,<<"10">>),Host).

get_msg_info(To,Timestamp,Num,Host) ->
	case catch ejb_odbc_query:get_notice_msg(Host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"host">>,<<"m_body">>], SRes}
		when is_list(SRes) ->
				lists:map(fun([Lfrom,H,Lbody]) ->
			%%		{obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
					{obj,[{"F",Lfrom},{"T",To},{"B",Lbody},{"D",H}]}
				end ,SRes);
		A ->
			?DEBUG("A ~p ~n",[A]),
			[]
		end.
							
get_msg_info1(To,Timestamp,Num,Host) ->
    ?DEBUG("get_notice_msg1 ~p ~n",[Timestamp]),
	case catch ejb_odbc_query:get_notice_msg1(Host,Timestamp,Num) of
	{selected,  [<<"m_from">>,<<"host">>,<<"m_body">>], SRes}
		when is_list(SRes) ->
				lists:reverse(lists:map(fun([Lfrom,H,Lbody]) ->
		%%			{obj,[{"F",Lfrom},{"FH",Lfrom_host},{"T",Lto},{"TH",Lto_host},{"B",Lbody},{"R",Flag}]}
					{obj,[{"F",Lfrom},{"T",To},{"B",Lbody},{"D",H}]}
				end ,SRes));
		_ ->
			[]
		end.


get_time() ->
 	{MegaSecs, Secs,MicroSec} = os:timestamp(),
	MegaSecs * 1000000000000 + Secs*1000000 + MicroSec.


handle_time(Time) when is_integer(Time)->
	if Time > 1662768873 ->
		mod_time:get_timestamp() - 3600*24;
	true ->
		Time
	end;
handle_time(_) ->
	mod_time:get_timestamp() - 3600*24.
			
handle_num(Num) when is_binary(Num)->
	INum = binary_to_integer(Num),
	if INum < 10 ->
		<<"10">>;
	true ->
		Num
	end;
handle_num(Num) when is_integer(Num) ->
	if Num < 10 ->
		<<"10">>;
	true ->
		Num
	end;
handle_num(Num) ->
	<<"10">>.
	
handle_time(Time,Direc) ->
        case Direc of
        <<"0">> ->
               if Time =:= 0 orelse Time =:= <<"0">> ->
                        mod_time:get_timestamp();
			                true ->
					                        ITime = http_utils:to_integer(Time),
                   if ITime > 15006744073709500 ->
                             mod_time:get_timestamp();
                      true ->
                            Time
                      end
                end;
       _ ->
              Time
       end.

