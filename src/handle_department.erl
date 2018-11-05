-module(handle_department).

-export([make_tree_dept/2,get_dept_json/1]).

-include("logger.hrl").
-include("ejb_http_server.hrl").

-record(tree_dept,{name,user_list = [],sub_dept = []}).

make_tree_dept(Info,Flag) ->
        Deps = lists:foldl(fun(R1,Acc)->
                 Res = lists:reverse(R1),
                 [Fp|Tail1] = Res,
                 [D|Tail2] = Tail1,
                 [N|Tail3] = Tail2,
                 [R|Tail4] = Tail3,
                 DepList = lists:reverse(Tail4),
		 DepList1 = 
			case lists:nth(1,DepList) of
			null ->
				lists:delete(lists:nth(1,DepList),DepList);
			_ ->
				DepList
			end,
                 handle_dept_list(DepList1,Acc,ejb_public:handle_column_null(R),
                                ejb_public:handle_column_null(N),ejb_public:handle_column_null(D),ejb_public:handle_column_null(Fp))  end,[],Info),
	
	lists:foreach(fun(D) ->
		catch ets:insert(cache_info,#cache_info{name = D#tree_dept.name,cache = D#tree_dept.sub_dept}) end,Deps),

	case Flag of
	true ->
		catch ets:delete(cache_info,<<"tree_depts">>);
	_ ->
		ok
	end,
        catch ets:insert(cache_info,#cache_info{name = <<"tree_depts">>,cache = Deps}).

get_dept_json(Flag)->
	case catch pg_odbc:sql_query(<<"ejb_http_server">>,
			[<<"select distinct(host_id) from host_users;">>]) of
	{selected,_,L} when is_list(L) ->
		lists:foreach(fun([I]) ->
			Iname = 
			    case I of
			    <<"tree_depts">> ->
				<<"json_tree_depts">>;
			    I ->
				str:concat(I,<<"_json">>)
			    end,
			case ets:lookup(cache_info,I) of
		    	[Tree_dept] when is_record(Tree_dept,cache_info) ->
				Dept_list = Tree_dept#cache_info.cache,
				Rslt = 
					lists:map(fun(T) ->
	        	   		make_dept_json(T,T#tree_dept.sub_dept)   
					end,Dept_list),
        			Body = list_to_binary(rfc4627:encode(Rslt)),
					case Flag of
					true ->
						catch ets:delete(cache_info,Iname);
					_ ->
						ok
					end,
					ets:insert(cache_info,#cache_info{name = Iname,cache = Body}),
				Body;
			_ ->
				[]
		end end,[[<<"tree_depts">>]] ++ L);
	_ ->
		ok
	end.

make_dept_json(P,[]) ->
	UserL = 
	   	lists:map(fun({R,N,_Fp}) -> 
			{obj,[{"U",R},{"N",N},{"S",0}]} 
			end,P#tree_dept.user_list),
	{obj,[{"D",P#tree_dept.name},{"UL",UserL},{"SD",[]}]};
make_dept_json(P,_SubD) ->
	UserL = 
		lists:map(fun({R,N,Fp}) ->
			 {obj,[{"U",R},{"N",N},{"S",0},{"Fp",Fp}]}
		end,P#tree_dept.user_list),
	SubDL = 
		lists:flatmap(fun(T) ->  
				[make_dept_json(T,T#tree_dept.sub_dept)]
			end,P#tree_dept.sub_dept),
	{obj,[{"D",P#tree_dept.name},{"UL",UserL},{"SD",SubDL}]}.
		
handle_dept_list([],Acc,_R,_N,_D,_Fp) ->
	Acc;
handle_dept_list([Head|Tail],Acc,R,N,D,Fp) ->
	DF = Head,
	DN = 
		case Tail of
		[] ->
			[];
		_ ->
			[DN1|_Left] = Tail,
			DN1
		end,
	NewL = 
		lists:filter(fun(T) ->
			case T#tree_dept.name of
			DF ->
				true;
			_ ->
				false
			end
		end,Acc),
	NewAcc = 
		if NewL == [] ->
			[#tree_dept{name = DF} | Acc];
		 true ->
			Acc
	end,
	FAcc = lists:map(fun(T) -> 
			case   T#tree_dept.name of
			DF ->
			     case DN of
			     <<"">> ->
                     NewList = [{R,N,Fp} | T#tree_dept.user_list],
				     Nt = T#tree_dept{user_list = NewList},
				     Nt;
				[] ->
					NewList = [{R,N,Fp} | T#tree_dept.user_list],
					Nt = T#tree_dept{user_list = NewList},
					Nt;
                null ->
					NewList = [{R,N,Fp} | T#tree_dept.user_list],
					Nt = T#tree_dept{user_list = NewList},
					Nt;
				_ ->
				     NewList = handle_dept_list(Tail,T#tree_dept.sub_dept,R,N,D,Fp),
				     Nt = T#tree_dept{sub_dept = NewList},
				     Nt
				end;
			_ ->
			     	T
			end 
		end,
	NewAcc),
	FAcc.



