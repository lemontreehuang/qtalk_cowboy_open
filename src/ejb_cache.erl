-module(ejb_cache).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("logger.hrl").
-include("ejb_http_server.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([get_host/0,update_user_rbt/3,update_user_vcard/1,get_random_node/0,get_http_other_node/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-record(state,{http_server,ten_minute_timer}).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Args]) ->
    register(ejb_cache,self()),
    Ejabberd_Server = proplists:get_value(ejabberd_server,Args),
    Ejabberd_Opts = ejb_http_server_env:get_env(ejb_http_server,ejabberd_config,	[host,"ejabhost1"]),
    Ejabberd_Node = ejb_http_server_env:get_env(ejb_http_server,ejabberd_node, 		[]),
    Http_other_Node = ejb_http_server_env:get_env(ejb_http_server,http_other_node,  []),
    Host = proplists:get_value(host,Ejabberd_Opts),
    Nodes = lists:flatmap(fun(I) ->
            [list_to_atom(I)] end,Ejabberd_Node),
    Other_nodes  = get_correct_http_other_node(Http_other_Node),
    catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"http_server">>,val = Ejabberd_Server}),
    catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"host">>,val = list_to_binary(Host)}),
    catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"node">>,val = Nodes}),
    catch ets:insert(ejabberd_config,#ejabberd_config{key = <<"http_other_node">>,val = Other_nodes}),
    {ok, #state{http_server = Ejabberd_Server},1000}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_user_vcard,Vcard},State) ->
    spawn(?MODULE, update_user_vcard,[Vcard]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

get_random_node() ->
    Nodes = 
        case catch  ets:lookup(ejabberd_config,<<"node">>) of
        [Ejb_node] when is_record(Ejb_node,ejabberd_config) ->
            Ejb_node#ejabberd_config.val;
        _ ->
            []
        end,
    lists:nth(erlang:phash(os:timestamp(), length(Nodes)), Nodes).

get_http_other_node() ->
    case catch  ets:lookup(ejabberd_config,<<"http_other_node">>) of
    [Ejb_node] when is_record(Ejb_node,ejabberd_config) ->
        Ejb_node#ejabberd_config.val;
    _ ->
        []
    end.
 
get_correct_http_other_node(Http_other_Node) ->
    lists:flatmap(fun(O) ->
                N = list_to_atom(O),
                Node = node(),
                case N of
                Node ->
                    [];
                _ ->
                    [N]
                end  end,Http_other_Node).

handle_info(timeout,#state{http_server = Http_Server } = State) ->
    ejb_update_cache:update_online(Http_Server,false),	
    catch ejb_update_cache:get_monitor_info(Http_Server,false),	
    ejb_update_cache:update_department_info(false),
    ejb_update_cache:update_online_status_cache(false),
    ejb_update_cache:update_vcard_version(false),
    ejb_update_cache:update_muc_vcard_version(false),
    ejb_update_cache:update_robot_info(false),
    ejb_update_cache:update_user_rbt(false),
    ejb_update_cache:update_version_users(false),
    ejb_update_cache:update_domain_to_url(false),
    ejb_update_cache:update_user_profile(false),
    iplimit_util:update_iplimit(),
    iplimit_util:update_ip_limit(),
    Ten_min_timer = erlang:start_timer(600*1000,self(),ten_min),
    {noreply, State#state{ten_minute_timer = Ten_min_timer}};
handle_info({timeout, TimerRef, ten_min}, State=#state{http_server = Server}) ->
    ejb_update_cache:update_online(Server,false),	
    catch ejb_update_cache:get_monitor_info(Server,false),	
    ejb_update_cache:update_online_status_cache(false),
    ejb_update_cache:update_vcard_version(false),
    ejb_update_cache:update_user_profile(false),
    ejb_update_cache:update_muc_vcard_version(false),
    ejb_update_cache:update_robot_info(false),
    ejb_update_cache:update_user_rbt(false),
    iplimit_util:update_iplimit(),
    iplimit_util:update_ip_limit(),
    memory_free:memory_free(200),
    ejb_update_cache:update_department_info(false),
    ejb_update_cache:update_version_users(false),
    case  erlang:read_timer(TimerRef) of
    false ->
        New_tref = erlang:start_timer(600*1000,self(),ten_min),
        NewState = State#state{ten_minute_timer = New_tref},
        {noreply,NewState};
    _ -> {noreply,State}
    end;
handle_info({update_muc_vcard,Muc},State)  ->
    catch ets:insert(muc_vcard,Muc),
    {noreply, State};	
handle_info({update_vcard,Vcard},State)  ->
    catch ets:insert(vcard_version,Vcard),
    {noreply, State};	
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_host() ->
    case catch  ets:lookup(ejabberd_config,<<"host">>) of
        [Http_server] when is_record(Http_server,ejabberd_config) -> Http_server#ejabberd_config.val;
        _ -> <<"ejabhost1">>
    end.

update_user_rbt(User,Rbt,Method) ->
    case Method of
    <<"add">> -> catch ets:insert(user_rbts,#user_rbts{user = User,rbt = Rbt});
    <<"del">> -> catch ets:delete_object(user_rbts,#user_rbts{user = User,rbt = Rbt});
    _  -> ?DEBUG("update user rbt unknown info ~p ~n",[Method])
    end.

update_user_vcard(Vcard) ->
    ets:insert(vcard_version,Vcard).
