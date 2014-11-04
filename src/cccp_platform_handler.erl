%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cccp_platform_handler).

-behaviour(gen_listener).

-export([start_link/1]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([process_call_to_platform/1
        ,handle_all/2
        ]).

-include("cccp.hrl").

-record(state, {call = whapps_call:new() :: whapps_call:call()
                ,flow = wh_json:new() :: wh_json:object()
                ,cccp_module_pid :: {pid(), reference()} | 'undefined'
                ,status = <<"sane">> :: ne_binary()
                ,queue :: api_binary()
                ,self = self()
               }).
-type state() :: #state{}.


%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [
                   {'self', []}
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_all'}
                      ,[{<<"*">>, <<"*">>}]
                     }
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Call) ->
    gen_listener:start_link(?MODULE, [
                                      {'bindings', ?BINDINGS}
                                      ,{'responders', ?RESPONDERS}
                                      ,{'queue_name', ?QUEUE_NAME}       % optional to include
                                      ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                                      ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                                     ], [Call]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init([whapps_call:call()]) -> {'ok', state()}.
init([Call]) ->
    process_flag('trap_exit', 'true'),
    CallId = whapps_call:call_id(Call),
    put('callid', CallId),
    self() ! 'initialize',
    {'ok', #state{call=Call}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({'gen_listener',{'created_queue',Queue}}, #state{call=Call}=State) ->
    {'noreply', State#state{call=whapps_call:set_controller_queue(Queue, Call)}};
handle_cast({'gen_listener',{'is_consuming', 'true'}}, #state{call=Call}=State) ->
    CallId = whapps_call:call_id(Call),
    Srv = whapps_call:kvs_fetch('server_pid', Call),
    gen_listener:add_binding(Srv, {'call',[{'callid', CallId}]}),
    gen_listener:add_responder(Srv, {'cccp_util', 'handle_callinfo'}, [{<<"call_event">>, <<"*">>}]),
    gen_listener:add_responder(Srv, {'cccp_util', 'handle_disconnect'}, [{<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>}]),
    process_call_to_platform(Call),
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('initialize', #state{call=Call}=State) ->
    CallUpdate = whapps_call:kvs_store('consumer_pid', self(), Call),
    whapps_call:cache(CallUpdate, ?APP_NAME),
    {'noreply', State#state{call=CallUpdate}};

handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_all(JObj, Props) ->
    lager:debug("handle_all JObj: ~p", [JObj]),
    lager:debug("handle_all Props: ~p", [Props]).

process_call_to_platform(Call) ->
    whapps_call_command:answer(Call),
    CID = wnm_util:normalize_number(whapps_call:caller_id_number(Call)),
    case cccp_util:authorize(CID, <<"cccps/cid_listing">>) of
        [AccountId, AccountCID, ForceCID] ->
            dial(AccountId, AccountCID, ForceCID, Call);
        _ ->
            pin_collect(Call)
    end.

dial(AccountId, AccountCID, ForceCID, Call) ->
    {'num_to_dial', Number} = cccp_util:get_number(Call),
    EP = stepswitch_resources:endpoints(Number, wh_json:new()),
    Call1 = whapps_call:set_account_id(AccountId, Call),
    Call2 = case ForceCID of
        'false' -> Call1;
         _ -> whapps_call:set_caller_id_number(AccountCID, Call1)
    end,
    whapps_call_command:bridge(EP, Call2).

pin_collect(Call) ->
    case whapps_call_command:b_prompt_and_collect_digits(9,12,<<"disa-enter_pin">>,3,Call) of
       {ok,<<>>} ->
           whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
           whapps_call_command:hangup(Call);
       {ok, EnteredPin} ->
           lager:info("Pin entered."),
           case cccp_util:authorize(EnteredPin, <<"cccps/pin_listing">>) of
               [AccountId, AccountCID, ForceCID] ->
                   dial(AccountId, AccountCID, ForceCID, Call);
               _ ->
                   lager:info("Wrong Pin entered."),
                   whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
                   whapps_call_command:hangup(Call)
           end;
       _ ->
           lager:info("No pin entered."),
           whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
           whapps_call_command:hangup(Call)
     end.

