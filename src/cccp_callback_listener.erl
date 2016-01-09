%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   OnNet (Kirill Sysoev github.com/onnet)
%%%-------------------------------------------------------------------
-module(cccp_callback_listener).

-behaviour(gen_listener).

-export([start_link/1
         ,handle_resource_response/2
        ]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("cccp.hrl").

-define(BINDINGS, [{'self', []}
                  ]).
-define(RESPONDERS, [{{?MODULE, 'handle_resource_response'},[{<<"*">>, <<"*">>}]}
                    ,{{'cccp_util', 'relay_amqp'}, [{<<"*">>, <<"*">>}]}
                    ]).

-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link([any()]) -> startlink_ret().
start_link(JObj) ->
    gen_listener:start_link(?MODULE, [{'responders', ?RESPONDERS}
                                      ,{'bindings', ?BINDINGS}
                                      ,{'queue_name', ?QUEUE_NAME}
                                      ,{'queue_options', ?QUEUE_OPTIONS}
                                      ,{'consume_options', ?CONSUME_OPTIONS}
                                     ], [JObj]).

-spec init(wh_json:object()) -> {'ok', state()}.
init([JObj]) ->
    CustomerNumber = wh_json:get_value(<<"Number">>, JObj),
    BLegNumber = wh_json:get_value(<<"B-Leg-Number">>, JObj),
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    OutboundCID = wh_json:get_value(<<"Outbound-Caller-ID-Number">>, JObj),
    AuthDocId = wh_json:get_value(<<"Auth-Doc-Id">>, JObj),
    CallbackDelay = wh_json:get_value(<<"Callback-Delay">>, JObj),

    {'ok', #state{customer_number = CustomerNumber
                  ,parked_call_id = 'undefined'
                  ,b_leg_number = BLegNumber
                  ,account_id = AccountId
                  ,account_cid = OutboundCID
                  ,call = whapps_call:new()
                  ,queue = 'undefined'
                  ,auth_doc_id = AuthDocId
                  ,callback_delay = case is_integer(CallbackDelay) of
                                        'true' -> CallbackDelay * ?MILLISECONDS_IN_SECOND;
                                        'false' -> whapps_config:get_integer(?CCCP_CONFIG_CAT, <<"callback_delay">>, 3) * ?MILLISECONDS_IN_SECOND
                                    end
                 }}.

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
handle_cast({'gen_listener', {'created_queue', Q}}, #state{queue='undefined'}=S) ->
    gen_listener:cast(self(), 'originate_park'),
    {'noreply', S#state{queue = Q}};
handle_cast('originate_park', State) ->
    originate_park(State),
    {'noreply', State};
handle_cast({'call_update', CallUpdate}, State) ->
    {'noreply', State#state{call=CallUpdate}};
handle_cast({'offnet_ctl_queue', CtrlQ}, State) ->
    {'noreply', State#state{offnet_ctl_q=CtrlQ}};
handle_cast({'parked', CallId, ToDID}, State) ->
    P = bridge_to_final_destination(CallId, ToDID, State),
    lager:debug("bridging to ~s (via ~s) in ~p", [ToDID, CallId, P]),
    {'noreply', State#state{parked_call_id = CallId}};
handle_cast('stop_callback', State) ->
    {'stop', 'normal', State};
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
handle_event(_JObj, #state{call=Call, b_leg_number=BLegNumber, auth_doc_id=AuthDocId}=_State) ->
    {'reply', [{'call', Call},{b_leg_number, BLegNumber},{auth_doc_id, AuthDocId}]}.

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
    'ok'.

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

-spec originate_park(state()) -> 'ok'.
originate_park(#state{account_id=AccountId
                      ,parked_call_id=CallId
                      ,customer_number=ToDID
                      ,account_cid=CID
                      ,queue=Q
                      ,b_leg_number=BLegNumber
                      ,callback_delay=CallbackDelay
                     }) ->
    _ = timer:sleep(CallbackDelay),
    BowOut = case BLegNumber of
        'undefined' -> 'true';
        _ -> 'false'
    end,
    Req = cccp_util:build_request(CallId, ToDID, CID, Q, 'undefined', AccountId, <<"park">>, BowOut),
    wapi_resource:publish_originate_req(Req).

-spec handle_resource_response(wh_json:object(), wh_proplist()) -> 'ok'.
handle_resource_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case wh_util:get_event_type(JObj) of
        {<<"dialplan">>,<<"route_win">>} ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            gen_listener:cast(Srv, {'call_update', whapps_call:from_route_win(JObj,call(Props))}),
            gen_listener:add_binding(Srv, {'call',[{'callid', CallId},{restrict_to,[<<"CHANNEL_ANSWER">>,<<"CHANNEL_REPLACED">>]}]});
        {<<"call_event">>,<<"CHANNEL_REPLACED">>} ->
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),
            gen_listener:rm_binding(Srv, {'call',[]}),
            CallIdNew = wh_json:get_value(<<"Replaced-By">>, JObj),
            gen_listener:cast(Srv, {'call_id_update', CallIdNew}),
            gen_listener:add_binding(Srv, {'call',[{'callid', CallIdNew}]});
        {<<"call_event">>,<<"CHANNEL_ANSWER">>} ->
            CallUpdate = whapps_call:kvs_store_proplist([{'consumer_pid', self()},{'auth_doc_id', props:get_value('auth_doc_id',Props)}], whapps_call:from_route_req(JObj,call(Props))),
            gen_listener:cast(Srv, {'call_update', CallUpdate}),
            gen_listener:cast(Srv, {'parked', CallId, b_leg_number(props:set_value(call, CallUpdate,Props))});
        {<<"call_event">>,<<"CHANNEL_DESTROY1">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        {<<"call_event">>,<<"CHANNEL_EXECUTE_COMPLETE">>} ->
            cccp_util:handle_disconnect(JObj, Props);
        {<<"resource">>,<<"originate_resp">>} ->
            handle_originate_response(JObj, Props);
        _ -> 'ok'
    end.

-spec handle_originate_response(wh_json:object(), server_ref()) -> 'ok'.
handle_originate_response(JObj, Props) ->
    Srv = props:get_value('server', Props),
    case {wh_json:get_value(<<"Application-Name">>, JObj)
          ,wh_json:get_value(<<"Application-Response">>, JObj)
         }
    of
        {<<"bridge">>, <<"SUCCESS">>} ->
            gen_listener:cast(Srv, 'stop_callback');
        _ -> 'ok'
    end.

-spec bridge_to_final_destination(ne_binary(), ne_binary(), state()) -> 'ok'.
bridge_to_final_destination(CallId, ToDID, #state{queue=Q
                                                  ,offnet_ctl_q=CtrlQ
                                                  ,account_id=AccountId
                                                  ,account_cid=AccountCID
                                                  ,auth_doc_id=AccountDocId
                                                  ,customer_number=CustomerNumber
                                                 }) ->

    cccp_util:bridge(CallId, ToDID, CustomerNumber, Q, CtrlQ, AccountId, AccountCID),

    case AccountDocId of
        'undefined' -> 'ok';
        _ -> cccp_util:store_last_dialed(ToDID, AccountDocId)
    end.

b_leg_number(Props) ->
    case props:get_value('b_leg_number', Props) of
        'undefined' ->
            Call = props:get_value('call', Props),
            {'num_to_dial', Number} = get_number(Call),
            Number;
        BLegNumber -> BLegNumber
    end.

call(Props) ->
    props:get_value('call', Props).

%%%===================================================================
%%% temporary till dtmf dubbing at bowout will be solved
%%%===================================================================

-spec get_number(whapps_call:call()) ->
                        {'num_to_dial', ne_binary()} |
                        'ok'.
-spec get_number(whapps_call:call(), integer()) ->
                        {'num_to_dial', ne_binary()} |
                        'ok'.
get_number(Call) ->
    get_number(Call, 3).

get_number(Call, 0) ->
    lager:info("run out of attempts amount... hanging up"),
    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
    whapps_call_command:queued_hangup(Call);
get_number(Call, Retries) ->
    RedialCode = whapps_config:get(?CCCP_CONFIG_CAT, <<"last_number_redial_code">>, <<"*0">>),
    case whapps_call_command:b_prompt_and_collect_digits(2, 17, <<"cf-enter_number">>, 3, Call) of
        {'ok', RedialCode} ->
            cccp_util:get_last_dialed_number(Call);
        {'ok', EnteredNumber} ->
            cccp_util:verify_entered_number(every_sec(EnteredNumber), Call, Retries);
        _Err ->
            lager:info("no phone number obtained: ~p", [_Err]),
            whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
            get_number(Call, Retries - 1)
    end.

every_sec(Number) when is_binary(Number) ->
    every_sec(wh_util:to_list(Number));
every_sec(Number) ->
    every_sec(Number, []).

every_sec([], Acc) -> Acc;
every_sec([X], Acc) -> Acc ++ [X];
every_sec([X,_], Acc) -> Acc ++ [X];
every_sec([X,_|T], Acc) -> every_sec(T, Acc ++ [X]).

