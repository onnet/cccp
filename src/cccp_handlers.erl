-module(cccp_handlers).

-export([handle_route_req/2
        ,handle_route_win/2
        ,handle_config_change/2
        ,handle_callinfo/2
        ,truncate_plus/1
        ,relay_amqp/2
        ]).

-include("cccp.hrl").

-spec handle_route_req(wh_json:object(), wh_proplist()) -> any().
handle_route_req(JObj, Props) ->
    'true' = wapi_route:req_v(JObj),

    Call = whapps_call:from_route_req(JObj),

    ReqNum = wnm_util:normalize_number(whapps_call:request_user(Call)),
    CB_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),

    case lists:member(ReqNum, [CB_Number, CC_Number]) of 
      true ->
          Q = props:get_value('queue', Props),
          Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                         ,{<<"Method">>, <<"park">>}
                                         | wh_api:default_headers(Q, ?APP_NAME, ?APP_VERSION)
                                        ]),
          ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
          Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
          whapps_util:amqp_pool_send(Resp, Publisher),
          whapps_call:cache(Call, ?APP_NAME);
       _ ->
          'ok'
    end.

handle_route_win(JObj, Props) ->
    lager:info("CCCP has received a route win, taking control of the call"),
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case whapps_call:retrieve(CallId, ?APP_NAME) of
        {'ok', Call} ->
            WonCall = whapps_call:kvs_store('relay_amqp_pid', self(), whapps_call:from_route_win(JObj, Call)),
            whapps_call:cache(WonCall, ?APP_NAME),
            Srv = props:get_value('server', Props),
            gen_listener:add_binding(Srv, {'call',[{'callid', CallId}]}),
            gen_listener:add_responder(Srv, {?MODULE, 'handle_callinfo'}, [{<<"*">>, <<"*">>}]),
            handle_cccp_call(WonCall);
        {'error', _R} ->
            lager:debug("Unable to find call record during route_win")
    end.

handle_config_change(_JObj, _Props) ->
    'ok'.
    
handle_callinfo(JObj, Props) ->
    lager:debug("handle_callinfo JObj: ~p", [JObj]),
    relay_amqp(JObj, Props).

handle_cccp_call(Call) ->
    CID = wnm_util:normalize_number(whapps_call:caller_id_number(Call)),
    ReqNum = wnm_util:normalize_number(whapps_call:request_user(Call)),
    CB_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),
    case ReqNum of
        CB_Number ->
            handle_callback(CID, Call);
        CC_Number ->
            handle_call_to_platform(CID, Call)
    end.

handle_callback(CallerNumber, Call) ->
    whapps_call_command:hangup(Call),
    case cid_authorize(CallerNumber) of
        [AccountId, AccountCID, _ForceCID] ->
            JObj = {[{<<"Number">>, CallerNumber}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Outbound-Caller-ID-Number">>, AccountCID}
                   ]},
            timer:sleep(2000),
            cccp_callback_handler:add_request(JObj);
        _ ->
            lager:info("No caller information found for ~p. Won't call it back.", [CallerNumber]),
            'ok'
    end.

handle_call_to_platform(CID, Call) ->
    whapps_call_command:answer(Call),
    case cid_authorize(CID) of
        [AccountId, AccountCID, ForceCID] ->
            dial(AccountId, AccountCID, ForceCID, Call);
        _ ->
            pin_authorize(Call) 
    end.

cid_authorize(CID) ->
    ViewOptions = [{'key', CID}],
    case couch_mgr:get_results(?CCCPS_DB, <<"cccps/cid_listing">>, ViewOptions) of
        {ok,[]} ->
            lager:info("Auth by CID failed for: ~p. No CID in Db.", [CID]),
            'unauthorized';
        {ok, [JObj]} -> 
            lager:info("CID ~p is authorized.", [JObj]),
            [
             wh_json:get_value([<<"value">>,<<"account_id">>], JObj),
             wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj),
             wh_json:get_value([<<"value">>,<<"force_outbound_cid">>], JObj)
            ];
        E -> 
            lager:info("Auth by CID failed for ~p. Error occurred: ~p.", [CID, E]),
            'unauthorized'
    end.

dial(AccountId, AccountCID, ForceCID, Call) ->
    {'num_to_dial', Number} = get_number(Call),
    EP = stepswitch_resources:endpoints(Number, wh_json:new()),
    lager:info("EndPoints: ~p", [EP]),
    Call1 = whapps_call:set_account_id(AccountId, Call),
    Call2 = case ForceCID of
        'false' -> Call1;
         _ -> whapps_call:set_caller_id_number(AccountCID, Call1)
    end,
    lager:info("Outbound Call: ~p", [Call2]),
    whapps_call_command:bridge(EP, Call2).

get_number(Call) ->
    case whapps_call_command:b_prompt_and_collect_digits(3, 12, <<"cf-enter_number">>, 3, Call) of
       {ok,<<>>} ->
           whapps_call_command:b_prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:hangup(Call);
       {ok, EnteredNumber} ->
           Number = wnm_util:to_e164(EnteredNumber),
           lager:info("Phone number entered: ~p. Normalized number: ~p", [EnteredNumber, Number]),
           {'num_to_dial', truncate_plus(Number)};
       _ ->
           lager:info("No Phone number obtained."),
           whapps_call_command:b_prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:hangup(Call)
     end.

pin_authorize(Call) ->
    case whapps_call_command:b_prompt_and_collect_digits(9,12,<<"disa-enter_pin">>,3,Call) of
       {ok,<<>>} ->
           whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
           whapps_call_command:hangup(Call);
       {ok, EnteredPin} ->
           lager:info("Pin entered."),
           case check_pin(EnteredPin) of
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
        
check_pin(Pin)->
    ViewOptions = [{'key', Pin}],
    case couch_mgr:get_results(?CCCPS_DB, <<"cccps/pin_listing">>, ViewOptions) of
        {ok,[]} ->
            lager:info("Auth by Pin failed for: ~p. No Pin in Db.", [Pin]),
            'unauthorized';
        {ok, [JObj]} ->
            lager:info("Pin ~p is authorized.", [JObj]),
            [
             wh_json:get_value([<<"value">>,<<"account_id">>], JObj),
             wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj),
             wh_json:get_value([<<"value">>,<<"force_outbound_cid">>], JObj)
            ];
        E ->
            lager:info("Auth by Pin failed for ~p. Error occurred: ~p.", [Pin, E]),
            'unauthorized'
    end.

relay_amqp(JObj, _Props) ->
    {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
    RouteWinPid = whapps_call:kvs_fetch('relay_amqp_pid', Call),
    case is_pid(RouteWinPid) of
        true ->
            whapps_call_command:relay_event(RouteWinPid, JObj);
        _ ->
            'ok'
    end.

truncate_plus(Number) ->
    case Number of
        <<$+, PluslessNumber/binary>> ->
            PluslessNumber;
        PluslessNumber ->
            PluslessNumber
    end.

