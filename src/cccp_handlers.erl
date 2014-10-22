-module(cccp_handlers).

-export([handle_route_req/2
        ,handle_route_win/2
        ,handle_config_change/2
        ,handle_all/2
        ,handle_cccp_call/1
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

handle_route_win(JObj, _Props) ->
    lager:info("CCCP has received a route win, taking control of the call"),
    'true' = wapi_route:win_v(JObj),
    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            handle_cccp_call(whapps_call:from_route_win(JObj, Call));
        {'error', _R} ->
            lager:debug("Unable to find call record during route_win")
    end.

handle_cccp_call(Call) ->
    CID = wnm_util:normalize_number(whapps_call:caller_id_number(Call)),
    ReqNum = wnm_util:normalize_number(whapps_call:request_user(Call)),
    CB_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cb_number">>)),
    CC_Number = wnm_util:normalize_number(whapps_config:get(?CCCP_CONFIG_CAT, <<"cccp_cc_number">>)),
    case ReqNum of
        CB_Number ->
            cccp_util:handle_callback(CID, Call);
        CC_Number ->
            cccp_util:handle_call_to_platform(CID, Call)
    end.
    
handle_config_change(_JObj, _Props) ->
    'ok'.
    
handle_all(JObj, Props) ->
    lager:info("CCCP Handle All JObj: ~p", [JObj]),
    lager:info("CCCP Handle All Props: ~p", [Props]).
