-module(cccp_util).

-export([truncate_plus/1
        ,handle_callinfo/2
        ,relay_amqp/2
        ,cid_authorize/1
        ,handle_disconnect/2
        ]).

-include("cccp.hrl").

truncate_plus(Number) ->
    case Number of
        <<$+, PluslessNumber/binary>> ->
            PluslessNumber;
        PluslessNumber ->
            PluslessNumber
    end.

handle_callinfo(JObj, Props) ->
    relay_amqp(JObj, Props).

relay_amqp(JObj, _Props) ->
    {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
    RouteWinPid = whapps_call:kvs_fetch('consumer_pid', Call),
    case is_pid(RouteWinPid) of
        true ->
            whapps_call_command:relay_event(RouteWinPid, JObj);
        _ ->
            'ok'
    end.

handle_disconnect(JObj, _Props) ->
    case (<<"CHANNEL_EXECUTE_COMPLETE">> =:= wh_json:get_value(<<"Event-Name">>, JObj)) 
          andalso 
         is_binary(wh_json:get_value(<<"Hangup-Code">>, JObj)) of
            true ->
                {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
                whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                whapps_call_command:queued_hangup(Call);
            _ -> ok
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
