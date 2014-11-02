-module(cccp_util).

-export([truncate_plus/1
        ,handle_callinfo/2
        ,relay_amqp/2
        ,cid_authorize/1
        ,handle_disconnect/2
        ,get_number/1
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
    case whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME) of
        {'ok', Call} ->
            relay_event(JObj, Call);
        _ ->
            'ok'
    end.

relay_event(JObj, Call) ->
    RouteWinPid = whapps_call:kvs_fetch('consumer_pid', Call),
    case is_pid(RouteWinPid) of
        true ->
            whapps_call_command:relay_event(RouteWinPid, JObj);
        _ ->
            'ok'
    end.



handle_disconnect(JObj, _Props) ->
    {'ok', Call} = whapps_call:retrieve(wh_json:get_value(<<"Call-ID">>, JObj), ?APP_NAME),
    case (<<"CHANNEL_EXECUTE_COMPLETE">> =:= wh_json:get_value(<<"Event-Name">>, JObj)) 
             andalso 
         is_binary(wh_json:get_value(<<"Hangup-Code">>, JObj)) of
        'true' ->
             case wh_json:get_value(<<"Disposition">>, JObj) of
                'undefined' ->
                    'ok';
                <<"UNALLOCATED_NUMBER">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                <<"INVALID_NUMBER_FORMAT">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                <<"CALL_REJECTED">> ->
                    whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
                    whapps_call_command:queued_hangup(Call);
                _ ->
                    whapps_call_command:queued_hangup(Call)
             end;
         _ -> 'ok'
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

get_number(Call) ->
    case whapps_call_command:b_prompt_and_collect_digits(3, 12, <<"cf-enter_number">>, 3, Call) of
       {ok,<<>>} ->
           whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:queued_hangup(Call);
       {ok, EnteredNumber} ->
           Number = wnm_util:to_e164(re:replace(EnteredNumber, "[^0-9]", "", [global, {return, 'binary'}])),
           lager:info("Phone number entered: ~p. Normalized number: ~p", [EnteredNumber, Number]),
           {'num_to_dial', cccp_util:truncate_plus(Number)};
       _ ->
           lager:info("No Phone number obtained."),
           whapps_call_command:prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:queued_hangup(Call)
     end.

