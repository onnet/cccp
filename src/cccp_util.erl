-module(cccp_util).

-export([handle_callback/2
        ,handle_call_to_platform/2
        ]).

-include("cccp.hrl").


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

handle_callback(CallerNumber, Call) ->
    lager:info("Inside handle_callback"),
    whapps_call_command:hangup(Call),
    case cid_authorize(CallerNumber) of
        [AccountId, AccountCID, _ForceCID] ->
            lager:info("CCCP. Caller information found for ~p. AccountId: ~p AccountCID: ~p", [CallerNumber, AccountId, AccountCID]),
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
    lager:info("CCCP. Inside handle_call_to_platform"),
    whapps_call_command:answer(Call),
    case cid_authorize(CID) of
        [AccountId, AccountCID, ForceCID] ->
            dial(AccountId, AccountCID, ForceCID, Call);
        _ ->
            pin_authorize(Call) 
    end.

dial(AccountId, AccountCID, ForceCID, Call) ->
    case whapps_call_command:b_prompt_and_collect_digits(3,12,<<"cf-enter_number">>,3,Call) of
       {ok,<<>>} ->
           lager:info("No Phone number entered."),
           whapps_call_command:b_prompt(<<"hotdesk-invalid_entry">>, Call),
           whapps_call_command:hangup(Call);
       {ok, EnteredNumber} ->
           Number = wnm_util:to_e164(EnteredNumber),
           lager:info("Phone number entered: ~p. Normalized number: ~p", [EnteredNumber, Number]),
           EP = stepswitch_resources:endpoints(Number, wh_json:new()),
           lager:info("EndPoints: ~p", [EP]),
           Call1 = whapps_call:set_account_id(AccountId, Call),
           Call2 = case ForceCID of
                       'false' -> Call1;
                       _ -> whapps_call:set_caller_id_number(AccountCID, Call1)
                   end,
           lager:info("Outbound Call: ~p", [Call2]),
           whapps_call_command:bridge(EP, Call2);
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

