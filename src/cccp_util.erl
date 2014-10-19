-module(cccp_util).

-export([cid_authorize/1
        ,handle_callback/2
        ,handle_call_platform/2
        ]).

-include("cccp.hrl").


cid_authorize(CID) ->
    ViewOptions = [{'key', CID}],
    case couch_mgr:get_results(?CCCPS_DB, <<"cccps/cid_listing">>, ViewOptions) of
        {ok,[]} ->
            lager:info("CCCP Callback request is Not authorized for ~p. No CID in Db.", [CID]),
            'unauthorized';
        {ok, [JObj]} -> 
            lager:info("IAM Callback request is authorized for ~p", [JObj]),
            [
             wh_json:get_value([<<"value">>,<<"account_id">>], JObj),
             wh_json:get_value([<<"value">>,<<"outbound_cid">>], JObj),
             wh_json:get_value([<<"value">>,<<"force_outbound_cid">>], JObj)
            ];
        E -> 
            lager:info("CCCP Callback request is Not authorized for ~p. Error occurred: ~p.", [CID, E]),
            'unauthorized'
    end.

handle_callback(CallerNumber, Call) ->
    lager:info("CCCP. Inside handle_callback"),
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
            lager:info("CCCP. No caller information found for ~p.", [CallerNumber]),
            'ok'
    end.

handle_call_platform(CID, Call) ->
    lager:info("CCCP. Inside handle_call_platform"),
    whapps_call_command:answer(Call),
    case cid_authorize(CID) of
        [_AccountId, _AccountCID, _ForceCID] ->
            allow_dial(Call);
        _ ->
            pin_authorize(Call) 
    end.

allow_dial(Call) ->
    play_ringing(Call),
    play_dialtone(Call).

pin_authorize(Call) ->
    play_ringing(Call),
    Prompt = <<"disa-enter_pin">>,
    NoopId = whapps_call_command:prompt(Prompt, Call),
    case whapps_call_command:collect_digits(6
                                            ,whapps_call_command:default_collect_timeout()
                                            ,whapps_call_command:default_interdigit_timeout()
                                            ,NoopId
                                            ,Call
                                           )
    of
        {'ok', Pin} ->
            check_pin(Pin);
        E ->
            lager:info("caller entered bad pin: '~s'", [E]),
            _ = whapps_call_command:b_prompt(<<"disa-invalid_pin">>, Call),
            whapps_call_command:queued_hangup(Call)
    end.


check_pin(Pin)->
    lager:info("CCCP checking Pin: ~p", [Pin]).


play_dialtone(Call) ->
    lager:info("CCCP. Inside play_dialtone"),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"350">>, <<"440">>]}
                              ,{<<"Duration-ON">>, <<"5000">>}
                              ,{<<"Duration-OFF">>, <<"0">>}
                              ]),
    whapps_call_command:tones([Tone], Call).


play_ringing(Call) ->
    lager:info("CCCP. Inside play_ringing"),
    Tone = wh_json:from_list([{<<"Frequencies">>, [<<"440">>, <<"480">>]}
                              ,{<<"Duration-ON">>, <<"2000">>}
                              ,{<<"Duration-OFF">>, <<"4000">>}
                              ,{<<"Repeat">>, 1}
                             ]),
    whapps_call_command:tones([Tone], Call).

