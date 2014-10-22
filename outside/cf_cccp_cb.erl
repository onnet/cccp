%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Base module for callflow action
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_cccp_cb).

-include("../callflow.hrl").

-export([handle/2]).

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    CallerIdNumber = whapps_call:caller_id_number(Call),
    whapps_call_command:b_hangup(Call),
    case cccp_util:callback_authorize(CallerIdNumber) of
        [AccountId, AccountCID, _ForceCID] ->
            lager:info("Caller information found for ~p. AccountId: ~p AccountCID: ~p", [CallerIdNumber, AccountId, AccountCID]),
            JObj = {[{<<"Number">>, CallerIdNumber}
                    ,{<<"Account-ID">>, AccountId}
                    ,{<<"Outbound-Caller-ID-Number">>, AccountCID}
                   ]},
            cccp_callback_handler:add_request(JObj);
        _ -> 
            lager:info("No caller information found for ~p.", [CallerIdNumber]),
            'ok'
    end,
    cf_exe:stop(Call).
