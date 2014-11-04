-module(cf_cccp).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(_Data, Call) ->
    lager:info("starting CCCP handler"),
    CID = wnm_util:normalize_number(whapps_call:caller_id_number(Call)),
    cccp_util:handle_call_to_platform(CID, Call).
