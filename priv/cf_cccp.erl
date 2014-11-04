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
    cccp_platform_sup:new(Call).
