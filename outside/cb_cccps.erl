-module(cb_cccps).

-export([init/0
         ,authenticate/1
         ,authorize/1
         ,allowed_methods/0, allowed_methods/1
         ,resource_exists/0, resource_exists/1
         ,content_types_provided/1
         ,content_types_accepted/1
         ,languages_provided/1
         ,charsets_provided/1
         ,encodings_provided/1
         ,validate/1, validate/2
         ,billing/1
         ,put/1
         ,post/2
         ,delete/2
         ,etag/1
         ,expires/1
         ,finish_request/1
        ]).

-include("../crossbar.hrl").

-define(CB_LIST, <<"cccps/crossbar_listing">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    maybe_init_db(),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.cccps">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.cccps">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.cccps">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.content_types_accepted.cccps">>, ?MODULE, 'content_types_accepted'),
    _ = crossbar_bindings:bind(<<"*.languages_provided.cccps">>, ?MODULE, 'languages_provided'),
    _ = crossbar_bindings:bind(<<"*.charsets_provided.cccps">>, ?MODULE, 'charsets_provided'),
    _ = crossbar_bindings:bind(<<"*.encodings_provided.cccps">>, ?MODULE, 'encodings_provided'),
    _ = crossbar_bindings:bind(<<"*.validate.cccps">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.billing">>, ?MODULE, 'billing'),
    _ = crossbar_bindings:bind(<<"*.execute.get.cccps">>, ?MODULE, 'get'),
    _ = crossbar_bindings:bind(<<"*.execute.put.cccps">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.cccps">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.cccps">>, ?MODULE, 'delete'),
    _ = crossbar_bindings:bind(<<"*.etag.cccps">>, ?MODULE, 'etag'),
    _ = crossbar_bindings:bind(<<"*.expires.cccps">>, ?MODULE, 'expires'),
    _ = crossbar_bindings:bind(<<"*.finish_request">>, ?MODULE, 'finish_request').

-spec maybe_init_db() -> 'ok'.
maybe_init_db() ->
    case couch_mgr:db_exists(<<"cccps">>) of
        'true' -> 
             _ = couch_mgr:revise_doc_from_file(<<"cccps">>, 'crossbar', <<"views/cccps.json">>),
            'ok';
        'false' -> init_db()
    end.

-spec init_db() -> 'ok'.
init_db() ->
    couch_mgr:db_create(<<"cccps">>),
    _ = couch_mgr:revise_doc_from_file(<<"cccps">>, 'crossbar', <<"views/cccps.json">>),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authenticates the incoming request, returning true if the requestor is
%% known, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> 'false'.
authenticate(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Authorizes the incoming request, returning true if the requestor is
%% allowed to access the resource, or false if not.
%% @end
%%--------------------------------------------------------------------
-spec authorize(cb_context:context()) -> 'false'.
authorize(_) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
allowed_methods() ->
    [?HTTP_GET, ?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_POST, ?HTTP_DELETE].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /cccps => []
%%    /cccps/foo => [<<"foo">>]
%%    /cccps/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be using to respond (matched against
%% client's accept header)
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% What content-types will the module be requiring (matched to the client's
%% Content-Type header
%% Of the form {atom, [{Type, SubType}]} :: {to_json, [{<<"application">>, <<"json">>}]}
%% @end
%%--------------------------------------------------------------------
-spec content_types_accepted(cb_context:context()) -> cb_context:context().
content_types_accepted(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative languages, return a list of languages and optional
%% quality value:
%% [<<"en">>, <<"en-gb;q=0.7">>, <<"da;q=0.5">>]
%% @end
%%--------------------------------------------------------------------
-spec languages_provided(cb_context:context()) -> cb_context:context().
languages_provided(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative charsets, return a list of charsets and optional
%% quality value:
%% [<<"iso-8859-5">>, <<"unicode-1-1;q=0.8">>]
%% @end
%%--------------------------------------------------------------------
-spec charsets_provided(cb_context:context()) -> cb_context:context().
charsets_provided(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you provide alternative encodings, return a list of encodings and optional
%% quality value:
%% [<<"gzip;q=1.0">>, <<"identity;q=0.5">>, <<"*;q=0">>]
%% @end
%%--------------------------------------------------------------------
-spec encodings_provided(cb_context:context()) -> cb_context:context().
encodings_provided(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /cccps mights load a list of cccp objects
%% /cccps/123 might load the cccp object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context) ->
    validate_cccps(Context, cb_context:req_verb(Context)).
validate(Context, Id) ->
    validate_cccp(Context, Id, cb_context:req_verb(Context)).

-spec validate_cccps(cb_context:context(), http_method()) -> cb_context:context().
validate_cccps(Context, ?HTTP_GET) ->
    summary(Context);
validate_cccps(Context, ?HTTP_PUT) ->
    create(Context).

-spec validate_cccp(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate_cccp(Context, Id, ?HTTP_GET) ->
    read(Id, Context);
validate_cccp(Context, Id, ?HTTP_POST) ->
    update(Id, Context);
validate_cccp(Context, Id, ?HTTP_DELETE) ->
    read(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you handle billing-related calls, this callback will allow you to
%% execute those.
%% @end
%%--------------------------------------------------------------------
-spec billing(cb_context:context()) -> cb_context:context().
billing(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, _) ->
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verb is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, _) ->
    crossbar_doc:delete(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If you want to manipulate the etag header, change it here in the cb_context{}
%% @end
%%--------------------------------------------------------------------
-spec etag(cb_context:context()) -> cb_context:context().
etag(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Set the expires header
%% @end
%%--------------------------------------------------------------------
-spec expires(cb_context:context()) -> cb_context:context().
expires(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% The response has gone out, do some cleanup of your own here.
%% @end
%%--------------------------------------------------------------------
-spec finish_request(cb_context:context()) -> cb_context:context().
finish_request(Context) ->
    Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create(cb_context:context()) -> cb_context:context().
create(Context) ->
    OnSuccess = fun(C) -> on_successful_validation('undefined', C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read(ne_binary(), cb_context:context()) -> cb_context:context().
read(Id, Context) ->
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing menu document with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update(ne_binary(), cb_context:context()) -> cb_context:context().
update(Id, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(Id, C) end,
    cb_context:validate_request_data(<<"cccps">>, Context, OnSuccess).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_type">>, <<"cccp">>, cb_context:doc(Context)));
on_successful_validation(Id, Context) ->
    crossbar_doc:load_merge(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].
