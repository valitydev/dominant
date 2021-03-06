-module(dmt_api_repository).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

%% API

-export([checkout/3]).
-export([checkout_object/4]).
-export([pull/4]).
-export([commit/4]).

-export_type([version/0]).
-export_type([limit/0]).
-export_type([snapshot/0]).
-export_type([commit/0]).
-export_type([history/0]).

-type version() :: dmsl_domain_conf_thrift:'Version'().
-type limit() :: dmsl_domain_conf_thrift:'Limit'() | undefined.
-type snapshot() :: dmsl_domain_conf_thrift:'Snapshot'().
-type commit() :: dmsl_domain_conf_thrift:'Commit'().
-type history() :: dmsl_domain_conf_thrift:'History'().

-type ref() :: dmsl_domain_conf_thrift:'Reference'().
-type object_ref() :: dmsl_domain_thrift:'Reference'().
-type repository() :: module().
-type context() :: woody_context:ctx().

-type operation_error() :: dmt_domain:operation_error().

-callback checkout(ref(), context()) ->
    % TODO this was made due to dialyzer warning, can't find the way to fix it
    {ok, term()}
    | {error, version_not_found}.

-callback pull(version(), limit(), context()) ->
    {ok, history()}
    | {error, version_not_found}.

-callback commit(version(), commit(), context()) ->
    {ok, snapshot()}
    | {error, version_not_found | migration_in_progress | {operation_error, operation_error()}}.

%%

-spec checkout(ref(), repository(), context()) -> {ok, snapshot()} | {error, version_not_found}.
checkout({head, #domain_conf_Head{}} = V, Repository, Context) ->
    case Repository:checkout(V, Context) of
        {ok, Snapshot} ->
            {ok, dmt_api_cache:put(Snapshot)};
        {error, version_not_found} ->
            {error, version_not_found}
    end;
checkout({version, Version} = V, Repository, Context) ->
    case dmt_api_cache:get(Version) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, version_not_found} ->
            case Repository:checkout(V, Context) of
                {ok, Snapshot} ->
                    {ok, dmt_api_cache:put(Snapshot)};
                {error, version_not_found} ->
                    {error, version_not_found}
            end
    end.

-spec checkout_object(ref(), object_ref(), repository(), context()) ->
    {ok, dmsl_domain_conf_thrift:'VersionedObject'()} | {error, version_not_found | object_not_found}.
checkout_object(Reference, ObjectReference, Repository, Context) ->
    case checkout(Reference, Repository, Context) of
        {ok, Snapshot} ->
            try_get_object(ObjectReference, Snapshot);
        {error, _} = Error ->
            Error
    end.

-spec pull(version(), limit(), repository(), context()) -> {ok, history()} | {error, version_not_found}.
pull(Version, Limit, Repository, Context) ->
    Repository:pull(Version, Limit, Context).

-spec commit(version(), commit(), repository(), context()) ->
    {ok, version()}
    | {error, version_not_found | head_mismatch | migration_in_progress | {operation_error, operation_error()}}.
commit(Version, Commit, Repository, Context) ->
    case Repository:commit(Version, Commit, Context) of
        {ok, Snapshot} ->
            #domain_conf_Snapshot{version = VersionNext} = dmt_api_cache:put(Snapshot),
            {ok, VersionNext};
        {error, _} = Error ->
            Error
    end.

%% Internal

-spec try_get_object(object_ref(), snapshot()) ->
    {ok, dmsl_domain_conf_thrift:'VersionedObject'()} | {error, object_not_found}.
try_get_object(ObjectReference, #domain_conf_Snapshot{version = Version, domain = Domain}) ->
    case dmt_domain:get_object(ObjectReference, Domain) of
        {ok, Object} ->
            {ok, #domain_conf_VersionedObject{version = Version, object = Object}};
        error ->
            {error, object_not_found}
    end.
