-module(dmt_api_repository_handler).

-behaviour(woody_server_thrift_handler).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-export([handle_function/4]).

-type options() :: #{
    repository := module(),
    default_handling_timeout := timeout()
}.

-export_type([options/0]).

%% Internal types

-type context() :: woody_context:ctx().

%% API

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), options()) -> {ok, term()} | no_return().
handle_function(Function, Args, WoodyContext0, Options) ->
    DefaultDeadline = woody_deadline:from_timeout(default_handling_timeout(Options)),
    WoodyContext = dmt_api_woody_utils:ensure_woody_deadline_set(WoodyContext0, DefaultDeadline),
    do_handle_function(Function, Args, WoodyContext, Options).

%% Internals

-spec do_handle_function
    ('Commit', woody:args(), context(), woody:options()) -> {ok, dmt_api_repository:version()} | no_return();
    ('Checkout', woody:args(), context(), woody:options()) -> {ok, dmt_api_repository:snapshot()} | no_return();
    ('PullRange', woody:args(), context(), woody:options()) -> {ok, dmt_api_repository:history()} | no_return();
    ('Pull', woody:args(), context(), woody:options()) -> {ok, dmt_api_repository:history()} | no_return().
do_handle_function('Commit', {Version, Commit}, Context, Options) ->
    case dmt_api_repository:commit(Version, Commit, repository(Options), Context) of
        {ok, VersionNext} ->
            {ok, VersionNext};
        {error, {operation_error, Error}} ->
            woody_error:raise(business, handle_operation_error(Error));
        {error, version_not_found} ->
            woody_error:raise(business, #domain_conf_VersionNotFound{});
        {error, head_mismatch} ->
            woody_error:raise(business, #domain_conf_ObsoleteCommitVersion{});
        {error, migration_in_progress} ->
            woody_error:raise(system, {internal, resource_unavailable, <<"Migration in progress. Please, stand by.">>})
    end;
do_handle_function('Checkout', {Reference}, Context, Options) ->
    case dmt_api_repository:checkout(Reference, repository(Options), Context) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, version_not_found} ->
            woody_error:raise(business, #domain_conf_VersionNotFound{})
    end;
do_handle_function('PullRange', {After, Limit}, Context, Options) ->
    case dmt_api_repository:pull(After, Limit, repository(Options), Context) of
        {ok, History} ->
            {ok, History};
        {error, version_not_found} ->
            woody_error:raise(business, #domain_conf_VersionNotFound{})
    end;
%% depreceted, will be removed soon
do_handle_function('Pull', {Version}, Context, Options) ->
    case dmt_api_repository:pull(Version, undefined, repository(Options), Context) of
        {ok, History} ->
            {ok, History};
        {error, version_not_found} ->
            woody_error:raise(business, #domain_conf_VersionNotFound{})
    end.

%%

handle_operation_error({conflict, Conflict}) ->
    #domain_conf_OperationConflict{
        conflict = handle_operation_conflict(Conflict)
    };
handle_operation_error({invalid, Invalid}) ->
    #domain_conf_OperationInvalid{
        errors = handle_operation_invalid(Invalid)
    }.

handle_operation_conflict({object_already_exists, Ref}) ->
    {object_already_exists, #domain_conf_ObjectAlreadyExistsConflict{object_ref = Ref}};
handle_operation_conflict({object_not_found, Ref}) ->
    {object_not_found, #domain_conf_ObjectNotFoundConflict{object_ref = Ref}};
handle_operation_conflict({object_reference_mismatch, Ref}) ->
    {object_reference_mismatch, #domain_conf_ObjectReferenceMismatchConflict{object_ref = Ref}}.

handle_operation_invalid({objects_not_exist, Refs}) ->
    [
        {object_not_exists, #domain_conf_NonexistantObject{
            object_ref = Ref,
            referenced_by = ReferencedBy
        }}
     || {Ref, ReferencedBy} <- Refs
    ];
handle_operation_invalid({object_reference_cycles, Cycles}) ->
    [
        {object_reference_cycle, #domain_conf_ObjectReferenceCycle{cycle = Cycle}}
     || Cycle <- Cycles
    ].

-spec repository(options()) -> module().
repository(#{repository := Repository}) ->
    Repository.

-spec default_handling_timeout(options()) -> timeout().
default_handling_timeout(#{default_handling_timeout := Timeout}) ->
    Timeout.
