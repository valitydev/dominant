-module(dmt_api_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([pull_commit/1]).
-export([retry_commit/1]).
-export([insert/1]).
-export([update/1]).
-export([delete/1]).
-export([missing_version/1]).
-export([obsolete/1]).
-export([conflict/1]).
-export([nonexistent/1]).
-export([reference_cycles/1]).

-export([checkout_object/1]).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% tests descriptions

-type config() :: [{atom(), term()}].

-define(config(Key, C), (element(2, lists:keyfind(Key, 1, C)))).
% to emulate unlimited polling
-define(DEFAULT_LIMIT, 9001).

-type test_case_name() :: atom().
-type group_name() :: atom().

-spec all() -> [{group, group_name()}].
all() ->
    [
        {group, basic_lifecycle_v5},
        {group, repository_client}
    ].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {basic_lifecycle_v5, [sequence], [
            pull_commit,
            {group, basic_lifecycle},
            {group, error_mapping},
            retry_commit
        ]},
        {basic_lifecycle, [sequence, {repeat, 10}, shuffle], [
            insert,
            update,
            delete
        ]},
        {error_mapping, [sequence], [
            missing_version,
            obsolete,
            conflict,
            nonexistent,
            reference_cycles
        ]},
        {repository_client, [parallel], [
            checkout_object
        ]}
    ].

%%
%% starting/stopping
-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?config(suite_apps, C)).

-spec init_per_group(group_name(), config()) -> config().
init_per_group(basic_lifecycle_v5, C) ->
    [{group_apps, start_with_repository(dmt_api_repository_v5) ++ start_client()} | C];
init_per_group(repository_client, C) ->
    [{group_apps, start_with_repository(dmt_api_repository_v5) ++ start_client()} | C];
init_per_group(_, C) ->
    C.

start_with_repository(Repository) ->
    genlib_app:start_application_with(dmt_api, [
        {repository, Repository},
        {services, #{
            automaton => #{
                url => "http://machinegun:8022/v1/automaton"
            }
        }},
        % 100Kb
        {max_cache_size, 102400}
    ]).

start_client() ->
    genlib_app:start_application_with(dmt_client, [
        % milliseconds
        {cache_update_interval, 5000},
        {cache_update_pull_limit, ?DEFAULT_LIMIT},
        {max_cache_size, #{
            elements => 20,
            % 50Mb
            memory => 52428800
        }},
        {service_urls, #{
            'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
        }}
    ]).

-spec end_per_group(group_name(), config()) -> term().
end_per_group(GroupName, C) when
    GroupName == basic_lifecycle_v5;
    GroupName == repository_client
->
    genlib_app:stop_unload_applications(?config(group_apps, C));
end_per_group(_, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(_, C) ->
    %% added because dmt_client:checkout(latest)
    %% could return old version from cache otherwise
    {ok, _Version} = dmt_client_cache:update(),
    C.

-spec end_per_testcase(test_case_name(), config()) -> term().
end_per_testcase(_, _) ->
    ok.

%%
%% tests

-spec insert(term()) -> term().
insert(_C) ->
    ID = next_id(),
    Object = fixture_domain_object(ID, <<"InsertFixture">>),
    Ref = fixture_object_ref(ID),
    #domain_conf_ObjectNotFound{} = (catch dmt_client:checkout_object(Ref)),
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    Version2 = dmt_client:commit(
        Version1,
        #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object}}]}
    ),
    _ = dmt_client_cache:update(),
    Object = dmt_client:checkout_object(Ref),
    #domain_conf_ObjectNotFound{} = (catch dmt_client:checkout_object(Version1, Ref)),
    Object = dmt_client:checkout_object(Version2, Ref).

-spec update(term()) -> term().
update(_C) ->
    ID = next_id(),
    Object1 = fixture_domain_object(ID, <<"UpdateFixture1">>),
    Object2 = fixture_domain_object(ID, <<"UpdateFixture2">>),
    Ref = fixture_object_ref(ID),
    #domain_conf_Snapshot{version = Version0} = dmt_client:checkout(latest),
    Version1 = dmt_client:commit(
        Version0,
        #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object1}}]}
    ),
    Version2 = dmt_client:commit(
        Version1,
        #domain_conf_Commit{ops = [{update, #domain_conf_UpdateOp{old_object = Object1, new_object = Object2}}]}
    ),
    _ = dmt_client_cache:update(),
    Object1 = dmt_client:checkout_object(Version1, Ref),
    Object2 = dmt_client:checkout_object(Version2, Ref).

-spec delete(term()) -> term().
delete(_C) ->
    ID = next_id(),
    Object = fixture_domain_object(ID, <<"DeleteFixture">>),
    Ref = fixture_object_ref(ID),
    #domain_conf_Snapshot{version = Version0} = dmt_client:checkout(latest),
    Version1 = dmt_client:commit(
        Version0,
        #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object}}]}
    ),
    Version2 = dmt_client:commit(
        Version1,
        #domain_conf_Commit{ops = [{remove, #domain_conf_RemoveOp{object = Object}}]}
    ),
    Object = dmt_client:checkout_object(Version1, Ref),
    #domain_conf_ObjectNotFound{} = (catch dmt_client:checkout_object(Version2, Ref)).

-spec pull_commit(term()) -> term().
pull_commit(_C) ->
    ID = next_id(),
    History1 = #{} = dmt_client:pull_range(0, ?DEFAULT_LIMIT),
    Version1 = lists:max([0 | maps:keys(History1)]),
    Object = fixture_domain_object(ID, <<"PullFixture">>),
    Commit = #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object}}]},
    Version2 = dmt_client:commit(Version1, Commit),
    #{Version2 := Commit} = dmt_client:pull_range(Version1, ?DEFAULT_LIMIT).

-spec retry_commit(term()) -> term().
retry_commit(_C) ->
    Commit1 = #domain_conf_Commit{
        ops = [
            {insert, #domain_conf_InsertOp{
                object = fixture_domain_object(next_id(), <<"RetryCommitFixture">>)
            }}
        ]
    },
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    Version2 = dmt_client:commit(Version1, Commit1),
    Version2 = Version1 + 1,
    Version2 = dmt_client:commit(Version1, Commit1),
    #domain_conf_Snapshot{version = Version2} = dmt_client:checkout(latest),
    Commit2 = #domain_conf_Commit{
        ops = [
            {insert, #domain_conf_InsertOp{
                object = fixture_domain_object(next_id(), <<"RetryCommitFixture">>)
            }}
        ]
    },
    Version3 = dmt_client:commit(Version2, Commit2),
    Version3 = Version2 + 1,
    Version2 = dmt_client:commit(Version1, Commit1),
    #domain_conf_Snapshot{version = Version3} = dmt_client:checkout(latest).

-spec missing_version(term()) -> term().
missing_version(_C) ->
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    _ = ?assertThrow(
        #domain_conf_VersionNotFound{},
        dmt_client:commit(
            Version1 + 42,
            #domain_conf_Commit{
                ops = [
                    {insert, #domain_conf_InsertOp{
                        object = fixture_domain_object(next_id(), <<"MissingVersionFixture">>)
                    }}
                ]
            }
        )
    ).

-spec obsolete(term()) -> term().
obsolete(_C) ->
    Commit1 = #domain_conf_Commit{
        ops = [
            {insert, #domain_conf_InsertOp{
                object = fixture_domain_object(next_id(), <<"InitialFixture">>)
            }}
        ]
    },
    Commit2 = #domain_conf_Commit{
        ops = [
            {insert, #domain_conf_InsertOp{
                object = fixture_domain_object(next_id(), <<"ObsoleteFixture">>)
            }}
        ]
    },
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    _Version2 = dmt_client:commit(Version1, Commit1),
    _ = ?assertThrow(
        #domain_conf_ObsoleteCommitVersion{},
        dmt_client:commit(Version1, Commit2)
    ).

-spec conflict(term()) -> term().
conflict(_C) ->
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    _ = ?assertThrow(
        #domain_conf_OperationConflict{
            conflict =
                {object_not_found, #domain_conf_ObjectNotFoundConflict{
                    object_ref = {criterion, #domain_CriterionRef{id = 42}}
                }}
        },
        dmt_client:commit(Version1, #domain_conf_Commit{
            ops = [
                {update, #domain_conf_UpdateOp{
                    old_object = criterion_w_refs(42, []),
                    new_object = criterion_w_refs(42, [43, 44, 45])
                }}
            ]
        })
    ).

-spec nonexistent(term()) -> term().
nonexistent(_C) ->
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    _ = ?assertThrow(
        #domain_conf_OperationInvalid{
            errors = [
                {object_not_exists, #domain_conf_NonexistantObject{
                    object_ref = {criterion, #domain_CriterionRef{}},
                    referenced_by = [{criterion, #domain_CriterionRef{id = 42}}]
                }}
                | _
            ]
        },
        dmt_client:insert(Version1, criterion_w_refs(42, [43, 44, 45]))
    ).

-spec reference_cycles(term()) -> term().
reference_cycles(_C) ->
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    _ = ?assertThrow(
        #domain_conf_OperationInvalid{
            errors = [
                %% we expect 3 cycles to be found
                {object_reference_cycle, #domain_conf_ObjectReferenceCycle{
                    cycle = [{criterion, #domain_CriterionRef{}} | _]
                }},
                {object_reference_cycle, #domain_conf_ObjectReferenceCycle{
                    cycle = [{criterion, #domain_CriterionRef{}} | _]
                }},
                {object_reference_cycle, #domain_conf_ObjectReferenceCycle{
                    cycle = [{criterion, #domain_CriterionRef{}} | _]
                }}
            ]
        },
        dmt_client:insert(
            Version1,
            [
                criterion_w_refs(1, [2]),
                criterion_w_refs(2, [3]),
                criterion_w_refs(3, [4, 1]),
                criterion_w_refs(4, [1, 2])
            ]
        )
    ).

-spec checkout_object(term()) -> term().
checkout_object(_C) ->
    ID = next_id(),
    Object = fixture_domain_object(ID, <<"InsertFixture">>),
    Ref = fixture_object_ref(ID),
    #domain_conf_Snapshot{version = Version1} = dmt_client:checkout(latest),
    Version2 = dmt_client:commit(
        Version1,
        #domain_conf_Commit{ops = [{insert, #domain_conf_InsertOp{object = Object}}]}
    ),
    ?assertEqual(
        {ok, #domain_conf_VersionedObject{version = Version2, object = Object}},
        call_checkout_object({head, #domain_conf_Head{}}, Ref)
    ),
    ?assertEqual(
        {exception, #domain_conf_ObjectNotFound{}},
        call_checkout_object({version, Version1}, Ref)
    ),
    ?assertEqual(
        {ok, #domain_conf_VersionedObject{version = Version2, object = Object}},
        call_checkout_object({version, Version2}, Ref)
    ),
    ?assertEqual(
        {exception, #domain_conf_VersionNotFound{}},
        call_checkout_object({version, Version2 + 1}, Ref)
    ).

next_id() ->
    erlang:system_time(micro_seconds) band 16#7FFFFFFF.

fixture_domain_object(Ref, Data) ->
    {category, #domain_CategoryObject{
        ref = #domain_CategoryRef{id = Ref},
        data = #domain_Category{name = Data, description = Data}
    }}.

fixture_object_ref(Ref) ->
    {category, #domain_CategoryRef{id = Ref}}.

criterion_w_refs(ID, Refs) ->
    {criterion, #domain_CriterionObject{
        ref = #domain_CriterionRef{id = ID},
        data = #domain_Criterion{
            name = genlib:format(ID),
            predicate = {any_of, ordsets:from_list([{criterion, #domain_CriterionRef{id = Ref}} || Ref <- Refs])}
        }
    }}.

%%

call_checkout_object(Version, ObjectReference) ->
    call('RepositoryClient', 'checkoutObject', {Version, ObjectReference}).

call(ServiceName, Function, Args) ->
    Url = <<"http://dominant:8022/v1/domain/repository_client">>,
    Call = {{dmsl_domain_conf_thrift, ServiceName}, Function, Args},
    CallOpts = #{
        url => Url,
        event_handler => [scoper_woody_event_handler]
    },
    woody_client:call(Call, CallOpts, woody_context:new()).
