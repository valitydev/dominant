-module(dmt_api_migration_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").

-include_lib("damsel/include/dmsl_domain_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([fv2_migration_succeeds/1]).
-export([fv2_modernization_succeeds/1]).

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(CONF(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-define(REPOSITORY, dmt_api_repository_v5).

%%

-spec all() -> [test_case_name()].
all() ->
    [
        fv2_migration_succeeds,
        fv2_modernization_succeeds
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps1 = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]),
    Apps2 = genlib_app:start_application_with(dmt_api, [
        {repository, ?REPOSITORY},
        % NOTE
        % Effectively disable cache altogether so it won't spoil testcase expectations.
        % Keep in mind that in production setting this cache will certainly affect migrations'
        % visibility.
        {max_cache_size, 0},
        {services, #{
            automaton => #{
                url => "http://machinegun:8022/v1/automaton"
            }
        }}
    ]),

    Apps3 = genlib_app:start_application_with(dmt_client, [
        {max_cache_size, #{
            elements => 1,
            memory => 102400
        }},
        {service_urls, #{
            'Repository' => <<"http://dominant:8022/v1/domain/repository">>,
            'RepositoryClient' => <<"http://dominant:8022/v1/domain/repository_client">>
        }}
    ]),
    ok = logger:set_primary_config(level, info),
    [{suite_apps, Apps1 ++ Apps2 ++ Apps3} | C].

-spec end_per_suite(config()) -> term().
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?CONF(suite_apps, C)).

-spec init_per_testcase(test_case_name(), config()) -> config().
init_per_testcase(Name, C) ->
    _ = meck:unload(),
    WoodyCtx = woody_context:new(mk_rpc_id(Name)),
    [{woody_ctx, WoodyCtx}, {dmt_client_opts, #{woody_context => WoodyCtx}} | C].

mk_rpc_id(Name) ->
    TraceID = genlib:format("~s:~p", [Name, erlang:system_time(millisecond)]),
    woody_context:new_rpc_id(<<"undefined">>, TraceID, woody_context:new_req_id()).

-spec end_per_testcase(test_case_name(), config()) -> _.
end_per_testcase(_, _) ->
    ok.

%%

-define(FMT_VSN_INITIAL, 1).
-define(FMT_VSN_TARGET, 2).

-define(LEGACY_CONDITION,
    {payment_tool,
        {bank_card, #domain_BankCardCondition{
            definition = {payment_system_is, uzcard}
        }}}
).

-define(TARGET_CONDITION,
    {payment_tool,
        {bank_card, #domain_BankCardCondition{
            definition = undefined
        }}}
).

-spec fv2_migration_succeeds(config()) -> _.
fv2_migration_succeeds(C) ->
    ClientOpts = ?CONF(dmt_client_opts, C),
    ok = meck:new(dmt_api_repository_v5, [passthrough]),
    ok = meck:expect(dmt_api_repository_v5, get_version, fun(_) -> ?FMT_VSN_INITIAL end),
    Version1 = get_latest_version(ClientOpts),
    ObjectRef1 = #domain_CriterionRef{id = dmt_ct_helper:next_id()},
    Commit1 = dmt_ct_helper:mk_insert_commit(
        {criterion, #domain_CriterionObject{
            ref = ObjectRef1,
            data = #domain_Criterion{
                name = <<"Legacy Criterion 1">>,
                predicate = {condition, ?LEGACY_CONDITION}
            }
        }}
    ),
    Version2 = dmt_client_api:commit(Version1, Commit1, ClientOpts),
    ok = meck:expect(dmt_api_repository_v5, get_version, fun(_) -> ?FMT_VSN_TARGET end),
    Snapshot2 = checkout(Version2, ClientOpts),
    ?assertMatch(
        {criterion, #domain_CriterionObject{
            data = #domain_Criterion{
                predicate = {condition, ?TARGET_CONDITION}
            }
        }},
        get_domain_object({criterion, ObjectRef1}, Snapshot2)
    ),
    ObjectRef2 = #domain_CriterionRef{id = dmt_ct_helper:next_id()},
    Commit2 = dmt_ct_helper:mk_insert_commit(
        {criterion, #domain_CriterionObject{
            ref = ObjectRef2,
            data = #domain_Criterion{
                name = <<"Legacy Criterion 2">>,
                predicate = {condition, ?LEGACY_CONDITION}
            }
        }}
    ),
    Version3 = dmt_client_api:commit(Version2, Commit2, ClientOpts),
    Snapshot3 = checkout(Version3, ClientOpts),
    ?assertMatch(
        {criterion, #domain_CriterionObject{
            data = #domain_Criterion{
                predicate = {condition, ?TARGET_CONDITION}
            }
        }},
        get_domain_object({criterion, ObjectRef2}, Snapshot3)
    ),
    ok = meck:unload(dmt_api_repository_v5).

-spec fv2_modernization_succeeds(config()) -> _.
fv2_modernization_succeeds(C) ->
    ClientOpts = ?CONF(dmt_client_opts, C),
    ok = meck:new(dmt_api_repository_v5, [passthrough]),
    ok = meck:expect(dmt_api_repository_v5, get_version, fun(_) -> ?FMT_VSN_INITIAL end),
    Version1 = get_latest_version(ClientOpts),
    ObjectRef = #domain_CriterionRef{id = dmt_ct_helper:next_id()},
    Commit = dmt_ct_helper:mk_insert_commit(
        {criterion, #domain_CriterionObject{
            ref = ObjectRef,
            data = #domain_Criterion{
                name = <<"Legacy Criterion">>,
                predicate = {condition, ?LEGACY_CONDITION}
            }
        }}
    ),
    Version2 = dmt_client_api:commit(Version1, Commit, #{}),
    ok = meck:expect(dmt_api_repository_v5, get_version, fun(_) -> ?FMT_VSN_TARGET end),
    ok = dmt_api_repository_v5:modernize(?CONF(woody_ctx, C)),
    Snapshot2 = checkout(Version2, ClientOpts),
    ?assertMatch(
        {criterion, #domain_CriterionObject{
            data = #domain_Criterion{
                predicate = {condition, ?TARGET_CONDITION}
            }
        }},
        get_domain_object({criterion, ObjectRef}, Snapshot2)
    ),
    ok = meck:unload(dmt_api_repository_v5).

%%

checkout(head, ClientOpts) ->
    dmt_client_api:checkout({head, #domain_conf_Head{}}, ClientOpts);
checkout(Version, ClientOpts) when is_integer(Version) ->
    dmt_client_api:checkout({version, Version}, ClientOpts).

get_latest_version(ClientOpts) ->
    Snapshot = checkout(head, ClientOpts),
    Snapshot#domain_conf_Snapshot.version.

get_domain_object(ObjectRef, #domain_conf_Snapshot{domain = Domain}) ->
    maps:get(ObjectRef, Domain).
