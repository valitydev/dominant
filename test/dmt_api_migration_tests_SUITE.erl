-module(dmt_api_migration_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-type config() :: [{atom(), term()}].
-type test_case_name() :: atom().

-define(CONF(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-define(REPOSITORY, dmt_api_repository_v5).

%%

-spec all() -> [test_case_name()].
all() ->
    [
        % NOTE
        % Migration of version 1 events is not testable anymore since
        % https://github.com/valitydev/damsel/commit/4e82c243
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
