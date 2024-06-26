-module(dmt_api).

-behaviour(application).
-behaviour(supervisor).

%% API

-export([start/2]).
-export([stop/1]).
-export([init/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _Args) ->
    ok = setup_metrics(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

%%

-spec init(any()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_) ->
    {ok, IP} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    HealthCheck = enable_health_logging(genlib_app:env(?MODULE, health_check, #{})),
    EventHandlers = genlib_app:env(?MODULE, woody_event_handlers, [scoper_woody_event_handler]),
    API = woody_server:child_spec(
        ?MODULE,
        #{
            ip => IP,
            port => genlib_app:env(?MODULE, port, 8022),
            transport_opts => genlib_app:env(?MODULE, transport_opts, #{}),
            protocol_opts => genlib_app:env(?MODULE, protocol_opts, #{}),
            event_handler => EventHandlers,
            handlers => get_repository_handlers(),
            additional_routes => [
                get_prometheus_route(),
                erl_health_handle:get_route(HealthCheck)
            ]
        }
    ),
    Cache = dmt_api_cache:child_spec(),
    Children = [Cache, API],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 60}, Children}}.

get_repository_handlers() ->
    Repository = genlib_app:env(?MODULE, repository, dmt_api_repository_v5),
    DefaultTimeout = genlib_app:env(?MODULE, default_woody_handling_timeout, timer:seconds(30)),
    [
        get_handler(repository, #{
            repository => Repository,
            default_handling_timeout => DefaultTimeout
        }),
        get_handler(repository_client, #{
            repository => Repository,
            default_handling_timeout => DefaultTimeout
        })
        | get_machinery_handlers(Repository)
    ].

-spec get_handler(repository | repository_client | state_processor, woody:options()) ->
    woody:http_handler(woody:th_handler()).
get_handler(repository, Options) ->
    {"/v1/domain/repository", {
        {dmsl_domain_conf_thrift, 'Repository'},
        {dmt_api_repository_handler, Options}
    }};
get_handler(repository_client, Options) ->
    {"/v1/domain/repository_client", {
        {dmsl_domain_conf_thrift, 'RepositoryClient'},
        {dmt_api_repository_client_handler, Options}
    }}.

-spec get_machinery_handlers(module()) ->
    [woody:http_handler(woody:th_handler())].
get_machinery_handlers(Repository) ->
    [
        machinery_mg_backend:get_handler(
            {Repository, #{
                path => "/v1/stateproc",
                backend_config => #{schema => Repository}
            }}
        ),
        machinery_modernizer_mg_backend:get_handler(
            #{
                path => <<"/v1/modernizer">>,
                backend_config => #{schema => Repository}
            }
        )
    ].

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).

-spec get_prometheus_route() -> {iodata(), module(), _Opts :: any()}.
get_prometheus_route() ->
    {"/metrics/[:registry]", prometheus_cowboy2_handler, []}.

setup_metrics() ->
    ok = woody_ranch_prometheus_collector:setup(),
    ok = woody_hackney_prometheus_collector:setup().
