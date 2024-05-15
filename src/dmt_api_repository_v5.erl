-module(dmt_api_repository_v5).

-behaviour(dmt_api_repository).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-define(NS, 'domain-config').
-define(ID, <<"primary/v5">>).
-define(BASE, 10).

%% API

-export([checkout/2]).
-export([pull/2]).
-export([pull/3]).
-export([commit/3]).

%% State processor

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(event(), _).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result() :: machinery:result(event(), none()).
-type response(T) :: machinery:response(T).

% TODO
% Otherwise dialyzer spews pretty unreasonable complains. Lost any hope telling
% him that callback typespecs are fine.
% -behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).

%% Storage schema

-behaviour(machinery_mg_schema).
-export([marshal/3]).
-export([unmarshal/3]).
-export([get_version/1]).

%% NOTE
%% Used in migration tests.
-export([modernize/1]).

%%
-record(st, {
    snapshot = #domain_conf_Snapshot{version = 0, domain = dmt_domain:new()} :: snapshot(),
    history = #{} :: dmt_api_repository:history()
}).

-type st() :: #st{}.
-type event() :: {commit, commit(), #{snapshot => snapshot()}}.

-type ref() :: dmsl_domain_conf_thrift:'Reference'().
-type snapshot() :: dmt_api_repository:snapshot().
-type commit() :: dmt_api_repository:commit().
-type version() :: dmt_api_repository:version().

-spec checkout(ref(), woody_context:ctx()) ->
    {ok, snapshot()}
    | {error, version_not_found}.
checkout({head, #domain_conf_Head{}}, WoodyCtx) ->
    St = get_history_by_range({undefined, ?BASE, backward}, WoodyCtx),
    squash_state(St);
checkout({version, V}, WoodyCtx) ->
    BaseV = get_base_version(V),
    St = get_history_by_range({get_event_id(BaseV), V - BaseV, forward}, WoodyCtx),
    case squash_state(St) of
        {ok, #domain_conf_Snapshot{version = V}} = Result ->
            Result;
        {ok, _} ->
            {error, version_not_found}
    end.

-spec pull(version(), woody_context:ctx()) ->
    {ok, dmt_api_repository:history()}
    | {error, version_not_found}.
pull(Version, WoodyCtx) ->
    pull(Version, undefined, WoodyCtx).

-spec pull(version(), dmt_api_repository:limit(), woody_context:ctx()) ->
    {ok, dmt_api_repository:history()}
    | {error, version_not_found}.
pull(Version, Limit, WoodyCtx) ->
    After = get_event_id(Version),
    St = get_history_by_range({After, Limit, forward}, WoodyCtx),
    {ok, St#st.history}.

-spec commit(version(), commit(), woody_context:ctx()) ->
    {ok, snapshot()}
    | {error, version_not_found | {operation_error, dmt_domain:operation_error()}}.
commit(Version, Commit, WoodyCtx) ->
    BaseID = get_event_id(get_base_version(Version)),
    %% TODO in theory, it's enought ?BASE + 1 events here,
    %% but it's complicated and needs to be covered by tests
    HistoryRange = {BaseID, undefined, forward},
    Backend = get_backend(WoodyCtx),
    case machinery:call(?NS, ?ID, HistoryRange, {commit, Version, Commit}, Backend) of
        {ok, Response} ->
            Response;
        {error, notfound} ->
            ok = machinery:start(?NS, ?ID, undefined, Backend),
            commit(Version, Commit, WoodyCtx)
    end.

%%

-spec get_history_by_range(machinery:range(), woody_context:ctx()) -> st().
get_history_by_range(HistoryRange, WoodyCtx) ->
    Backend = get_backend(WoodyCtx),
    case machinery:get(?NS, ?ID, HistoryRange, Backend) of
        {ok, Machine} ->
            read_history(Machine);
        {error, notfound} ->
            ok = machinery:start(?NS, ?ID, undefined, Backend),
            get_history_by_range(HistoryRange, WoodyCtx)
    end.

-spec get_backend(woody_context:ctx()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    machinery_mg_backend:new(WoodyCtx, get_backend_config()).

-spec get_modernizer_backend(woody_context:ctx()) -> machinery_modernizer_mg_backend:backend().
get_modernizer_backend(WoodyCtx) ->
    machinery_modernizer_mg_backend:new(WoodyCtx, get_backend_config()).

get_backend_config() ->
    #{
        client => dmt_api_woody_utils:get_woody_client(automaton),
        schema => ?MODULE
    }.

%%

-spec init(args(_), machine(), handler_args(), handler_opts()) -> result().
init(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{}.

-spec process_call(args(Call), machine(), handler_args(), handler_opts()) ->
    {response(Response), result()}
when
    Call :: {commit, version(), commit()},
    Response ::
        {ok, snapshot()}
        | {error, version_not_found | head_mismatch | {operation_error, _Reason}}.
process_call(Args, #{namespace := ?NS, id := ?ID} = Machine, _HandlerArgs, _HandlerOpts) ->
    handle_call(Args, read_history(Machine));
process_call(_Call, Machine, _HandlerArgs, _HandlerOpts) ->
    process_unexpected_machine(Machine).

-spec process_timeout(machine(), handler_args(), handler_opts()) ->
    result().
process_timeout(#{namespace := ?NS, id := ?ID}, _HandlerArgs, _HandlerOpts) ->
    #{};
process_timeout(Machine, _HandlerArgs, _HandlerOpts) ->
    process_unexpected_machine(Machine).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) ->
    {error, noimpl}.
process_repair(_Args, #{namespace := ?NS, id := ?ID}, _HandlerArgs, _HandlerOpts) ->
    {error, noimpl};
process_repair(_Args, Machine, _HandlerArgs, _HandlerOpts) ->
    process_unexpected_machine(Machine).

-spec process_unexpected_machine(machine()) ->
    no_return().
process_unexpected_machine(#{namespace := NS, id := ID}) ->
    Message = genlib:format("Unexpected machine: ns = ~s, id = ~s", [NS, ID]),
    woody_error:raise(system, {internal, resource_unavailable, Message}).

%%

handle_call({commit, Version, Commit}, St) ->
    case squash_state(St) of
        {ok, #domain_conf_Snapshot{version = Version} = Snapshot} ->
            apply_commit(Snapshot, Commit);
        {ok, #domain_conf_Snapshot{version = V}} when V > Version ->
            % Is this retry? Maybe we already applied this commit.
            check_commit(Version, Commit, St);
        {ok, _} ->
            {{error, version_not_found}, #{}}
    end.

apply_commit(
    #domain_conf_Snapshot{version = VersionWas, domain = DomainWas},
    #domain_conf_Commit{ops = Ops} = Commit
) ->
    %% NOTE Actual timestamp of a produced event may differ since it
    %% is set by MG, but we require it now to construct latest
    %% snapshot. Thus, we must bear in mind that snapshot saved in
    %% event metadata always have timestamp originated by this service
    %% and not the actual commit event from the according machine's
    %% history.
    CreatedAt = current_timestamp(),
    case dmt_domain:apply_operations(Ops, DomainWas) of
        {ok, Domain} ->
            Snapshot = #domain_conf_Snapshot{version = VersionWas + 1, domain = Domain, created_at = CreatedAt},
            Event = make_event(Snapshot, Commit#domain_conf_Commit{created_at = CreatedAt}),
            {{ok, Snapshot}, #{events => [Event]}};
        {error, Reason} ->
            {{error, {operation_error, Reason}}, #{}}
    end.

check_commit(Version, #domain_conf_Commit{ops = Ops}, #st{snapshot = BaseSnapshot, history = History}) ->
    %% NOTE Match only 'ops' because timestamp will differ
    case maps:get(Version + 1, History) of
        #domain_conf_Commit{ops = Ops} ->
            % it's ok, commit already applied, lets return this snapshot
            {dmt_history:travel(Version + 1, History, BaseSnapshot), #{}};
        _ ->
            {{error, head_mismatch}, #{}}
    end.

-spec read_history(machine()) -> st().
read_history(#{history := Events}) ->
    lists:foldl(fun apply_event/2, #st{}, Events).

-spec apply_event(machinery:event(event()), st()) -> st().
apply_event({ID, CreatedAt, {commit, Commit0, Meta}}, #st{history = History} = St) ->
    Commit1 = Commit0#domain_conf_Commit{created_at = historical_timestamp(CreatedAt)},
    StNext = St#st{history = History#{ID => Commit1}},
    case Meta of
        #{snapshot := Snapshot} ->
            StNext#st{snapshot = Snapshot};
        #{} ->
            StNext
    end.

current_timestamp() ->
    genlib_rfc3339:format(erlang:system_time(microsecond), microsecond).

historical_timestamp(MachineEventCreatedAt) ->
    %% NOTE Reuse generic marshaling since timestamp is still same
    %% rfc3339 binary.
    machinery_mg_codec:marshal(timestamp, MachineEventCreatedAt).

squash_state(#st{snapshot = BaseSnapshot, history = History}) ->
    case dmt_history:head(History, BaseSnapshot) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, Error} ->
            error(Error)
    end.

-spec make_event(snapshot(), commit()) -> event().
make_event(Snapshot, Commit) ->
    Meta =
        case (Snapshot#domain_conf_Snapshot.version) rem ?BASE of
            0 ->
                #{snapshot => Snapshot};
            _ ->
                #{}
        end,
    {commit, Commit, Meta}.

%%

-spec modernize(woody_context:ctx()) -> ok.
modernize(WoodyCtx) ->
    machinery_modernizer:modernize(?NS, ?ID, get_modernizer_backend(WoodyCtx)).

-spec marshal(machinery_mg_schema:t(), machinery_mg_schema:v(_), machinery_mg_schema:context()) ->
    {machinery_msgpack:t(), machinery_mg_schema:context()}.
marshal({event, FmtVsn}, V, C) ->
    {encode_event_data(FmtVsn, V), C};
marshal({args, call}, V, C) ->
    {encode_call(V), C};
marshal({response, call}, V, C) ->
    {encode_call_result(V), C};
marshal(T, V, C) ->
    machinery_mg_schema_generic:marshal(T, V, C).

-spec unmarshal(machinery_mg_schema:t(), machinery_msgpack:t(), machinery_mg_schema:context()) ->
    {machinery_mg_schema:v(_), machinery_mg_schema:context()}.
unmarshal({event, FmtVsn}, V, C) ->
    {decode_event_data(FmtVsn, V), C};
unmarshal({args, call}, V, C) ->
    {decode_call(V), C};
unmarshal({response, call}, V, C) ->
    {decode_call_result(V), C};
unmarshal(T, V, C) ->
    machinery_mg_schema_generic:unmarshal(T, V, C).

%%

-spec get_version(machinery_mg_schema:vt()) -> machinery_mg_schema:version().
get_version(_) ->
    % NOTE
    % Current format version.
    % Be aware, this function is being mocked in the testsuite.
    2.

get_earliest_supported_version(_) ->
    2.

encode_event_data(FmtVsn, {commit, CommitIn, Meta}) ->
    % NOTE
    % Ensure that outdated commit won't sneak in.
    Commit = migrate(get_earliest_supported_version(event), get_version(event), commit, CommitIn),
    {arr, [{str, <<"commit">>}, encode(commit, Commit), encode_commit_meta(FmtVsn, Meta)]}.

encode_commit_meta(_FmtVsn, #{snapshot := SnapshotIn}) ->
    % NOTE
    % Ensure that outdated snapshot won't sneak in.
    Snapshot = migrate(get_earliest_supported_version(event), get_version(event), snapshot, SnapshotIn),
    {obj, #{{str, <<"snapshot">>} => encode(snapshot, Snapshot)}};
encode_commit_meta(_FmtVsn, #{}) ->
    {obj, #{}}.

decode_event_data(FmtVsn, {arr, [{str, <<"commit">>}, CommitEnc, Meta]}) when
    is_integer(FmtVsn), FmtVsn > 0
->
    Commit = migrate(FmtVsn, get_version(event), commit, decode(commit, CommitEnc)),
    {commit, Commit, decode_commit_meta(FmtVsn, Meta)}.

decode_commit_meta(FmtVsn, {obj, #{{str, <<"snapshot">>} := SnapshotEnc}}) ->
    Snapshot = migrate(FmtVsn, get_version(event), snapshot, decode(snapshot, SnapshotEnc)),
    #{snapshot => Snapshot};
decode_commit_meta(_FmtVsn, {obj, #{}}) ->
    #{}.

migrate(TargetVsn, TargetVsn, _Type, Data) ->
    % Nothing to migrate.
    Data;
migrate(Vsn, TargetVsn, Type, Data) when Vsn < TargetVsn ->
    Migrated = dmt_api_repository_migration:migrate(Vsn, Data),
    ok = validate(Type, Migrated),
    migrate(Vsn + 1, TargetVsn, Type, Migrated).

% NOTE
% Dialyzer correctly evaluates that this function will never be called.
% Left here for illustrative purposes. It will play its part next time fmtvsn is bumped.
-dialyzer({nowarn_function, [validate/2]}).
validate(T, V) ->
    _ = encode(T, V),
    ok.

%%

encode_call({commit, Version, Commit}) ->
    {arr, [{str, <<"commit">>}, {i, Version}, encode(commit, Commit)]}.

decode_call({arr, [{str, <<"commit">>}, {i, Version}, Commit]}) ->
    {commit, Version, decode(commit, Commit)}.

encode_call_result({ok, Snapshot}) ->
    {arr, [{str, <<"ok">>}, encode(snapshot, Snapshot)]};
encode_call_result({error, Reason}) ->
    {arr, [{str, <<"err">>}, {bin, term_to_binary(Reason)}]}.

decode_call_result({arr, [{str, <<"ok">>}, Snapshot]}) ->
    {ok, decode(snapshot, Snapshot)};
decode_call_result({arr, [{str, <<"err">>}, {bin, Reason}]}) ->
    {error, binary_to_term(Reason)}.

%%

encode(T, V) ->
    {bin, dmt_api_thrift_utils:encode(binary, get_type_info(T), V)}.

decode(T, {bin, V}) ->
    dmt_api_thrift_utils:decode(binary, get_type_info(T), V).

get_type_info(commit) ->
    {struct, struct, {dmsl_domain_conf_thrift, 'Commit'}};
get_type_info(snapshot) ->
    {struct, struct, {dmsl_domain_conf_thrift, 'Snapshot'}}.

get_base_version(V) when is_integer(V) andalso V >= ?BASE ->
    (V div ?BASE) * ?BASE - 1;
get_base_version(V) when is_integer(V) ->
    0.

get_event_id(ID) when is_integer(ID) andalso ID > 0 ->
    ID;
get_event_id(0) ->
    undefined.
