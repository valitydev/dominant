-module(dmt_api_repository_v3).
-behaviour(dmt_api_repository).

-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

-define(NS  , <<"domain-config">>).
-define(ID  , <<"primary/v3">>).
-define(BASE, 10).

%% API

-export([checkout/2]).
-export([pull/2]).
-export([commit/3]).

%% State processor

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

%%
-record(st, {
    snapshot = #'Snapshot'{version = 0, domain = dmt_domain:new()} :: snapshot(),
    history = #{} :: dmt_api_repository:history()
}).

-type st()              :: #st{}.
-type context()         :: woody_context:ctx().
-type history_range()   :: mg_proto_state_processing_thrift:'HistoryRange'().
-type machine()         :: mg_proto_state_processing_thrift:'Machine'().
-type history()         :: mg_proto_state_processing_thrift:'History'().

-type ref()             :: dmsl_domain_config_thrift:'Reference'().
-type snapshot()        :: dmt_api_repository:snapshot().
-type commit()          :: dmt_api_repository:commit().

-spec checkout(ref(), context()) ->
    {ok, snapshot()} |
    {error, version_not_found}.

checkout({head, #'Head'{}}, Context) ->
    HistoryRange = #mg_stateproc_HistoryRange{
        'after' = undefined,
        'limit' = ?BASE,
        'direction' = backward
    },
    case get_history_by_range(HistoryRange, Context) of
        #st{} = St ->
            squash_state(St);
        {error, version_not_found} ->
            {error, version_not_found}
    end;
checkout({version, V}, Context) ->
    BaseV = get_base_version(V),
    HistoryRange = #mg_stateproc_HistoryRange{
        'after' = get_event_id(BaseV),
        'limit' = V - BaseV,
        'direction' = forward
    },
    case get_history_by_range(HistoryRange, Context) of
        #st{} = St ->
            case squash_state(St) of
                {ok, #'Snapshot'{version = V}} = Result ->
                    Result;
                {ok, _} ->
                    {error, version_not_found}
            end;
        {error, version_not_found} ->
            {error, version_not_found}
    end.

-spec pull(dmt_api_repository:version(), context()) ->
    {ok, dmt_api_repository:history()} |
    {error, version_not_found}.

pull(Version, Context) ->
    After = get_event_id(Version),
    case get_history_by_range(#mg_stateproc_HistoryRange{'after' = After}, Context) of
        #st{history = History} ->
            {ok, History};
        {error, version_not_found} ->
            {error, version_not_found}
    end.

-spec commit(dmt_api_repository:version(), commit(), context()) ->
    {ok, snapshot()} |
    {error, version_not_found | {operation_conflict, dmt_api_repository:operation_conflict()}}.

commit(Version, Commit, Context) ->
    BaseID = get_event_id(get_base_version(Version)),
    decode_call_result(dmt_api_automaton_client:call(
        ?NS,
        ?ID,
        #mg_stateproc_HistoryRange{'after' = BaseID},
        encode_call({commit, Version, Commit}),
        Context
    )).

%%

-spec get_history_by_range(history_range(), context()) ->
    st() | {error, version_not_found}.

get_history_by_range(HistoryRange, Context) ->
    case dmt_api_automaton_client:get_history(?NS, ?ID, HistoryRange, Context) of
        {ok, History} ->
            read_history(History);
        {error, #mg_stateproc_MachineNotFound{}} ->
            ok = dmt_api_automaton_client:start(?NS, ?ID, Context),
            get_history_by_range(HistoryRange, Context);
        {error, #mg_stateproc_EventNotFound{}} ->
            {error, version_not_found}
    end.

%%

-define(NIL, {nl, #mg_msgpack_Nil{}}).

-spec handle_function(woody:func(), woody:args(), context(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function('ProcessCall', [#mg_stateproc_CallArgs{arg = Payload, machine = Machine}], Context, _Opts) ->
    Call = decode_call(Payload),
    {Result, Events} = handle_call(Call, read_history(Machine), Context),
    {ok, construct_call_result(Result, Events)};
handle_function(
    'ProcessSignal',
    [#mg_stateproc_SignalArgs{
        signal = {init, #mg_stateproc_InitSignal{}}
    }],
    Context,
    _Opts
) ->
    %%% TODO It's generally prettier to make up a _migrating_ repository which is the special repository
    %%%      module designed to facilitate migrations between some preconfigured 'old' repository backend
    %%%      and some 'new' one. The migration process could be triggered by the very first mutating
    %%%      operation (e.g. commit) going into this backend for example.
    Events = get_events_from_legacy(Context),
    {ok, construct_signal_result(Events)}.

construct_call_result(Response, Events) ->
    #mg_stateproc_CallResult{
        response = encode_call_result(Response),
        change = #mg_stateproc_MachineStateChange{aux_state = ?NIL, events = encode_events(Events)},
        action = #mg_stateproc_ComplexAction{}
    }.

construct_signal_result(Events) ->
    #mg_stateproc_SignalResult{
        change = #mg_stateproc_MachineStateChange{aux_state = ?NIL, events = encode_events(Events)},
        action = #mg_stateproc_ComplexAction{}
    }.

encode_events(Events) ->
    [encode_event(E) || E <- Events].

%%

handle_call({commit, Version, Commit}, St, _Context) ->
    case squash_state(St) of
        {ok, #'Snapshot'{version = Version} = Snapshot} ->
            apply_commit(Snapshot, Commit);
        {ok, _} ->
            {{error, head_mismatch}, []}
    end.

apply_commit(#'Snapshot'{version = VersionWas, domain = DomainWas}, #'Commit'{ops = Ops} = Commit) ->
    case dmt_domain:apply_operations(Ops, DomainWas) of
        {ok, Domain} ->
            Snapshot = #'Snapshot'{version = VersionWas + 1, domain = Domain},
            {{ok, Snapshot}, [make_event(Snapshot, Commit)]};
        {error, Reason} ->
            {{error, {operation_conflict, Reason}}, []}
    end.

%%

-spec read_history(machine() | history()) ->
    st().

read_history(#mg_stateproc_Machine{history = Events}) ->
    read_history(Events);
read_history(Events) ->
    read_history(Events, #st{}).

-spec read_history([mg_proto_state_processing_thrift:'Event'()],  st()) ->
    st().

read_history([], St) ->
    St;
read_history([#mg_stateproc_Event{id = Id, event_payload = EventData} | Rest], #st{history = History} = St) ->
    {commit, Commit, Meta} = decode_event(EventData),
    case Meta of
        #{snapshot := Snapshot} ->
            read_history(
                Rest,
                St#st{snapshot = Snapshot, history = History#{Id => Commit}}
            );
        #{} ->
            read_history(
                Rest,
                St#st{history = History#{Id => Commit}}
            )
    end.

squash_state(#st{snapshot = BaseSnapshot, history = History}) ->
    case dmt_history:head(History, BaseSnapshot) of
        {ok, Snapshot} ->
            {ok, Snapshot};
        {error, Error} ->
            error(Error)
    end.

%%

get_events_from_legacy(Context) ->
    LegacyHistory = dmt_api_repository_v2:get_history(undefined, Context),
    History = lists:keysort(1, maps:to_list(LegacyHistory)),
    convert_v2_events(History, #'Snapshot'{version = 0, domain = dmt_domain:new()}, []).

convert_v2_events([{Version1, Commit} | Others], #'Snapshot'{version = Version0} = Snapshot0, Events)
    when Version0 + 1 == Version1 % paranoidal check :)
->
    case apply_commit(Snapshot0, Commit) of
        {{ok, Snapshot}, [Event]} ->
            convert_v2_events(Others, Snapshot, [Event | Events]);
        {{error, Error}, []} ->
            error(Error)
    end;
convert_v2_events([], _, Events) ->
    lists:reverse(Events).

make_event(Snapshot, Commit) ->
    Meta = case (Snapshot#'Snapshot'.version) rem ?BASE of
        0 ->
            #{snapshot => Snapshot};
        _ ->
            #{}
    end,
    {commit, Commit, Meta}.

encode_event({commit, Commit, Meta}) ->
    {arr, [{str, <<"commit">>}, encode(commit, Commit), encode_commit_meta(Meta)]}.

encode_commit_meta(#{snapshot := Snapshot}) ->
    {obj, #{{str, <<"snapshot">>} => encode(snapshot, Snapshot)}};
encode_commit_meta(#{}) ->
    {obj, #{}}.

decode_event({arr, [{str, <<"commit">>}, Commit, Meta]}) ->
    {commit, decode(commit, Commit), decode_commit_meta(Meta)}.

decode_commit_meta({obj, #{{str, <<"snapshot">>} := Snapshot}}) ->
    #{snapshot => decode(snapshot, Snapshot)};
decode_commit_meta({obj, #{}}) ->
    #{}.

%%

encode_call({commit, Version, Commit}) ->
    {arr, [{str, <<"commit">>}, {i, Version}, encode(commit, Commit)]}.

decode_call({arr, [{str, <<"commit">>}, {i, Version}, Commit]}) ->
    {commit, Version, decode(commit, Commit)}.

encode_call_result({ok, Snapshot}) ->
    {arr, [{str, <<"ok">> }, encode(snapshot, Snapshot)]};
encode_call_result({error, Reason}) ->
    {arr, [{str, <<"err">>}, {bin, term_to_binary(Reason)}]}.

decode_call_result({arr, [{str, <<"ok">> }, Snapshot]}) ->
    {ok, decode(snapshot, Snapshot)};
decode_call_result({arr, [{str, <<"err">>}, {bin, Reason}]}) ->
    {error, binary_to_term(Reason)}.

%%

decode(T, V) ->
    dmt_api_thrift_utils:decode(msgpack, get_type_info(T), V).

encode(T, V) ->
    dmt_api_thrift_utils:encode(msgpack, get_type_info(T), V).

get_type_info(commit) ->
    {struct, struct, {dmsl_domain_config_thrift, 'Commit'}};
get_type_info(snapshot) ->
    {struct, struct, {dmsl_domain_config_thrift, 'Snapshot'}}.

get_base_version(V) when is_integer(V) andalso V >= ?BASE ->
    (V div ?BASE) * ?BASE - 1;
get_base_version(V) when is_integer(V) ->
    0.

get_event_id(ID) when is_integer(ID) andalso ID > 0 ->
    ID;
get_event_id(0) ->
    undefined.