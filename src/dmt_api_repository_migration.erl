-module(dmt_api_repository_migration).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

-export([migrate/2]).

-type snapshot() :: dmt_api_repository:snapshot().
-type commit() :: dmt_api_repository:commit().
-type version() :: dmt_api_repository:version().

-spec migrate
    (version(), commit()) -> commit();
    (version(), snapshot()) -> snapshot().
migrate(1, Data) ->
    migrate_legacy_payment_system_conditions(Data).

migrate_legacy_payment_system_conditions(Data) ->
    % TODO OPS-161
    % Relevant for https://github.com/valitydev/damsel/commit/09e7a75, revert this commit after
    % complete migration.
    % Replaces any `BankCardCondition` in a commit or snapshot which represents legacy payment
    % system condition (i.e. one that compares against an atom in % `LegacyBankCardPaymentSystem`
    % enum) with an empty `BankCardCondition`. Obviously, this incurs information loss yet
    % significantly easier than proper mapping. Two reasons why this looks valid:
    %  - Production domain config do not contain legacy conditions anywhere in the last 3 months
    %    of snapshots.
    %  - https://github.com/valitydev/party-management since at least bb4549c won't handle them
    %    anyway.
    genlib_ted:run(
        fun
            (Condition = #domain_BankCardCondition{}, _Annos) ->
                {replace, migrate_legacy_payment_system_condition(Condition)};
            (_, _Annos) ->
                proceed
        end,
        Data
    ).

migrate_legacy_payment_system_condition(Condition) ->
    case Condition#domain_BankCardCondition.definition of
        {payment_system_is, Legacy} ->
            true = is_atom(Legacy),
            Condition#domain_BankCardCondition{definition = undefined};
        {_, _} ->
            Condition;
        undefined ->
            Condition
    end.
