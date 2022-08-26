-module(dmt_ct_helper).

-include_lib("damsel/include/dmsl_domain_conf_thrift.hrl").

-export([mk_insert_commit/1]).
-export([mk_update_commit/2]).
-export([mk_remove_commit/1]).

-export([next_id/0]).

-type object() :: dmsl_domain_thrift:'DomainObject'().
-type commit() :: dmt_api_repository:commit().

-spec mk_insert_commit(object() | [object()]) -> commit().
mk_insert_commit(Objects) when is_list(Objects) ->
    #domain_conf_Commit{
        ops = [{insert, #domain_conf_InsertOp{object = Object}} || Object <- Objects]
    };
mk_insert_commit(Object) when is_tuple(Object) ->
    mk_insert_commit([Object]).

-spec mk_update_commit(_Old :: object(), _New :: object()) -> commit().
mk_update_commit(Old, New) when is_tuple(Old), is_tuple(New) ->
    #domain_conf_Commit{
        ops = [{update, #domain_conf_UpdateOp{old_object = Old, new_object = New}}]
    }.

-spec mk_remove_commit(object() | [object()]) -> commit().
mk_remove_commit(Objects) when is_list(Objects) ->
    #domain_conf_Commit{
        ops = [{remove, #domain_conf_RemoveOp{object = Object}} || Object <- Objects]
    };
mk_remove_commit(Object) when is_tuple(Object) ->
    mk_remove_commit([Object]).

%%

-spec next_id() -> 0..16#7FFFFFFF.
next_id() ->
    16#7FFFFFFF band
        (erlang:system_time(millisecond) * 1000 +
            erlang:unique_integer([positive, monotonic])).
