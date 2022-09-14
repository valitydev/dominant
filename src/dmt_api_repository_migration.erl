-module(dmt_api_repository_migration).

-export([migrate/2]).

-type snapshot() :: dmt_api_repository:snapshot().
-type commit() :: dmt_api_repository:commit().
-type version() :: dmt_api_repository:version().

-spec migrate
    (version(), commit()) -> no_return();
    (version(), snapshot()) -> no_return().
migrate(1, _Data) ->
    % NOTE
    % Migration of version 1 events is not possible anymore since
    % https://github.com/valitydev/damsel/commit/4e82c243
    error(noimpl).
