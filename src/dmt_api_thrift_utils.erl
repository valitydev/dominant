-module(dmt_api_thrift_utils).

-export([encode/3]).
-export([decode/3]).

-type thrift_type() :: term().
-type thrift_value() :: term().

-spec encode(binary, thrift_type(), thrift_value()) -> binary().
encode(binary, Type, Value) ->
    Codec0 = thrift_strict_binary_codec:new(),
    {ok, Codec} = thrift_strict_binary_codec:write(Codec0, Type, Value),
    thrift_strict_binary_codec:close(Codec).

-spec decode(binary, thrift_type(), binary()) -> thrift_value().
decode(binary, Type, Data) ->
    Codec = thrift_strict_binary_codec:new(Data),
    {ok, Value, Leftovers} = thrift_strict_binary_codec:read(Codec, Type),
    <<>> = thrift_strict_binary_codec:close(Leftovers),
    Value.
