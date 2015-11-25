-module(rxml_native_tests).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

native_lib_loads_test() ->
    ?assertEqual(ok, rxml_native:test()).
