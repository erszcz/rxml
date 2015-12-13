%% See assertEqual in $ERLANG/lib/stdlib-2.6/include/assert.hrl for the original.
-define(exmlAssertEqual(Example, Expr),
        begin
            ((fun (__X, __V) ->
                      case __V of
                          __X -> ok;
                          __V -> erlang:error({exmlAssertEqual,
                                               [{module, ?MODULE},
                                                {line, ?LINE},
                                                {expression, (??Expr)},
                                                {expected, __X},
                                                {value, __V}]})
                      end
              end)(xml_sort((Example)), xml_sort((Expr))))
        end).

xml_sort({xmlcdata, _} = CData) -> CData;
xml_sort(#xmlstreamstart{attrs = Attrs} = StreamStart) ->
    StreamStart#xmlstreamstart{attrs = lists:sort(Attrs)};
xml_sort(#xmlel{} = El) ->
    #xmlel{attrs = Attrs, children = Children} = El,
    El#xmlel{attrs = lists:sort(Attrs),
             children = [ xml_sort(C) || C <- Children ]}.