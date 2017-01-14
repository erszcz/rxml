-module(exml_stream_tests).

-include("exml_stream.hrl").
-include("exml_test.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

-compile(export_all).

basic_parse_no_namespaces_test() ->
    {ok, P0} = exml_stream:new_parser(),
    {ok, P1, E1} = exml_stream:parse(P0, <<"<stream-start">>),
    ?ae([], E1),
    {ok, P2, E2} = exml_stream:parse(P1, <<"><inside-stream>">>),
    ?ae([{xmlstreamstart,<<"stream-start">>,[]}], E2),
    {ok, P3, E3} = exml_stream:parse(P2, <<"<c/></inside-stream></stream-start>">>),
    ?ae([{xmlel,<<"inside-stream">>,[],[{xmlel,<<"c">>,[],[]}]},
         {xmlstreamend,<<"stream-start">>}],
        E3).

basic_parse_with_namespaces_test() ->
    {ok, P0} = exml_stream:new_parser(),
    {ok, P1, E1} = exml_stream:parse(P0, <<"<stream-start xmlns:s='test-namespace'">>),
    ?ae([], E1),
    {ok, P2, E2} = exml_stream:parse(P1, <<"><inside-stream>">>),
    ?ae([{xmlstreamstart,<<"stream-start">>,
          [{<<"xmlns:s">>,<<"test-namespace">>}]}], E2),
    {ok, P3, E3} = exml_stream:parse(P2, <<"<s:c/></inside-stream></stream-start>">>),
    ?ae([{xmlel,<<"inside-stream">>,[],[{xmlel,<<"s:c">>,[],[]}]},
         {xmlstreamend,<<"stream-start">>}],
        E3).

basic_parse_xmpp_stream_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Empty0} =
        exml_stream:parse(Parser0, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0'">>),
    ?assertEqual([], Empty0),
    {ok, Parser2, StreamStart} =
        exml_stream:parse(Parser1, <<" to='i.am.banana.com' xml:lang='en'><auth">>),
    ?exmlAssertEqual(
       [#xmlstreamstart{name = <<"stream:stream">>,
                        attrs = [{<<"xmlns:stream">>, <<"http://etherx.jabber.org/streams">>},
                                 {<<"version">>, <<"1.0">>},
                                 {<<"to">>, <<"i.am.banana.com">>},
                                 {<<"xml:lang">>, <<"en">>}]}],
       StreamStart),
    {ok, Parser3, Auth} = exml_stream:parse(Parser2, <<" mechanism='DIGEST-MD5'/>">>),
    ?assertEqual(
       [#xmlel{name = <<"auth">>, attrs = [{<<"mechanism">>, <<"DIGEST-MD5">>}]}],
       Auth),
    {ok, Parser4, Empty1} = exml_stream:parse(Parser3, <<"<stream:features><bind xmlns='some_ns'">>),
    ?assertEqual([], Empty1),
    {ok, Parser5, Empty2} = exml_stream:parse(Parser4, <<"/><session xmlns='some_other'/>This is ">>),
    ?assertEqual([], Empty2),
    {ok, Parser6, Features} = exml_stream:parse(Parser5, <<"some CData</stream:features>">>),
    %% TODO: uncomment and make sure it works
    %?assertMatch(
    %   [#xmlel{name = <<"stream:features">>,
    %           children = [#xmlel{name = <<"bind">>,
    %                              attrs = [{<<"xmlns">>, <<"some_ns">>}]},
    %                       #xmlel{name = <<"session">>,
    %                              attrs = [{<<"xmlns">>, <<"some_other">>}]},
    %                       _CData]}],
    %   Features),
    [#xmlel{children=[_, _, CData]}] = Features,
    ?assertEqual(<<"This is some CData">>, exml:unescape_cdata(CData)).
    %?assertEqual(ok, exml_stream:free_parser(Parser6)).

bosh_stream_restart_test() ->
    %% Line wrapping / double quotes here are deliberate - don't introduce extra whitespace.
    BOSHRestart = <<"<body rid='1478920449405923' xmlns='http://jabber.org/protocol/httpbind' "
                          "sid='36e8ab8ba312e27601c7039f97 ",
                          "681c3b45c9bec2' xmlns:xmpp='urn:xmpp:xbosh' xml:lang='en' "
                          "to='localhost' xmpp:restart='true'/>">>,
    {ok, Element} = exml:parse(BOSHRestart),
    io:format("expected: ~p\n", [xml_sort({xmlel,<<"body">>,
                                           [{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},
                                            {<<"xmlns:xmpp">>,<<"urn:xmpp:xbosh">>},
                                            {<<"rid">>,<<"1478920449405923">>},
                                            {<<"sid">>,<<"36e8ab8ba312e27601c7039f97 681c3b45c9bec2">>},
                                            {<<"xml:lang">>,<<"en">>},
                                            {<<"to">>,<<"localhost">>},
                                            {<<"xmpp:restart">>,<<"true">>}],
                                           []})]),
    io:format("actual: ~p\n", [xml_sort(Element)]),
    ?exmlAssertEqual({xmlel,<<"body">>,
                      [{<<"xmlns">>,<<"http://jabber.org/protocol/httpbind">>},
                       {<<"xmlns:xmpp">>,<<"urn:xmpp:xbosh">>},
                       {<<"rid">>,<<"1478920449405923">>},
                       {<<"sid">>,<<"36e8ab8ba312e27601c7039f97 681c3b45c9bec2">>},
                       {<<"xml:lang">>,<<"en">>},
                       {<<"to">>,<<"localhost">>},
                       {<<"xmpp:restart">>,<<"true">>}],
                      []}, Element).

-define(BANANA_STREAM, <<"<stream:stream xmlns:stream='something'><foo attr='bar'>I am a banana!<baz/></foo></stream:stream>">>).
-define(assertIsBanana(Elements),
        (fun() -> % fun instead of begin/end because we bind CData in unhygenic macro
                 %% this is a poor man's assertMatch
                 [#xmlstreamstart{name = <<"stream:stream">>,
                                  attrs = [{<<"xmlns:stream">>, <<"something">>}]},
                  #xmlel{name = <<"foo">>,
                         attrs = [{<<"attr">>, <<"bar">>}],
                         children = [CData, #xmlel{name = <<"baz">>}]},
                  #xmlstreamend{name = <<"stream:stream">>}] = xml_sort(Elements),
                 ?assertEqual(<<"I am a banana!">>, exml:unescape_cdata(CData)),
                 Elements
         end)()).

conv_test() ->
    AssertParses = fun(Input) ->
                           {ok, Parser0} = exml_stream:new_parser(),
                           {ok, Parser1, Elements} = exml_stream:parse(Parser0, Input),
                           ok = exml_stream:free_parser(Parser1),
                           ?assertIsBanana(Elements)
                   end,
    Elements = AssertParses(?BANANA_STREAM),
    AssertParses(exml:to_binary(Elements)),
    AssertParses(list_to_binary(exml:to_list(Elements))),
    AssertParses(list_to_binary(exml:to_iolist(Elements))).

stream_reopen_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Elements1} = exml_stream:parse(Parser0, ?BANANA_STREAM),
    ?assertIsBanana(Elements1),
    {ok, Parser2} = exml_stream:reset_parser(Parser1),
    {ok, Parser3, Elements2} = exml_stream:parse(Parser2, ?BANANA_STREAM),
    ?assertIsBanana(Elements2),
    ok = exml_stream:free_parser(Parser3).

infinit_framed_stream_test() ->
    {ok, Parser0} = exml_stream:new_parser([{infinite_stream, true},
                                            {autoreset, true}]),
    Els = [
           #xmlel{name = <<"open">>,
                  attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-framing">>},
                           {<<"to">>, <<"example.com">>},
                           {<<"version">>, <<"1.0">>}]},
           #xmlel{name = <<"foo">>},
           #xmlel{name = <<"message">>,
                  attrs = [{<<"to">>, <<"ala@example.com">>}],
                  children = [#xmlel{name = <<"body">>,
                                     children = [#xmlcdata{content = <<"Hi, How Are You?">>}]}]}
          ],
    lists:foldl(fun(#xmlel{name = Name} = Elem, Parser) ->
        Bin = exml:to_binary(Elem),
        {ok, Parser1, [Element]} = exml_stream:parse(Parser, Bin), %% matches to one element list
        #xmlel{ name = Name} = Element, %% checks if returned is xmlel of given name
        Parser1
    end, Parser0, Els).

parse_error_test() ->
    {ok, Parser0} = exml_stream:new_parser(),
    Input = <<"top-level non-tag<tag-start">>,
    ?assertEqual({error, {cdata_outside_outer_tag, Input}},
                 exml_stream:parse(Parser0, Input)),
    ok = exml_stream:free_parser(Parser0).

assert_parses_escape_cdata(Text) ->
    Escaped = exml:escape_cdata(Text),
    Tag = #xmlel{name = <<"tag">>, children=[Escaped]},
    Stream = [#xmlstreamstart{name = <<"s">>}, Tag, #xmlstreamend{name = <<"s">>}],
    {ok, Parser0} = exml_stream:new_parser(),
    {ok, Parser1, Elements} = exml_stream:parse(Parser0, exml:to_binary(Stream)),
    ?assertMatch([#xmlstreamstart{name = <<"s">>},
                  #xmlel{name = <<"tag">>, children=[_CData]},
                  #xmlstreamend{name = <<"s">>}],
                 Elements),
    [_, #xmlel{children=[CData]}, _] = Elements,
    ?assertEqual(Text, exml:unescape_cdata(CData)),
    ok = exml_stream:free_parser(Parser1).

cdata_test() ->
    assert_parses_escape_cdata(<<"I am a banana!">>),
    assert_parses_escape_cdata(<<"]:-> ]]> >">>),
    assert_parses_escape_cdata(<<"><tag">>),
    assert_parses_escape_cdata(<<"<!--">>),
    assert_parses_escape_cdata(<<"<![CDATA[ test">>).

-define(ATTR_TEST_STREAM, <<"<stream:stream xmlns:stream='something'><quote attr=\"&amp;&lt;&gt;&quot;&apos;&#xA;&#x9;&#xD;\"/></stream:stream>">>).

conv_attr_test() ->
    AssertParses = fun(Input) ->
                           {ok, Parser0} = exml_stream:new_parser(),
                           {ok, Parser1, Elements} = exml_stream:parse(Parser0, Input),
                           ok = exml_stream:free_parser(Parser1),
                           ?assertMatch([_, #xmlel{attrs = [{<<"attr">>, <<"&<>\"'\n\t\r">>}]} | _],
                                                   Elements),
                           Elements
                   end,
    Elements = AssertParses(?ATTR_TEST_STREAM),
    AssertParses(exml:to_binary(Elements)),
    AssertParses(list_to_binary(exml:to_list(Elements))),
    AssertParses(list_to_binary(exml:to_iolist(Elements))).

