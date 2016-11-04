dbg:tracer().
dbg:p(all, call).
dbg:tpl(exml_stream, parse_events, x).
{ok, P0} = exml_stream:new_parser().
{ok, P1, E1} = exml_stream:parse(P0, <<"<stream-start">>).
{ok, P2, E2} = exml_stream:parse(P1, <<"><inside-stream>">>).
{ok, P3, E3} = exml_stream:parse(P2, <<"<c/></inside-stream></stream-start>">>).
{E1, E2, E3}.

dbg:tracer().
dbg:p(all, call).
dbg:tpl(exml_stream, parse_events, x).
{ok, P0} = exml_stream:new_parser().
{ok, P1, E1} = exml_stream:parse(P0, <<"<stream-start xmlns:s='test-namespace'">>).
{ok, P2, E2} = exml_stream:parse(P1, <<"><inside-stream>">>).
{ok, P3, E3} = exml_stream:parse(P2, <<"<s:c/></inside-stream></stream-start>">>).
{E1, E2, E3}.

rd(xmlel, {name, attrs, children}).
dbg:tracer().
dbg:p(all, call).
dbg:tpl(exml_stream, parse_events, x).
{ok, Parser0} = exml_stream:new_parser().
{ok, Parser1, Empty0} =
    exml_stream:parse(Parser0, <<"<stream:stream xmlns:stream='http://etherx.jabber.org/streams' version='1.0'">>).
{ok, Parser2, StreamStart} =
    exml_stream:parse(Parser1, <<" to='i.am.banana.com' xml:lang='en'><auth">>).
{ok, Parser3, Auth} = exml_stream:parse(Parser2, <<" mechanism='DIGEST-MD5'/>">>).
{ok, Parser4, Empty1} = exml_stream:parse(Parser3, <<"<stream:features><bind xmlns='some_ns'">>).
{ok, Parser5, Empty2} = exml_stream:parse(Parser4, <<"/><session xmlns='some_other'/>This is ">>).
{ok, Parser6, Features} = exml_stream:parse(Parser5, <<"some CData</stream:features>">>).
[#xmlel{children=[_, _, CData]}] = Features.
{Empty0, StreamStart, Auth, Empty1, Empty2, Features, CData}.
