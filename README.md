# rxml

**rxml** is a fork of esl/exml which uses Rust for NIFs (native-implemented
functions) and uses an XML parser written completely in Rust.
Say bye-bye to C and Expat.

## ToDo

To some extent in order of importance:

- [ ] prepare a branch / fork of MongooseIM which depends on this library
      for quick/convenient development iterations
- [ ] load test and compare with C/Expat-based `esl/exml`
- [ ] use [Rustler](https://github.com/hansihe/Rustler) - higher-level
      Erlang/Rust interface for maintainability
- [ ] use the [ElementBuilder](https://docs.babelmonkeys.de/RustyXML/xml/struct.ElementBuilder.html)
      interface of RustyXML instead of the event based interface and completely
      get rid of `src/exml_event.erl` for maintainability
- [ ] use [rebar3_rust](https://github.com/sdwolf/rebar3_rust) for building
- [ ] clean up: remove `c_src` completely
- [ ] verify CData / attribute escaping differences across `esl/exml`,
      RustyXML, and the XMPP / XML standards - exml and RustyXML
      (un)escape differently, but does it matter?

exml
====

[![Build Status](https://secure.travis-ci.org/esl/exml.png)](http://travis-ci.org/esl/exml)

**exml** is an Erlang library helpful with parsing XML streams
and doing some basic XML structures manipulation.

Building
========

**exml** is a rebar-compatible OTP application, run `make` or
`./rebar compile` in order to build it.

As a requirement, development headers for expat library are
required.

Using
=====

**exml** can parse both XML streams as well as single XML
documents at once.

To parse a whole XML document:

```erlang
{ok, Parser} = exml:parse(<<"<my_xml_doc/>">>).
```

To generate an XML document from Erlang terms:

```erlang
El = #xmlel{name = <<"foo">>,
            attrs = [{<<"attr1">>, <<"bar">>}],
            children = [{xmlcdata, <<"Some Value">>}]},
exml:to_list(El).
```

or (pastable into `erl` shell):

```erlang
El = {xmlel, <<"foo">>,
      [{<<"attr1">>, <<"bar">>}],
      [{xmlcdata, <<"Some Value">>}]}.
exml:to_list(El).
```

Which results in:

```xml
<foo attr1='bar'>Some Value</foo>
```

`exml:to_binary/1` works similarly.

There're also `exml:to_pretty_iolist/1,3` for a quick'n'dirty document
preview (pastable into `erl`):

```erlang
rr("include/exml.hrl").
El = #xmlel{name = <<"outer">>,
            attrs = [{<<"attr1">>, <<"val1">>},
                     {<<"attr2">>, <<"val-two">>}],
            children = [#xmlel{name = <<"inner-childless">>},
                        #xmlel{name = <<"inner-w-children">>,
                               children = [#xmlel{name = <<"a">>}]}]}.
io:format("~s", [exml:to_pretty_iolist(El)]).
```

which prints:

```xml
<outer attr2='val-two' attr1='val1'>
  <inner-childless/>
  <inner-w-children>
    <a/>
  </inner-w-children>
</outer>
```

For an example of using the streaming API see `test/exml_stream_tests.erl`.
