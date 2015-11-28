%%%-------------------------------------------------------------------
%%% @author Michal Ptaszek <michal.ptaszek@erlang-solutions.com>
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Event-based XML parser
%%% @end
%%%-------------------------------------------------------------------
-module(exml_event).

-include("exml_event.hrl").

-export([new_parser/0,
         reset_parser/1,
         free_parser/1,
         parse/2,
         parse_final/2]).

-export_type([c_parser/0]).

-on_load(load/0).

-spec load() -> any().
load() ->
    %% force loading of native extensions
    {module, _} = code:ensure_loaded(rxml_native),
    ok.

-opaque c_parser() :: binary().

-spec new_parser() -> {ok, c_parser()}.
new_parser() ->
    rxml_native:new_parser().

-spec reset_parser(c_parser()) -> ok.
reset_parser(Parser) ->
    rxml_native:reset_parser(Parser).

-spec free_parser(c_parser()) -> ok.
free_parser(Parser) ->
    rxml_native:free_parser(Parser).

-spec parse(c_parser(), binary()) -> {ok, list()} | {error, string()}.
parse(Parser, Data) ->
    do_parse(Parser, Data, 0).

-spec parse_final(c_parser(), binary()) -> {ok, list()} | {error, string()}.
parse_final(Parser, Data) ->
    do_parse(Parser, Data, 1).

-spec do_parse(c_parser(), binary(), 0 | 1) -> {ok, list()} | {error, atom()}.
do_parse(Parser, Data, Final) ->
    try
        {ok, rxml_native:parse_nif(Parser, Data, Final)}
    catch
        error:R -> {error, R}
    end.
