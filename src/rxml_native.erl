-module(rxml_native).

%% exml.erl
-export([escape_cdata_nif/1,
         unescape_cdata_nif/1,
         escape_attr_nif/1,
         unescape_attr_nif/1]).

%% exml_event.erl
-export([new_parser/0,
         reset_parser/1,
         free_parser/1,
         parse_nif/3]).

%% debug Rust parser state
-export([debug/1]).

-on_load(load/0).

-spec load() -> ok | {error, {atom(), string()}}.
load() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, "librxml"), none).

%%
%% exml.erl
%%

-spec escape_cdata_nif(iodata()) -> binary().
escape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_cdata_nif(iodata()) -> binary().
unescape_cdata_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec escape_attr_nif(binary()) -> binary().
escape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec unescape_attr_nif(binary()) -> binary().
unescape_attr_nif(_Data) ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

%%
%% exml_event.erl
%%

-spec new_parser() -> {ok, exml_event:c_parser()}.
new_parser() ->
    erlang:nif_error({?MODULE, nif_not_loaded}).

-spec reset_parser(exml_event:c_parser()) -> ok.
reset_parser(Parser) ->
    erlang:nif_error({?MODULE, nif_not_loaded}, [Parser]).

-spec free_parser(exml_event:c_parser()) -> ok.
free_parser(Parser) ->
    erlang:nif_error({?MODULE, nif_not_loaded}, [Parser]).

%% Throws exceptions on errors.
-spec parse_nif(exml_event:c_parser(), binary(), integer()) -> Result when
      Result :: {ok, list()}.
parse_nif(Parser, Data, Final) ->
    erlang:nif_error(not_loaded, [Parser, Data, Final]).

-spec debug(exml_event:c_parser()) -> ok.
debug(Parser) ->
    erlang:nif_error({?MODULE, nif_not_loaded}, [Parser]).
