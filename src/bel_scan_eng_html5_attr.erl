-module(bel_scan_eng_html5_attr).
-behaviour(bel_scan_eng).

% bel_scan_eng callbacks
-export([ init/1
        , handle_start/2
        , handle_text/2
        , handle_match/2
        , handle_terminate/2
        ]).

-include("bel_scan_eng.hrl").

%%%=====================================================================
%%% bel_scan_eng callbacks
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = attribute,
                re = <<
                    "(\\w+)=\\\"(.*?[^\\\\\"])\\\"" "|"
                    "(\\w+)='(.*?[^\\\\'])'" "|"
                    "(\\w+)"
                >>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {halt, State}.

handle_match({?MODULE, attribute, Text, [<<>>, <<>>, <<>>, <<>>, K], Loc}, State) ->
    Token = bel_scan:token(attribute, Text, K, Loc),
    {reply, [Token], State};
handle_match({?MODULE, attribute, Text, [<<>>, <<>>, K, V], Loc}, State) ->
    Token = bel_scan:token(attribute, Text, {K, V}, Loc),
    {reply, [Token], State};
handle_match({?MODULE, attribute, Text, [K, V], Loc}, State) ->
    Token = bel_scan:token(attribute, Text, {K, V}, Loc),
    {reply, [Token], State};
handle_match(_Match, State) ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
