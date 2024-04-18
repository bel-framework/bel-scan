-module(bel_scan_eel_eng).
-behaviour(bel_scan_eng).

% bel_scan_eng callbacks
-export([ init/1
        , handle_start/1
        , handle_text/2
        , handle_match/2
        , handle_terminate/1
        ]).

-include("bel_scan_eng.hrl").

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = inline,
                re_start = <<"<%=\\s+">>,
                re_end = <<"\\s+.%>">>
            },
            #marker{
                id = start,
                re_start = <<"<%=\\s+">>,
                re_end = <<"\\s+%>">>
            },
            #marker{
                id = continue,
                re_start = <<"<%\\s+">>,
                re_end = <<"\\s+%>">>
            },
            #marker{
                id = terminate,
                re_start = <<"<%\\s+">>,
                re_end = <<"\\s+.%>">>
            },
            #marker{
                id = comment,
                re_start = <<"<%!--\\s+">>,
                re_end = <<"\\s+--%>">>
            }
        ]
    }.

handle_start(State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, {inline, Text, _Captured}}, State) ->
    Token = bel_scan:token(inline, Text, State),
    {reply, [Token], State};
handle_match({?MODULE, {start, _Text, _Captured}}, State) ->
    {noreply, State};
handle_match({?MODULE, {continue, _Text, _Captured}}, State) ->
    {noreply, State};
handle_match({?MODULE, {terminate, _Text, _Captured}}, State) ->
    {noreply, State};
handle_match({?MODULE, {comment, Text, _Captured}}, State) ->
    Token = bel_scan:token(comment, Text, State),
    {reply, [Token], State};
handle_match(_Match, State) ->
    {noreply, State}.

handle_terminate(State) ->
    {noreply, State}.
