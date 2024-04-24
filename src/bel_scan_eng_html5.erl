-module(bel_scan_eng_html5).
-behaviour(bel_scan_eng).

% bel_scan_eng callbacks
-export([ init/1
        , handle_start/2
        , handle_text/2
        , handle_match/2
        , handle_terminate/2
        ]).

-include("bel_scan_eng.hrl").

-define(DOCTYPE, "!(?:(?i)DOCTYPE)").

-define(SPECIAL_TAG, "(script|style|textarea|title)").

-define(VOID_TAG,
    "(area|base|br|col|embed|hr|img|input|"
    "link|meta|param|source|track|wbr)"
).

-define(ELEM_TAG, "(\\w+)").

-define(OPEN_TAG, "<").

-define(CLOSE_TAG, ">").

-define(CLOSE_VOID, "\\/?>").

-define(CLOSING_TAG, "<\\/(?1)\\s*>").

-define(ATTRS, "\\s*(.*?[^\\s]?)\\s*").

-define(CONTENT, ?ATTRS).

-define(CHILD_NODES, "\\s*((?:(?R)|(?:(?!<\\/?(?1)).))*)").

%%%=====================================================================
%%% bel_scan_eng callbacks
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = doctype,
                re = <<
                    ?OPEN_TAG ?DOCTYPE ?ATTRS ?CLOSE_TAG
                >>
            },
            #marker{
                id = special_tag,
                re = <<
                    ?OPEN_TAG ?SPECIAL_TAG ?ATTRS ?CLOSE_TAG
                        ?CONTENT
                    ?CLOSING_TAG
                >>
            },
            #marker{
                id = void_tag,
                re = <<
                    ?OPEN_TAG ?VOID_TAG ?ATTRS ?CLOSE_VOID
                >>
            },
            #marker{
                id = elem_tag,
                re = <<"(?s)"
                    ?OPEN_TAG ?ELEM_TAG ?ATTRS ?CLOSE_TAG
                        ?CHILD_NODES
                    ?CLOSING_TAG
                >>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, doctype, _Text, Captured, Loc}, State) ->
    [<<"html">>] = Captured,
    Token = bel_scan:token(doctype, <<"html">>, Loc),
    {reply, [Token], State};
handle_match({?MODULE, special_tag, _Text, Captured, Loc}, State) ->
    [Tag, Attrs, Content] = Captured,
    Metadata = {attributes(Attrs), Content},
    Token = bel_scan:token(special_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match({?MODULE, void_tag, _Text, Captured, Loc}, State) ->
    [Tag, Attrs] = Captured,
    Metadata = attributes(Attrs),
    Token = bel_scan:token(void_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match({?MODULE, elem_tag, _Text, Captured, Loc}, State) ->
    [Tag, Attrs, ChildNodes] = Captured,
    Metadata = {attributes(Attrs), child_nodes(ChildNodes)},
    Token = bel_scan:token(elem_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match(_Match, State) ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

attributes(Bin) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => [bel_scan_eng_html5_attr]
    })).

child_nodes(Bin) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => [?MODULE]
    })).
