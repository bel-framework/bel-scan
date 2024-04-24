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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SPECIAL_TAG, "(script|style|textarea|title)").

-define(VOID_TAG,
    "(area|base|br|col|embed|hr|img|input|"
    "link|meta|param|source|track|wbr)"
).

-define(ELEM_TAG, "(\\w+)").

-define(OPEN_TAG, "(<)").

-define(CLOSE_TAG, "(>)").

-define(CLOSE_VOID, "(\\/?>)").

-define(CLOSING_TAG, "(<\\/(?2)>)").

-define(ATTRS, "(.*?)").

-define(CONTENT, "(.*?)").

-define(CHILD_NODES, "((?:(?R)|(?:(?!<\\/?(?2)).*?))*)").

%%%=====================================================================
%%% bel_scan_eng callbacks
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = doctype,
                re = <<"<!(?:(?i)DOCTYPE)\\s*html>">>
            },
            #marker{
                id = special_tag,
                re = <<"(?s)"
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

handle_match({?MODULE, doctype, _Text, [], Loc}, State) ->
    Token = bel_scan:token(doctype, <<"html">>, Loc),
    {reply, [Token], State};
handle_match({?MODULE, special_tag, _Text, Captured, Loc}, State) ->
    [OAngB, Tag, Attrs, _CAngB, Content, _CTag] = Captured,
    Metadata = {attributes(Attrs, [OAngB, Tag], Loc), Content},
    Token = bel_scan:token(special_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match({?MODULE, void_tag, _Text, Captured, Loc}, State) ->
    [OAngB, Tag, Attrs, _CAngB] = Captured,
    Metadata = attributes(Attrs, [OAngB, Tag], Loc),
    Token = bel_scan:token(void_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match({?MODULE, elem_tag, _Text, Captured, Loc}, State) ->
    [OAngB, Tag, Attrs, CAngB, ChildNodes, _CTag] = Captured,
    Metadata = {
        attributes(Attrs, [OAngB, Tag], Loc),
        child_nodes(ChildNodes, [OAngB, Tag, Attrs, CAngB], Loc, State)
    },
    Token = bel_scan:token(elem_tag, Tag, Metadata, Loc),
    {reply, [Token], State};
handle_match({Mod, _, _, _, _}, State) when Mod =/= ?MODULE ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

attributes(Bin, PrevParts, Loc) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => [bel_scan_eng_html5_attr],
        loc => init_loc(PrevParts, Loc)
    })).

child_nodes(Bin, PrevParts, Loc, State) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => bel_scan:get_engines(State),
        loc => init_loc(PrevParts, Loc)
    })).

init_loc(PrevParts, {InitLoc, _EndLoc}) ->
    BLoc = bel_scan_loc:read(iolist_to_binary(PrevParts)),
    bel_scan_loc:incr(BLoc, InitLoc).

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

% NOTE: Just a test, not intended to have a valid HTML syntax.

-define(SLINE, <<
"     <!dOCtYPE html> Lalala  <area  hidden  /> <input /> <div id=\"foo\" title='b\\'a\\'r'    hidden > <div><b>vvv</b> </div> </div> Some test <title hidden     id='foo'  required   ><b>AAA</b></title> bar"
>>).

-define(MLINE, <<"
<script>

    <div>Foo</div>

</script>
<span>   <i>  ooooo  </i> </span>
bar

<title>

    Title

            </title>
">>).

scan_(Bin) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{engines => [?MODULE]})).

scan_test() ->
    [ { "Should scan single line"
      , ?assertEqual([
            {text,{{{1,1},{1,6}},undefined},<<"     ">>},
            {doctype,{{{1,6},{1,21}},undefined},<<"html">>},
            {text,{{{1,21},{1,30}},undefined},<<" Lalala  ">>},
            {void_tag,
                {{{1,30},{1,47}},
                    [{attribute,{{{1,37},{1,43}},<<"hidden">>},<<"hidden">>}]},
                <<"area">>},
            {text,{{{1,47},{1,48}},undefined},<<" ">>},
            {void_tag,{{{1,48},{1,57}},[]},<<"input">>},
            {text,{{{1,57},{1,58}},undefined},<<" ">>},
            {elem_tag,
                {{{1,58},{1,129}},
                    {[{attribute,
                        {{{1,63},{1,71}},{<<"id">>,<<"foo">>}},
                        <<"id=\"foo\"">>},
                    {attribute,
                        {{{1,72},{1,87}},{<<"title">>,<<"b\\'a\\'r">>}},
                        <<"title='b\\'a\\'r'">>},
                    {attribute,{{{1,91},{1,97}},<<"hidden">>},<<"hidden">>}],
                    [{text,{{{1,99},{1,100}},undefined},<<" ">>},
                    {elem_tag,
                        {{{1,100},{1,122}},
                        {[],
                            [{elem_tag,
                                {{{1,105},{1,115}},
                                {[],[{text,{{{1,108},{1,111}},undefined},<<"vvv">>}]}},
                                <<"b">>},
                            {text,{{{1,115},{1,116}},undefined},<<" ">>}]}},
                        <<"div">>},
                    {text,{{{1,122},{1,123}},undefined},<<" ">>}]}},
                <<"div">>},
            {text,{{{1,129},{1,140}},undefined},<<" Some test ">>},
            {special_tag,
                {{{1,140},{1,198}},
                    {[{attribute,{{{1,147},{1,153}},<<"hidden">>},<<"hidden">>},
                    {attribute,
                        {{{1,158},{1,166}},{<<"id">>,<<"foo">>}},
                        <<"id='foo'">>},
                    {attribute,
                        {{{1,168},{1,176}},<<"required">>},
                        <<"required">>}],
                    <<"<b>AAA</b>">>}},
                <<"title">>},
            {text,{{{1,198},{1,202}},undefined},<<" bar">>}
      ], scan_(?SLINE))}
    , { "Should scan multiple lines"
      , ?assertEqual([
            {text,{{{1,1},{2,1}},undefined},<<"\n">>},
            {special_tag,
                {{{2,1},{5,10}},{[],<<"\n\n    <div>Foo</div>\n\n">>}},
                <<"script">>},
            {text,{{{5,10},{6,1}},undefined},<<"\n">>},
            {elem_tag,
                {{{6,1},{6,34}},
                {[],
                [{text,{{{6,7},{6,10}},undefined},<<"   ">>},
                    {elem_tag,
                        {{{6,10},{6,26}},
                        {[],
                        [{text,
                            {{{6,13},{6,22}},undefined},
                            <<"  ooooo  ">>}]}},
                        <<"i">>},
                    {text,{{{6,26},{6,27}},undefined},<<" ">>}]}},
                <<"span">>},
            {text,{{{6,34},{9,1}},undefined},<<"ript>\n">>},
            {special_tag,
                {{{9,1},{13,21}},
                {[],<<"\n\n    Title\n\n            ">>}},
                <<"title">>},
            {text,{{{13,21},{14,1}},undefined},<<"t">>}
        ], scan_(?MLINE))}
    ].

-endif.
