%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc HTML5 engine module.
%%%
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(bel_scan_eng_html5).
-behaviour(bel_scan_eng).

% bel_scan_eng callback functions
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

-define(ATTRS_ENGINES, [bel_scan_eng_html5_attr]).

-record(state, { attrs_engines :: [module()] }).

%%%=====================================================================
%%% bel_scan_eng callback functions
%%%=====================================================================

init(Opts) ->
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
        ],
        state = #state{
            attrs_engines = bel_scan:init_engines(
                maps:get(attrs_engines, Opts, ?ATTRS_ENGINES)
            )
        }
    }.

handle_start(_Bin, Scan) ->
    {noreply, Scan}.

handle_text(_Text, Scan) ->
    {noreply, Scan}.

handle_match({?MODULE, doctype, [], Anno}, Scan) ->
    Token = bel_scan:token(doctype, Anno, <<"html">>),
    {reply, [Token], Scan};
handle_match({?MODULE, special_tag, Captured, Anno}, Scan) ->
    [OAngB, Tag, Attrs, _CAngB, Content, _CTag] = Captured,
    Metadata = {
        attributes(Attrs, [OAngB, Tag], Anno, Scan),
        Content
    },
    Token = bel_scan:token(binary_to_atom(Tag), Anno, Metadata),
    {reply, [Token], Scan};
handle_match({?MODULE, void_tag, Captured, Anno}, Scan) ->
    [OAngB, Tag, Attrs, _CAngB] = Captured,
    Metadata = {
        attributes(Attrs, [OAngB, Tag], Anno, Scan),
        []
    },
    Token = bel_scan:token(binary_to_atom(Tag), Anno, Metadata),
    {reply, [Token], Scan};
handle_match({?MODULE, elem_tag, Captured, Anno}, Scan) ->
    [OAngB, Tag, Attrs, CAngB, ChildNodes, _CTag] = Captured,
    Metadata = {
        attributes(Attrs, [OAngB, Tag], Anno, Scan),
        child_nodes(ChildNodes, [OAngB, Tag, Attrs, CAngB], Anno, Scan)
    },
    Token = bel_scan:token(binary_to_atom(Tag), Anno, Metadata),
    {reply, [Token], Scan};
handle_match({Mod, _, _, _}, Scan) when Mod =/= ?MODULE ->
    {noreply, Scan}.

handle_terminate(_Tokens, Scan) ->
    {noreply, Scan}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

state(Scan) ->
    {?MODULE, Engine} = bel_scan:lookup_engine(?MODULE, Scan),
    Engine#engine.state.

attributes(Bin, PrevParts, Anno, Scan) ->
    State = state(Scan),
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => State#state.attrs_engines,
        loc => init_loc(PrevParts, Anno)
    })).

child_nodes(Bin, PrevParts, Anno, Scan) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => bel_scan:get_engines(Scan),
        loc => init_loc(PrevParts, Anno)
    })).

init_loc(PrevParts, Anno) ->
    Loc = bel_scan_anno:get_loc(Anno),
    bel_scan_loc:read(iolist_to_binary(PrevParts), Loc).

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
            {text,
                {anno,string,{loc,0,1,1,1,1},{loc,5,1,6,1,1},<<"     ">>},
                undefined},
            {doctype,
                {anno,string,
                {loc,5,1,6,1,1},
                {loc,20,1,21,1,1},
                <<"<!dOCtYPE html>">>},
                <<"html">>},
            {text,
                {anno,string,
                {loc,20,1,21,1,1},
                {loc,29,1,30,1,1},
                <<" Lalala  ">>},
                undefined},
            {area,
                {anno,string,
                {loc,29,1,30,1,1},
                {loc,46,1,47,1,1},
                <<"<area  hidden  />">>},
                {[{attribute,
                {anno,string,
                    {loc,36,1,37,1,1},
                    {loc,42,1,43,1,1},
                    <<"hidden">>},
                <<"hidden">>}],
                []}},
            {text,
                {anno,string,{loc,46,1,47,1,1},{loc,47,1,48,1,1},<<" ">>},
                undefined},
            {input,
                {anno,string,
                {loc,47,1,48,1,1},
                {loc,56,1,57,1,1},
                <<"<input />">>},
                {[],[]}},
            {text,
                {anno,string,{loc,56,1,57,1,1},{loc,57,1,58,1,1},<<" ">>},
                undefined},
            {'div',
                {anno,string,
                {loc,57,1,58,1,1},
                {loc,128,1,129,1,1},
                <<"<div id=\"foo\" title='b\\'a\\'r'    hidden > <div><b>vvv</b> </div> </div>">>},
                {[{attribute,
                {anno,string,
                    {loc,62,1,63,1,1},
                    {loc,70,1,71,1,1},
                    <<"id=\"foo\"">>},
                {<<"id">>,<<"foo">>}},
                {attribute,
                {anno,string,
                    {loc,71,1,72,1,1},
                    {loc,86,1,87,1,1},
                    <<"title='b\\'a\\'r'">>},
                {<<"title">>,<<"b\\'a\\'r">>}},
                {attribute,
                {anno,string,
                    {loc,90,1,91,1,1},
                    {loc,96,1,97,1,1},
                    <<"hidden">>},
                <<"hidden">>}],
                [{text,
                {anno,string,
                    {loc,98,1,99,1,1},
                    {loc,99,1,100,1,1},
                    <<" ">>},
                undefined},
                {'div',
                {anno,string,
                    {loc,99,1,100,1,1},
                    {loc,121,1,122,1,1},
                    <<"<div><b>vvv</b> </div>">>},
                {[],
                    [{b,
                    {anno,string,
                    {loc,104,1,105,1,1},
                    {loc,114,1,115,1,1},
                    <<"<b>vvv</b>">>},
                    {[],
                    [{text,
                        {anno,string,
                        {loc,107,1,108,1,1},
                        {loc,110,1,111,1,1},
                        <<"vvv">>},
                        undefined}]}},
                    {text,
                    {anno,string,
                    {loc,114,1,115,1,1},
                    {loc,115,1,116,1,1},
                    <<" ">>},
                    undefined}]}},
                {text,
                {anno,string,
                    {loc,121,1,122,1,1},
                    {loc,122,1,123,1,1},
                    <<" ">>},
                undefined}]}},
            {text,
                {anno,string,
                {loc,128,1,129,1,1},
                {loc,139,1,140,1,1},
                <<" Some test ">>},
                undefined},
            {title,
                {anno,string,
                {loc,139,1,140,1,1},
                {loc,197,1,198,1,1},
                <<"<title hidden     id='foo'  required   ><b>AAA</b></title>">>},
                {[{attribute,
                {anno,string,
                    {loc,146,1,147,1,1},
                    {loc,152,1,153,1,1},
                    <<"hidden">>},
                <<"hidden">>},
                {attribute,
                {anno,string,
                    {loc,157,1,158,1,1},
                    {loc,165,1,166,1,1},
                    <<"id='foo'">>},
                {<<"id">>,<<"foo">>}},
                {attribute,
                {anno,string,
                    {loc,167,1,168,1,1},
                    {loc,175,1,176,1,1},
                    <<"required">>},
                <<"required">>}],
                <<"<b>AAA</b>">>}},
            {text,
                {anno,string,
                {loc,197,1,198,1,1},
                {loc,201,1,202,1,1},
                <<" bar">>},
                undefined}
        ], scan_(?SLINE))}
    , { "Should scan multiple lines"
      , ?assertEqual([
            {text,
                {anno,string,{loc,0,1,1,1,1},{loc,1,2,1,1,1},<<"\n">>},
                undefined},
            {script,
                {anno,string,
                    {loc,1,2,1,1,1},
                    {loc,40,6,10,1,1},
                    <<"<script>\n\n    <div>Foo</div>\n\n</script>">>},
                {[],<<"\n\n    <div>Foo</div>\n\n">>}},
            {text,
                {anno,string,
                    {loc,40,6,10,1,1},
                    {loc,41,7,1,1,1},
                    <<"\n">>},
                undefined},
            {span,
                {anno,string,
                    {loc,41,7,1,1,1},
                    {loc,74,7,34,1,1},
                    <<"<span>   <i>  ooooo  </i> </span>">>},
                {[],
                [{text,
                    {anno,string,
                        {loc,47,7,7,1,1},
                        {loc,50,7,10,1,1},
                        <<"   ">>},
                    undefined},
                {i,{anno,string,
                        {loc,50,7,10,1,1},
                        {loc,66,7,26,1,1},
                        <<"<i>  ooooo  </i>">>},
                    {[],
                    [{text,
                        {anno,string,
                            {loc,53,7,13,1,1},
                            {loc,62,7,22,1,1},
                            <<"  ooooo  ">>},
                        undefined}]}},
                {text,
                    {anno,string,
                        {loc,66,7,26,1,1},
                        {loc,67,7,27,1,1},
                        <<" ">>},
                    undefined}]}},
            {text,
                {anno,string,
                    {loc,74,7,34,1,1},
                    {loc,80,10,1,1,1},
                    <<"\nbar\n\n">>},
                undefined},
            {title,
                {anno,string,
                    {loc,80,10,1,1,1},
                    {loc,120,14,21,1,1},
                    <<"<title>\n\n    Title\n\n            </title>">>},
                {[],<<"\n\n    Title\n\n            ">>}},
            {text,
                {anno,string,
                    {loc,120,14,21,1,1},
                    {loc,121,15,1,1,1},
                    <<"\n">>},
                undefined}
        ], scan_(?MLINE))}
    ].

-endif.
