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

handle_match({?MODULE, doctype, _Text, [], Loc}, Scan) ->
    Token = bel_scan:token(doctype, <<"html">>, Loc),
    {reply, [Token], Scan};
handle_match({?MODULE, special_tag, _Text, Captured, Loc}, Scan) ->
    [OAngB, Tag, Attrs, _CAngB, Content, _CTag] = Captured,
    Metadata = {attributes(Attrs, [OAngB, Tag], Loc, Scan), Content},
    Token = bel_scan:token(special_tag, Tag, Metadata, Loc),
    {reply, [Token], Scan};
handle_match({?MODULE, void_tag, _Text, Captured, Loc}, Scan) ->
    [OAngB, Tag, Attrs, _CAngB] = Captured,
    Metadata = attributes(Attrs, [OAngB, Tag], Loc, Scan),
    Token = bel_scan:token(void_tag, Tag, Metadata, Loc),
    {reply, [Token], Scan};
handle_match({?MODULE, elem_tag, _Text, Captured, Loc}, Scan) ->
    [OAngB, Tag, Attrs, CAngB, ChildNodes, _CTag] = Captured,
    Metadata = {
        attributes(Attrs, [OAngB, Tag], Loc, Scan),
        child_nodes(ChildNodes, [OAngB, Tag, Attrs, CAngB], Loc, Scan)
    },
    Token = bel_scan:token(elem_tag, Tag, Metadata, Loc),
    {reply, [Token], Scan};
handle_match({Mod, _, _, _, _}, Scan) when Mod =/= ?MODULE ->
    {noreply, Scan}.

handle_terminate(_Tokens, Scan) ->
    {noreply, Scan}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

state(Scan) ->
    {?MODULE, Engine} = bel_scan:lookup_engine(?MODULE, Scan),
    Engine#engine.state.

attributes(Bin, PrevParts, Loc, Scan) ->
    State = state(Scan),
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => State#state.attrs_engines,
        loc => init_loc(PrevParts, Loc)
    })).

child_nodes(Bin, PrevParts, Loc, Scan) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => bel_scan:get_engines(Scan),
        loc => init_loc(PrevParts, Loc)
    })).

init_loc(PrevParts, {InitLoc, _EndLoc}) ->
    bel_scan_loc:read(iolist_to_binary(PrevParts), InitLoc).

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
                {{{2,1},{6,10}},{[],<<"\n\n    <div>Foo</div>\n\n">>}},
                <<"script">>},
            {text,{{{6,10},{7,1}},undefined},<<"\n">>},
            {elem_tag,
                {{{7,1},{7,34}},
                {[],
                [{text,{{{7,7},{7,10}},undefined},<<"   ">>},
                {elem_tag,
                    {{{7,10},{7,26}},
                        {[],
                        [{text,
                            {{{7,13},{7,22}},undefined},
                            <<"  ooooo  ">>}]}},
                    <<"i">>},
                {text,{{{7,26},{7,27}},undefined},<<" ">>}]}},
                <<"span">>},
            {text,{{{7,34},{10,1}},undefined},<<"\nbar\n\n">>},
            {special_tag,
                {{{10,1},{14,21}},
                {[],<<"\n\n    Title\n\n            ">>}},
                <<"title">>},
            {text,{{{14,21},{15,1}},undefined},<<"\n">>}
        ], scan_(?MLINE))}
    ].

-endif.
