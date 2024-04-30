%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc EEl engine module.
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
-module(bel_scan_eng_eel).
-behaviour(bel_scan_eng).

% bel_scan_eng callback functions
-export([ init/1
        , handle_start/2
        , handle_text/2
        , handle_match/2
        , handle_terminate/2
        ]).

% Support functions
-export([ expr_token/3
        , expr_inline_id/0
        , expr_start_id/0
        , expr_continue_id/0
        , expr_end_id/0
        , comment_id/0
        ]).

-include("bel_scan_eng.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(EXPR_INLINE_ID, eel_expr_inline).
-define(EXPR_START_ID, eel_expr_start).
-define(EXPR_CONTINUE_ID, eel_expr_continue).
-define(EXPR_END_ID, eel_expr_end).
-define(EXPR_COMMENT_ID, eel_comment).

%%%=====================================================================
%%% bel_scan_eng callback functions
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = expr_inline_id(),
                re = "<%=\\s+((?:(?!<%).)*)\\s+\.%>"
            },
            #marker{
                id = expr_start_id(),
                re = "<%=\\s+((?:(?!<%).)*)\\s+%>"
            },
            #marker{
                id = expr_continue_id(),
                re = "<%\\s+((?:(?!<%).)*)\\s+%>"
            },
            #marker{
                id = expr_end_id(),
                re = "<%\\s+((?:(?!<%).)*)\\s+\.%>"
            },
            #marker{
                id = comment_id(),
                re = "<%!--\s+((?:(?!<%).)*)\s+--%>"
            }
            % TODO: Use "simple" markers and use a parser (yecc) to
            %       spot issues, like missing ending marker, e.g.:
            %       > "<%= case Bool of true -> %>ok<% end %>" <- Missing ".%>"
            % #marker{
            %     id = '<%=',
            %     re = "<%="
            % },
            % #marker{
            %     id = '.%>',
            %     re = "\\.\\s*%>"
            % },
            % #marker{
            %     id = '<%',
            %     re = "<%(?:(?!=))"
            % },
            % #marker{
            %     id = '%>',
            %     re = "(?:(?!\\.))%>"
            % },
            % #marker{
            %     id = '<%!--',
            %     re = "<%!--"
            % },
            % #marker{
            %     id = '--%>',
            %     re = "--%>"
            % }
        ]
    }.

handle_start(_Bin, Scan) ->
    {noreply, Scan}.

handle_text(_Text, Scan) ->
    {noreply, Scan}.

handle_match({?MODULE, eel_comment, _Captured, _Anno}, Scan) ->
    % TODO: Maybe push a comment token.
    {noreply, Scan};
handle_match({?MODULE, MarkerId, [Expr], Anno}, Scan) ->
    Token = expr_token(MarkerId, Anno, Expr),
    {reply, [Token], Scan};
handle_match({Mod, _, _, _}, Scan) when Mod =/= ?MODULE ->
    {noreply, Scan}.

handle_terminate(_Tokens, Scan) ->
    {noreply, Scan}.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

expr_token(Id, Anno, Expr) ->
    bel_scan_token:new(#{
        id => Id,
        anno => Anno,
        engine => ?MODULE,
        metadata => Expr
    }).

expr_inline_id() ->
    ?EXPR_INLINE_ID.

expr_start_id() ->
    ?EXPR_START_ID.

expr_continue_id() ->
    ?EXPR_CONTINUE_ID.

expr_end_id() ->
    ?EXPR_END_ID.

comment_id() ->
    ?EXPR_COMMENT_ID.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

% NOTE: Just a test, not intended to have a valid HTML syntax.

-define(SLINE, <<
"a <%= b .%> c <%= d %> e <% f %> g <% h .%> i"
>>).

-define(MLINE, <<"a <%= b
.%> c <%=
d %> e
<% f

%>

   g

<%

h

         .%>

i

">>).

scan_(Bin) ->
    bel_scan:get_tokens(bel_scan:bin(Bin, #{
        engines => [
            ?MODULE,
            {bel_scan_eng_html5, #{
                attr_engines => [
                    bel_scan_eng_eel_attr,
                    bel_scan_eng_html5_attr
                ]
            }}
        ]
    })).

scan_test() ->
    [ { "Should scan attributes"
      , ?assertEqual([
            {token,html_tag,
                {anno,string,
                    {loc,0,1,1,1,1},
                    {loc,35,1,36,1,1},
                    <<"<div :if={@bool} id={@id}>foo</div>">>},
                {<<"div">>,
                    {[{token,eel_directive,
                        {anno,string,
                            {loc,5,1,6,1,1},
                            {loc,16,1,17,1,1},
                            <<":if={@bool}">>},
                        {<<"if">>,
                        [{token,eel_expr_inline,
                                {anno,string,
                                    {loc,10,1,11,1,1},
                                    {loc,15,1,16,1,1},
                                    <<":if={@bool}">>},
                                <<"@bool">>,bel_scan_eng_eel}]},
                        bel_scan_eng_eel_attr},
                    {token,eel_attribute,
                        {anno,string,
                            {loc,17,1,18,1,1},
                            {loc,25,1,26,1,1},
                            <<"id={@id}">>},
                        {<<"id">>,
                        [{token,eel_expr_inline,
                                {anno,string,
                                    {loc,21,1,22,1,1},
                                    {loc,24,1,25,1,1},
                                    <<"id={@id}">>},
                                <<"@id">>,bel_scan_eng_eel}]},
                        bel_scan_eng_eel_attr}],
                    [{token,text,
                        {anno,string,
                            {loc,26,1,27,1,1},
                            {loc,29,1,30,1,1},
                            <<"foo">>},
                        undefined,bel_scan}]}},
                bel_scan_eng_html5},
            {token,html_tag,
                {anno,string,
                    {loc,35,1,36,1,1},
                    {loc,63,1,64,1,1},
                    <<"<span class='foo'>bar</span>">>},
                {<<"span">>,
                    {[{token,html5_attribute,
                        {anno,string,
                            {loc,41,1,42,1,1},
                            {loc,52,1,53,1,1},
                            <<"class='foo'">>},
                        {<<"class">>,<<"foo">>},
                        bel_scan_eng_html5_attr}],
                    [{token,text,
                        {anno,string,
                            {loc,53,1,54,1,1},
                            {loc,56,1,57,1,1},
                            <<"bar">>},
                        undefined,bel_scan}]}},
                bel_scan_eng_html5}
      ], scan_(<<"<div :if={@bool} id={@id}>foo</div><span class='foo'>bar</span>">>))}
    , { "Should scan single line"
      , ?assertEqual([
            {token,text,
                    {anno,string,
                        {loc,0,1,1,1,1},
                        {loc,2,1,3,1,1},
                        <<"a ">>},
                    undefined,bel_scan},
            {token,eel_expr_inline,
                    {anno,string,
                        {loc,2,1,3,1,1},
                        {loc,11,1,12,1,1},
                        <<"<%= b .%>">>},
                    <<"b">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,11,1,12,1,1},
                        {loc,14,1,15,1,1},
                        <<" c ">>},
                    undefined,bel_scan},
            {token,eel_expr_start,
                    {anno,string,
                        {loc,14,1,15,1,1},
                        {loc,22,1,23,1,1},
                        <<"<%= d %>">>},
                    <<"d">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,22,1,23,1,1},
                        {loc,25,1,26,1,1},
                        <<" e ">>},
                    undefined,bel_scan},
            {token,eel_expr_continue,
                    {anno,string,
                        {loc,25,1,26,1,1},
                        {loc,32,1,33,1,1},
                        <<"<% f %>">>},
                    <<"f">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,32,1,33,1,1},
                        {loc,35,1,36,1,1},
                        <<" g ">>},
                    undefined,bel_scan},
            {token,eel_expr_end,
                    {anno,string,
                        {loc,35,1,36,1,1},
                        {loc,43,1,44,1,1},
                        <<"<% h .%>">>},
                    <<"h">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,43,1,44,1,1},
                        {loc,45,1,46,1,1},
                        <<" i">>},
                    undefined,bel_scan}
        ], scan_(?SLINE))}
    , { "Should scan multiple lines"
      , ?assertEqual([
            {token,text,
                    {anno,string,
                        {loc,0,1,1,1,1},
                        {loc,2,1,3,1,1},
                        <<"a ">>},
                    undefined,bel_scan},
            {token,eel_expr_inline,
                    {anno,string,
                        {loc,2,1,3,1,1},
                        {loc,11,2,4,1,1},
                        <<"<%= b\n.%>">>},
                    <<"b">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,11,2,4,1,1},
                        {loc,14,2,7,1,1},
                        <<" c ">>},
                    undefined,bel_scan},
            {token,eel_expr_start,
                    {anno,string,
                        {loc,14,2,7,1,1},
                        {loc,22,3,5,1,1},
                        <<"<%=\nd %>">>},
                    <<"d">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,22,3,5,1,1},
                        {loc,25,4,1,1,1},
                        <<" e\n">>},
                    undefined,bel_scan},
            {token,eel_expr_continue,
                    {anno,string,
                        {loc,25,4,1,1,1},
                        {loc,33,6,3,1,1},
                        <<"<% f\n\n%>">>},
                    <<"f">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,33,6,3,1,1},
                        {loc,41,10,1,1,1},
                        <<"\n\n   g\n\n">>},
                    undefined,bel_scan},
            {token,eel_expr_end,
                    {anno,string,
                        {loc,41,10,1,1,1},
                        {loc,60,14,13,1,1},
                        <<"<%\n\nh\n\n         .%>">>},
                    <<"h">>,bel_scan_eng_eel},
            {token,text,
                    {anno,string,
                        {loc,60,14,13,1,1},
                        {loc,65,18,1,1,1},
                        <<"\n\ni\n\n">>},
                    undefined,bel_scan}
        ], scan_(?MLINE))}
    ].

-endif.
