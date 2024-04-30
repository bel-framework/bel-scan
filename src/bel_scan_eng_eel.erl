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

-include("bel_scan_eng.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=====================================================================
%%% bel_scan_eng callback functions
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = '<%=',
                re = "<%="
            },
            #marker{
                id = '.%>',
                re = "\\.\\s*%>"
            },
            #marker{
                id = '<%',
                re = "<%(?:(?!=))"
            },
            #marker{
                id = '%>',
                re = "(?:(?!\\.))%>"
            },
            #marker{
                id = '<%!--',
                re = "<%!--"
            },
            #marker{
                id = '--%>',
                re = "--%>"
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, MarkerId, [], Anno}, State) ->
    Token = bel_scan_token:new(#{id => MarkerId, anno => Anno}),
    {reply, [Token], State};
handle_match({Mod, _, _, _}, State) when Mod =/= ?MODULE ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.

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
    bel_scan:get_tokens(bel_scan:bin(Bin, #{engines => [?MODULE]})).

scan_test() ->
    [ { "Should scan single line"
      , ?assertEqual([
        {token,text,
                {anno,string,
                    {loc,0,1,1,1,1},
                    {loc,2,1,3,1,1},
                    <<"a ">>},
                undefined},
        {token,'<%=',
                {anno,string,
                    {loc,2,1,3,1,1},
                    {loc,5,1,6,1,1},
                    <<"<%=">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,5,1,6,1,1},
                    {loc,8,1,9,1,1},
                    <<" b ">>},
                undefined},
        {token,'.%>',
                {anno,string,
                    {loc,8,1,9,1,1},
                    {loc,11,1,12,1,1},
                    <<".%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,11,1,12,1,1},
                    {loc,14,1,15,1,1},
                    <<" c ">>},
                undefined},
        {token,'<%=',
                {anno,string,
                    {loc,14,1,15,1,1},
                    {loc,17,1,18,1,1},
                    <<"<%=">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,17,1,18,1,1},
                    {loc,20,1,21,1,1},
                    <<" d ">>},
                undefined},
        {token,'%>',
                {anno,string,
                    {loc,20,1,21,1,1},
                    {loc,22,1,23,1,1},
                    <<"%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,22,1,23,1,1},
                    {loc,25,1,26,1,1},
                    <<" e ">>},
                undefined},
        {token,'<%',
                {anno,string,
                    {loc,25,1,26,1,1},
                    {loc,27,1,28,1,1},
                    <<"<%">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,27,1,28,1,1},
                    {loc,30,1,31,1,1},
                    <<" f ">>},
                undefined},
        {token,'%>',
                {anno,string,
                    {loc,30,1,31,1,1},
                    {loc,32,1,33,1,1},
                    <<"%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,32,1,33,1,1},
                    {loc,35,1,36,1,1},
                    <<" g ">>},
                undefined},
        {token,'<%',
                {anno,string,
                    {loc,35,1,36,1,1},
                    {loc,37,1,38,1,1},
                    <<"<%">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,37,1,38,1,1},
                    {loc,40,1,41,1,1},
                    <<" h ">>},
                undefined},
        {token,'.%>',
                {anno,string,
                    {loc,40,1,41,1,1},
                    {loc,43,1,44,1,1},
                    <<".%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,43,1,44,1,1},
                    {loc,45,1,46,1,1},
                    <<" i">>},
                undefined}
        ], scan_(?SLINE))}
    , { "Should scan multiple lines"
      , ?assertEqual([
        {token,text,
                {anno,string,
                    {loc,0,1,1,1,1},
                    {loc,2,1,3,1,1},
                    <<"a ">>},
                undefined},
        {token,'<%=',
                {anno,string,
                    {loc,2,1,3,1,1},
                    {loc,5,1,6,1,1},
                    <<"<%=">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,5,1,6,1,1},
                    {loc,8,2,1,1,1},
                    <<" b\n">>},
                undefined},
        {token,'.%>',
                {anno,string,
                    {loc,8,2,1,1,1},
                    {loc,11,2,4,1,1},
                    <<".%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,11,2,4,1,1},
                    {loc,14,2,7,1,1},
                    <<" c ">>},
                undefined},
        {token,'<%=',
                {anno,string,
                    {loc,14,2,7,1,1},
                    {loc,17,2,10,1,1},
                    <<"<%=">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,17,2,10,1,1},
                    {loc,20,3,3,1,1},
                    <<"\nd ">>},
                undefined},
        {token,'%>',
                {anno,string,
                    {loc,20,3,3,1,1},
                    {loc,22,3,5,1,1},
                    <<"%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,22,3,5,1,1},
                    {loc,25,4,1,1,1},
                    <<" e\n">>},
                undefined},
        {token,'<%',
                {anno,string,
                    {loc,25,4,1,1,1},
                    {loc,27,4,3,1,1},
                    <<"<%">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,27,4,3,1,1},
                    {loc,31,6,1,1,1},
                    <<" f\n\n">>},
                undefined},
        {token,'%>',
                {anno,string,
                    {loc,31,6,1,1,1},
                    {loc,33,6,3,1,1},
                    <<"%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,33,6,3,1,1},
                    {loc,41,10,1,1,1},
                    <<"\n\n   g\n\n">>},
                undefined},
        {token,'<%',
                {anno,string,
                    {loc,41,10,1,1,1},
                    {loc,43,10,3,1,1},
                    <<"<%">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,43,10,3,1,1},
                    {loc,57,14,10,1,1},
                    <<"\n\nh\n\n         ">>},
                undefined},
        {token,'.%>',
                {anno,string,
                    {loc,57,14,10,1,1},
                    {loc,60,14,13,1,1},
                    <<".%>">>},
                undefined},
        {token,text,
                {anno,string,
                    {loc,60,14,13,1,1},
                    {loc,65,18,1,1,1},
                    <<"\n\ni\n\n">>},
                undefined}
        ], scan_(?MLINE))}
    ].

-endif.
