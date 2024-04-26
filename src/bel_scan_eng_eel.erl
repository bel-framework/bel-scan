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
                id = inline,
                re = <<"<%=\\s+((?:(?!<%).)*)\\s+\.%>">>
            },
            #marker{
                id = start,
                re = <<"<%=\\s+((?:(?!<%).)*)\\s+%>">>
            },
            #marker{
                id = continue,
                re = <<"<%\\s+((?:(?!<%).)*)\\s+%>">>
            },
            #marker{
                id = terminate,
                re = <<"<%\\s+((?:(?!<%).)*)\\s+\.%>">>
            },
            #marker{
                id = comment,
                re = <<"<%!--\s+((?:(?!<%).)*)\s+--%>">>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, MarkerId, _Text, Captured, Loc}, State) ->
    [Expr] = Captured,
    Token = bel_scan:token(MarkerId, Expr, Loc),
    {reply, [Token], State};
handle_match({Mod, _, _, _, _}, State) when Mod =/= ?MODULE ->
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
            {text,{{{1,1},{1,3}},undefined},<<"a ">>},
            {inline,{{{1,3},{1,12}},undefined},<<"b">>},
            {text,{{{1,12},{1,15}},undefined},<<" c ">>},
            {start,{{{1,15},{1,23}},undefined},<<"d">>},
            {text,{{{1,23},{1,26}},undefined},<<" e ">>},
            {continue,{{{1,26},{1,33}},undefined},<<"f">>},
            {text,{{{1,33},{1,36}},undefined},<<" g ">>},
            {terminate,{{{1,36},{1,44}},undefined},<<"h">>},
            {text,{{{1,44},{1,46}},undefined},<<" i">>}
        ], scan_(?SLINE))}
    , { "Should scan multiple lines"
      , ?assertEqual([
            {text,{{{1,1},{1,3}},undefined},<<"a ">>},
            {inline,{{{1,3},{2,4}},undefined},<<"b">>},
            {text,{{{2,4},{2,7}},undefined},<<" c ">>},
            {start,{{{2,7},{3,5}},undefined},<<"d">>},
            {text,{{{3,5},{4,1}},undefined},<<" e\n">>},
            {continue,{{{4,1},{6,3}},undefined},<<"f">>},
            {text,{{{6,3},{10,1}},undefined},<<"\n\n   g\n\n">>},
            {terminate,{{{10,1},{14,13}},undefined},<<"h">>},
            {text,{{{14,13},{18,1}},undefined},<<"\n\ni\n\n">>}
        ], scan_(?MLINE))}
    ].

-endif.
