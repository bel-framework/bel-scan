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

%%%=====================================================================
%%% bel_scan_eng callback functions
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = inline,
                re = <<
                    "(<%=\\s+)(.*?[^\\s]*)(\\s+\\.%>)" "|"
                    "(<%=\\s+)(.*?[^\\s]*)(^(\\s*\\.%>))"
                >>
            },
            #marker{
                id = start,
                re = <<
                    "(<%=\\s+)(.*?[^\\s]*)(\\s+%>)" "|"
                    "(<%=\\s+)(.*?[^\\s]*)(^(\\s*%>))"
                >>
            },
            #marker{
                id = continue,
                re = <<
                    "(<%\\s+)(.*?[^\\s]*)(\\s+%>)" "|"
                    "(<%\\s+)(.*?[^\\s]*)(^(\\s*%>))"
                >>
            },
            #marker{
                id = terminate,
                re = <<
                    "(<%\\s+)(.*?[^\\s]*)(\\s+\\.%>)" "|"
                    "(<%\\s+)(.*?[^\\s]*)(^(\\s*\\.%>))"
                >>
            },
            #marker{
                id = comment,
                re = <<
                    "(<%!--\\s+)(.*?[^\\s]*)(\\s+--%>)" "|"
                    "(<%!--\\s+)(.*?[^\\s]*)(^(\\s*--%>))"
                >>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

handle_match({?MODULE, MarkerId, _Text, Captured, Loc}, State) ->
    [_SMarker, Expr, _EMarker] = Captured,
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
