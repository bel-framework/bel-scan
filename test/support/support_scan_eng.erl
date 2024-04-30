%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Support engine.
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
-module(support_scan_eng).
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
                id = expr,
                re = <<
                    % nested case {{ {{ foo}} }}
                    "{{\\s*(.*?[^}]+[^{]+[^\\s])\\s*}}" "|"
                    % simple case {{ foo }}
                    "{{\\s*(.*?[^\\s])\\s*}}"
                >>
            }
        ]
    }.

handle_start(_Bin, State) ->
    {noreply, State}.

handle_text(_Text, State) ->
    {noreply, State}.

% nested case
handle_match({?MODULE, expr, [Expr], Anno}, State) ->
    {reply, [token(Anno, Expr)], State};
% simple case
handle_match({?MODULE, expr, [<<>>, Expr], Anno}, State) ->
    {reply, [token(Anno, Expr)], State};
handle_match({Mod, _, _, _}, State) when Mod =/= ?MODULE ->
    {noreply, State}.

handle_terminate(_Tokens, State) ->
    {noreply, State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

token(Anno, Expr) ->
    bel_scan_token:new(#{
        id => expr,
        anno => Anno,
        metadata => scan(Expr)
    }).

scan(Expr) ->
    {ok, Tokens, _} = erl_scan:string(binary_to_list(<<Expr/binary, $.>>)),
    {ok, AST} = erl_parse:parse_exprs(Tokens),
    AST.
