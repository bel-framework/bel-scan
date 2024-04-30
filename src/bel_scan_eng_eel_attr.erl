%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc EEl attributes engine module.
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
-module(bel_scan_eng_eel_attr).
-behaviour(bel_scan_eng).

% bel_scan_eng callback functions
-export([ init/1
        , handle_start/2
        , handle_text/2
        , handle_match/2
        , handle_terminate/2
        ]).

-include("bel_scan_eng.hrl").

-define(DIRECTIVES, [<<"if">>, <<"let">>, <<"for">>]).

%%%=====================================================================
%%% bel_scan_eng callback functions
%%%=====================================================================

init(_Opts) ->
    #engine{
        markers = [
            #marker{
                id = eel_attribute,
                re = "(?:(?!:))(\\w+)=\\{(.*?[^\\\\}])\\}"
            },
            #marker{
                id = eel_directive,
                re = [":(", lists:join("|", ?DIRECTIVES), ")=\\{(.*?[^\\\\}])\\}"]
            }
        ]
    }.

handle_start(_Bin, Scan) ->
    {noreply, Scan}.

handle_text(_Text, Scan) ->
    {halt, Scan}.

handle_match({?MODULE, eel_attribute, [K, Expr], Anno}, Scan) ->
    {reply, [attribute_token(Anno, {K, Expr})], Scan};
handle_match({?MODULE, eel_directive, [K, Expr], Anno}, Scan) ->
    case lists:member(K, ?DIRECTIVES) of
        true ->
            {reply, [directive_token(Anno, {K, Expr})], Scan};
        false ->
            error({invalid_directive, K})
    end;
handle_match({Mod, _, _, _}, Scan) when Mod =/= ?MODULE ->
    {noreply, Scan}.

handle_terminate(_Tokens, Scan) ->
    {noreply, Scan}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

attribute_token(Anno, {K, Expr}) ->
    InitLoc = bel_scan_anno:get_loc(Anno),
    Loc = bel_scan_loc:read(<<K/binary, "={">>, InitLoc),
    token(eel_attribute, Loc, Anno, {K, Expr}).

directive_token(Anno, {K, Expr}) ->
    InitLoc = bel_scan_anno:get_loc(Anno),
    Loc = bel_scan_loc:read(<<$:, K/binary, "={">>, InitLoc),
    token(eel_directive, Loc, Anno, {K, Expr}).

token(Id, Loc, Anno, {K, Expr}) ->
    bel_scan_token:new(#{
        id => Id,
        anno => Anno,
        metadata => {K, expr_tokens(Loc, Anno, Expr)},
        engine => ?MODULE
    }).

expr_tokens(Loc, Anno0, Expr) ->
    Anno1 = bel_scan_anno:set_loc(Loc, Anno0),
    EndLoc = bel_scan_loc:read(Expr, Loc),
    Anno = bel_scan_anno:set_end_loc(EndLoc, Anno1),
    [bel_scan_eng_eel:expr_token(
        bel_scan_eng_eel:expr_inline_id(),
        Anno,
        Expr
    )].
