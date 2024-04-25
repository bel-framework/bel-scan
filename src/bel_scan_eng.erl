%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Engine behaviour module.
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
-module(bel_scan_eng).

% API functions
-export([ compile/1, fold/2 ]).

% State getters and setters functions
-export([ get_module/1
        , set_module/2
        , get_markers/1
        , set_markers/2
        , get_state/1
        , set_state/2
        ]).

-export_type([ t/0
             , scan/0
             , marker_id/0
             , token/0
             , loc/0
             , opts/0
             , state/0
             , re_group/0
             , captured/0
             ]).

-include("bel_scan_eng.hrl").

-type t()         :: #engine{}.
-type scan()      :: bel_scan:t().
-type marker_id() :: bel_scan_mark:id().
-type token()     :: bel_scan:token().
-type loc()       :: bel_scan_loc:t().
-type opts()      :: term().
-type state()     :: term().
-type re_group()  :: binary().
-type captured()  :: [re_group()].

%%%=====================================================================
%%% Callbacks
%%%=====================================================================

-callback init(Opts) -> Engine
    when Opts     :: opts()
       , Engine   :: t()
                   .

-callback handle_start(Bin, Scan) -> Return
    when Bin    :: binary()
       , Scan   :: scan()
       , Return :: {noreply, scan()}
                 | {reply, binary(), scan()}
                 | {halt, scan()}
                 .

-callback handle_text(Text, Scan) -> Return
    when Text   :: binary()
       , Scan   :: scan()
       , Return :: {noreply, scan()}
                 | {reply, binary(), scan()}
                 | {halt, scan()}
                 .

-callback handle_match(Match, Scan) -> Return
    when Match     :: {MarkerMod, MarkerId, Text, Captured, Loc}
       , MarkerMod :: module()
       , MarkerId  :: marker_id()
       , Text      :: binary()
       , Captured  :: captured()
       , Loc       :: {InitLoc, EndLoc}
       , InitLoc   :: loc()
       , EndLoc    :: loc()
       , Scan      :: scan()
       , Return    :: {noreply, scan()}
                    | {reply, [token()], scan()}
                    | {halt, scan()}
                    .

-callback handle_terminate(Tokens, Scan0) -> Return
    when Tokens :: [token()]
       , Scan0  :: scan()
       , Return :: {noreply, scan()}
                 | {reply, [token()], scan()}
                 | {halt, scan()}
                 .

%%%=====================================================================
%%% API functions
%%%=====================================================================

compile(#engine{markers = Markers} = Eng) ->
    Eng#engine{markers = [bel_scan_mark:compile(M) || M <- Markers]}.

fold(#engine{} = Eng, Funs) when is_list(Funs) ->
    lists:foldl(fun(F, E) -> F(E) end, Eng, Funs).

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_module(#engine{module = Module}) ->
    Module.

set_module(Module, #engine{} = Eng) when is_atom(Module) ->
    Eng#engine{module = Module}.

get_markers(#engine{markers = Markers}) ->
    Markers.

set_markers(Markers, #engine{} = Eng) ->
    Eng#engine{markers = Markers}.

get_state(#engine{state = State}) ->
    State.

set_state(State, #engine{} = Eng) ->
    Eng#engine{state = State}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
