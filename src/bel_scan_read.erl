%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Reader module.
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
-module(bel_scan_read).

% API functions
-export([ bin/1 ]).

-define(is_lf(X), (
    X =:= $\r orelse X =:= $\n orelse X =:= $\f
)).

%%%=====================================================================
%%% API functions
%%%=====================================================================

bin(<<$\r, $\n, Rest/binary>>) ->
    {{new_ln, 2}, Rest};
bin(<<Char, Rest/binary>>) when ?is_lf(Char) ->
    {{new_ln, 1}, Rest};
bin(<<_, Rest/binary>>) ->
    {{continue, 1}, Rest};
bin(<<>>) ->
    terminate.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
