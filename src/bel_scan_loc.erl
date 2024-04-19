%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Location module.
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
-module(bel_scan_loc).

-export([ new/1
        , read/1
        , read/2
        , get_ln/1
        , set_ln/2
        , get_col/1
        , set_col/2
        , get_first_col/1
        , set_first_col/2
        , incr_ln/2
        , incr_col/2
        , new_ln/1
        , to_tuple/1
        ]).

-export_type([ t/0 ]).

-define(FIRST_LN, 1).
-define(FIRST_COL, 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(loc, { ln, col, first_col }).

-opaque t() :: #loc{}.

%%%=====================================================================
%%% API
%%%=====================================================================

new(Params) when is_map(Params) ->
    FirstCol = maps:get(first_col, Params, ?FIRST_COL),
    #loc{
        ln = maps:get(ln, Params, ?FIRST_LN),
        col = maps:get(col, Params, FirstCol),
        first_col = FirstCol
    }.

read(Bin) ->
    read(Bin, new(#{})).

read(Bin, #loc{} = Loc) when is_binary(Bin) ->
    do_read(Bin, Loc).

do_read(Bin, Loc) ->
    case bel_scan_read:bin(Bin) of
        {{new_ln, Incr}, Rest} ->
            do_read(Rest, incr_ln(Incr, Loc));
        {{continue, Incr}, Rest} ->
            do_read(Rest, incr_col(Incr, Loc));
        terminate ->
            Loc
    end.

get_ln(#loc{ln = Ln}) ->
    Ln.

set_ln(Ln, #loc{} = Loc) ->
    Loc#loc{ln = Ln}.

get_col(#loc{col = Col}) ->
    Col.

set_col(Col, #loc{} = Loc) ->
    Loc#loc{col = Col}.

get_first_col(#loc{first_col = FirstCol}) ->
    FirstCol.

set_first_col(FirstCol, #loc{} = Loc) ->
    Loc#loc{first_col = FirstCol}.

incr_ln(N, #loc{ln = Ln} = Loc) ->
    Loc#loc{ln = Ln+N}.

incr_col(N, #loc{col = Col} = Loc) ->
    Loc#loc{col = Col+N}.

new_ln(#loc{ln = Ln, first_col = FirstCol} = Loc) ->
    Loc#loc{ln = Ln+1, col = FirstCol}.

to_tuple(#loc{ln = Ln, col = Col}) ->
    {Ln, Col}.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

new_test() ->
    [ { "Should have default values"
      , ?assertEqual(#loc{
            ln = ?FIRST_LN,
            col = ?FIRST_COL,
            first_col = ?FIRST_COL
        }, new(#{}))
      }
    , { "Should have params values"
      , ?assertEqual(#loc{
            ln = 6,
            col = 6,
            first_col = 6
        }, new(#{
            ln => 6,
            col => 6,
            first_col => 6
        }))
      }
    ].

loc_test() ->
    Loc = new(#{}),
    [ { "Should increment one line"
      , ?assert((incr_ln(1, Loc))#loc.ln =:= ?FIRST_LN+1)}
    , { "Should increment one column"
      , ?assert((incr_col(1, Loc))#loc.col =:= ?FIRST_COL+1)}
    , { "Should add a new line"
      , ?assert(
            (new_ln(Loc))#loc.ln =:= ?FIRST_LN+1
            andalso (new_ln(Loc))#loc.col =:= ?FIRST_COL
        )}
    ].

-endif.
