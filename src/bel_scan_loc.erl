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

% API functions
-export([ new/1
        , read/1
        , read/2
        , incr_pos/2
        , incr_ln/2
        , incr_col/2
        , new_ln/1
        , to_tuple/1
        ]).

% State getters and setters functions
-export([ get_pos/1
        , set_pos/2
        , get_ln/1
        , set_ln/2
        , get_col/1
        , set_col/2
        , get_first_ln/1
        , set_first_ln/2
        , get_first_col/1
        , set_first_col/2
        ]).

-export_type([ t/0, pos/0, pos_tuple/0 ]).

-define(FIRST_POS, 0).
-define(FIRST_LN, 1).
-define(FIRST_COL, 1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(loc, { pos, ln, col, first_ln, first_col }).

-type t()         :: #loc{}.
-type pos()       :: non_neg_integer().
-type pos_tuple() :: {pos(), pos()}.

%%%=====================================================================
%%% API functions
%%%=====================================================================

new(Params) when is_map(Params) ->
    FirstLn = maps:get(first_ln, Params, ?FIRST_LN),
    FirstCol = maps:get(first_col, Params, ?FIRST_COL),
    #loc{
        pos = maps:get(pos, Params, ?FIRST_POS),
        ln = maps:get(ln, Params, FirstLn),
        col = maps:get(col, Params, FirstCol),
        first_ln = FirstLn,
        first_col = FirstCol
    }.

read(Bin) ->
    read(Bin, new(#{})).

read(Bin, #loc{} = Loc) when is_binary(Bin) ->
    do_read(Bin, Loc).

do_read(Bin, Loc) ->
    case bel_scan_read:bin(Bin) of
        {{new_ln, Incr}, Rest} ->
            do_read(Rest,
                set_ln(Loc#loc.ln+1,
                    set_col(Loc#loc.first_col,
                        incr_pos(Incr, Loc))));
        {{continue, Incr}, Rest} ->
            do_read(Rest, incr_col(Incr, Loc));
        terminate ->
            Loc
    end.

incr_pos(N, #loc{pos = Pos} = Loc) ->
    Loc#loc{pos = Pos+N}.

incr_ln(N, #loc{ln = Ln, pos = Pos} = Loc) ->
    Loc#loc{
        ln = Ln+N,
        pos = Pos+N
    }.

incr_col(N, #loc{col = Col, pos = Pos} = Loc) ->
    Loc#loc{
        col = Col+N,
        pos = Pos+N
    }.

new_ln(#loc{ln = Ln, first_col = FirstCol, pos = Pos} = Loc) ->
    Loc#loc{
        ln = Ln+1,
        col = FirstCol,
        pos = Pos+1
    }.

to_tuple(#loc{ln = Ln, col = Col}) ->
    {Ln, Col}.

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_pos(#loc{pos = Pos}) ->
    Pos.

set_pos(Pos, #loc{} = Loc) ->
    Loc#loc{pos = Pos}.

get_ln(#loc{ln = Ln}) ->
    Ln.

set_ln(Ln, #loc{} = Loc) ->
    Loc#loc{ln = Ln}.

get_col(#loc{col = Col}) ->
    Col.

set_col(Col, #loc{} = Loc) ->
    Loc#loc{col = Col}.

get_first_ln(#loc{first_ln = FirstLn}) ->
    FirstLn.

set_first_ln(FirstLn, #loc{} = Loc) ->
    Loc#loc{first_ln = FirstLn}.

get_first_col(#loc{first_col = FirstCol}) ->
    FirstCol.

set_first_col(FirstCol, #loc{} = Loc) ->
    Loc#loc{first_col = FirstCol}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

new_test() ->
    [ { "Should have default values"
      , ?assertEqual(#loc{
            pos = ?FIRST_POS,
            ln = ?FIRST_LN,
            col = ?FIRST_COL,
            first_ln = ?FIRST_LN,
            first_col = ?FIRST_COL
        }, new(#{}))
      }
    , { "Should have params values"
      , ?assertEqual(#loc{
            pos = 6,
            ln = 6,
            col = 6,
            first_ln = 6,
            first_col = 6
        }, new(#{
            pos => 6,
            ln => 6,
            col => 6,
            first_ln => 6,
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
