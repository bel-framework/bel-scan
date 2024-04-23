%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Binary part module.
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
-module(bel_scan_bpart).

-export([ new/1
        , reset_pos/2
        , get_bin/1
        , set_bin/2
        , get_pos/1
        , set_pos/2
        , get_len/1
        , set_len/2
        , incr_pos/2
        , incr_len/2
        , get_part/1
        ]).

-export_type([ t/0 ]).

-define(FIRST_POS, 0).
-define(INIT_LEN, 0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(bpart, { bin :: binary()
               , pos :: non_neg_integer()
               , len :: non_neg_integer()
               }).
-opaque t() :: #bpart{}.

%%%=====================================================================
%%% API
%%%=====================================================================

new(Params) when is_map(Params) ->
    #bpart{
        bin = maps:get(bin, Params),
        pos = maps:get(pos, Params, ?FIRST_POS),
        len = maps:get(len, Params, ?INIT_LEN)
    }.

reset_pos(Pos, #bpart{} = BPart) ->
    BPart#bpart{
        pos = Pos,
        len = ?INIT_LEN
    }.

incr_pos(N, #bpart{pos = Pos} = BPart) ->
    BPart#bpart{pos = Pos+N}.

incr_len(N, #bpart{len = Len} = BPart) ->
    BPart#bpart{len = Len+N}.

get_part(#bpart{bin = Bin} = BPart) ->
    binary:part(Bin, BPart#bpart.pos, BPart#bpart.len).

get_bin(#bpart{bin = Bin}) ->
    Bin.

set_bin(Bin, #bpart{} = BPart) ->
    BPart#bpart{bin = Bin}.

get_pos(#bpart{pos = Pos}) ->
    Pos.

set_pos(Pos, #bpart{} = BPart) ->
    BPart#bpart{pos = Pos}.

get_len(#bpart{len = Len}) ->
    Len.

set_len(Len, #bpart{} = BPart) ->
    BPart#bpart{len = Len}.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

new_test() ->
    [ { "Should have default values"
      , ?assertEqual(#bpart{
            bin = <<>>,
            pos = ?FIRST_POS,
            len = ?INIT_LEN
        }, new(#{bin => <<>>}))
      }
    , { "Should have params values"
      , ?assertEqual(#bpart{
            bin = <<>>,
            pos = 6,
            len = 6
        }, new(#{
            bin => <<>>,
            pos => 6,
            len => 6
        }))
      }
    ].

cursor_test() ->
    BPart = new(#{bin => <<>>}),
    [ { "Should increment one position"
      , ?assert((incr_pos(1, BPart))#bpart.pos =:= ?FIRST_POS+1)}
    , { "Should increment one length"
      , ?assert((incr_len(1, BPart))#bpart.len =:= ?INIT_LEN+1)}
    ].

get_part_test() ->
    ?assertEqual(<<"bpart">>, get_part(new(#{
        bin => <<"  bpart  ">>,
        pos => 2,
        len => 5
    }))).

-endif.
