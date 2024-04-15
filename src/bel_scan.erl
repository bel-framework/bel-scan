%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Generic scanner.
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
-module(bel_scan).
-compile(inline_list_funcs).

% API
-export([ new/1
        , string/2
        , continue/2
        , terminate/1
        , new_ln/1
        , incr_col/1
        , incr_col/2
        , update_pos/1
        , pos_text/1
        , push_token/2
        , push_tokens/2
        ]).

% State get/set
-export([ get/2
        , set/3
        , get_input/1
        , set_input/2
        , get_handler/1
        , set_handler/2
        , get_metadata/1
        , set_metadata/2
        , get_tokens/1
        , set_tokens/2
        , get_ln/1
        , set_ln/2
        , get_col/1
        , set_col/2
        , get_buffer_pos/1
        , set_buffer_pos/2
        , get_pos/1
        , set_pos/2
        , get_len/1
        , set_len/2
        ]).

-export_type([ t/0
             , options/0
             , metadata/0
             , token/0
             , ln/0
             , col/0
             , position/0
             , length/0
             , result/0
             ]).

% Callbacks

-callback init(options()) -> {ok, metadata()}.

-callback handle_char(char(), binary(), t()) -> t().

-callback handle_tokens([token()], t()) -> result().

% Libs

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Macros

-define(is_ln(X), (
    is_integer(X) andalso X >= 1
)).

-define(is_col(X), (
    is_integer(X) andalso X >= 1
)).

-define(is_position(X), (
    is_integer(X) andalso X >= 0
)).

-define(is_length(X), (
    is_integer(X) andalso X >= 0
)).

-define(is_params(X), (
    is_map(X)
    andalso is_map_key(input, X)
    andalso is_map_key(handler, X)
)).

-define(DEFAULTS, #{
    metadata => undefined,
    tokens => [],
    ln => 1,
    col => 1,
    buffer_pos => 0,
    pos => 0,
    len => 0
}).

% Types

-record(state, { input      :: binary()
               , handler    :: module()
               , metadata   :: metadata()
               , tokens     :: [token()]
               , ln         :: ln()
               , col        :: col()
               , buffer_pos :: position()
               , pos        :: position()
               , len        :: length()
               }).

-opaque t()      :: #state{}.
-type options()  :: term().
-type metadata() :: term().
-type token()    :: term().
-type ln()       :: pos_integer().
-type col()      :: pos_integer().
-type position() :: non_neg_integer().
-type length()   :: non_neg_integer().
-type result()   :: term().

%%%=====================================================================
%%% API
%%%=====================================================================

% Fixes no return warning because of the false positive of the #state{}.
-dialyzer({nowarn_function, [new/1]}).

new(Params) when ?is_params(Params) ->
    maps:fold(fun set/3, #state{}, maps:merge(?DEFAULTS, Params)).

string(Opts, #state{} = State) ->
    Handler = State#state.handler,
    {ok, Metadata} = Handler:init(Opts),
    continue(State#state.input, State#state{metadata = Metadata}).

continue(<<$\r, $\n, Rest/bitstring>>, #state{} = State) ->
    continue(Rest, new_ln(incr_col(State)));
continue(<<$\r, Rest/bitstring>>, #state{} = State) ->
    continue(Rest, new_ln(incr_col(State)));
continue(<<$\n, Rest/bitstring>>, #state{} = State) ->
    continue(Rest, new_ln(incr_col(State)));
continue(<<$\f, Rest/bitstring>>, #state{} = State) ->
    continue(Rest, new_ln(incr_col(State)));
continue(<<Char, Rest/bitstring>>, #state{handler = Handler} = State) ->
    Handler:handle_char(Char, Rest, State);
continue(<<>>, #state{} = State) ->
    terminate(State).

terminate(#state{handler = Handler} = State) ->
    Handler:handle_tokens(State#state.tokens, State).

new_ln(#state{} = State) ->
    State#state{
        ln = State#state.ln+1,
        col = 1
    }.

incr_col(#state{} = State) ->
    incr_col(1, State).

incr_col(N, #state{} = State) when ?is_col(N) ->
    State#state{
        col        = State#state.col+N,
        buffer_pos = State#state.buffer_pos+N,
        len        = State#state.len+N
    }.

update_pos(#state{} = State) ->
    State#state{
        pos = State#state.buffer_pos,
        len = 0
    }.

pos_text(#state{} = State) ->
    binary_part(State#state.input, State#state.pos, State#state.len).

push_token(Token, #state{} = State) ->
    State#state{tokens = [Token | State#state.tokens]}.

push_tokens(Tokens, #state{} = State) when is_list(Tokens) ->
    lists:foldl(fun push_token/2, State, Tokens).

%%%=====================================================================
%%% State get/set
%%%=====================================================================

get(input, State) ->
    get_input(State);
get(handler, State) ->
    get_handler(State);
get(metadata, State) ->
    get_metadata(State);
get(tokens, State) ->
    get_tokens(State);
get(ln, State) ->
    get_ln(State);
get(col, State) ->
    get_col(State);
get(buffer_pos, State) ->
    get_buffer_pos(State);
get(pos, State) ->
    get_pos(State);
get(len, State) ->
    get_len(State).

set(input, Value, State) ->
    set_input(Value, State);
set(handler, Value, State) ->
    set_handler(Value, State);
set(metadata, Value, State) ->
    set_metadata(Value, State);
set(tokens, Value, State) ->
    set_tokens(Value, State);
set(ln, Value, State) ->
    set_ln(Value, State);
set(col, Value, State) ->
    set_col(Value, State);
set(buffer_pos, Value, State) ->
    set_buffer_pos(Value, State);
set(pos, Value, State) ->
    set_pos(Value, State);
set(len, Value, State) ->
    set_len(Value, State).

get_input(#state{input = Input}) ->
    Input.

set_input(Input, #state{} = State) when is_binary(Input) ->
    State#state{input = Input}.

get_handler(#state{handler = Handler}) ->
    Handler.

set_handler(Handler, #state{} = State) when is_atom(Handler) ->
    State#state{handler = Handler}.

get_metadata(#state{metadata = Metadata}) ->
    Metadata.

set_metadata(Metadata, #state{} = State) ->
    State#state{metadata = Metadata}.

get_tokens(#state{tokens = Tokens}) ->
    Tokens.

set_tokens(Tokens, #state{} = State) when is_list(Tokens) ->
    State#state{tokens = Tokens}.

get_ln(#state{ln = Ln}) ->
    Ln.

set_ln(Ln, #state{} = State) when ?is_ln(Ln) ->
    State#state{ln = Ln}.

get_col(#state{col = Col}) ->
    Col.

set_col(Col, #state{} = State) when ?is_col(Col) ->
    State#state{col = Col}.

get_buffer_pos(#state{buffer_pos = BufferPos}) ->
    BufferPos.

set_buffer_pos(BufferPos, #state{} = State) when ?is_position(BufferPos) ->
    State#state{buffer_pos = BufferPos}.

get_pos(#state{pos = Pos}) ->
    Pos.

set_pos(Pos, #state{} = State) when ?is_position(Pos) ->
    State#state{pos = Pos}.

get_len(#state{len = Len}) ->
    Len.

set_len(Len, #state{} = State) when ?is_length(Len) ->
    State#state{len = Len}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!

%%%=====================================================================
%%% Tests
%%% TODO: All kind of missing tests.
%%% TODO: Move tests to "../test/bel_scan_SUITE.erl".
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).

% Callbacks

init([]) ->
    {ok, []}.

handle_char(_Char, Rest, State) ->
    continue(Rest, incr_col(State)).

handle_tokens(_Tokens, State) ->
    State.

% Support

params(Input) ->
    #{input => Input, handler => ?MODULE}.

% Runners

new_test() ->
    [ { "Should raise 'function_clause' when wrong params"
      , ?assertError(function_clause, new(#{}))}
    , { "Should return a valid state"
        , ?assertEqual(#state{
            input      = <<>>,
            handler    = ?MODULE,
            metadata   = maps:get(metadata, ?DEFAULTS),
            tokens     = maps:get(tokens, ?DEFAULTS),
            ln         = maps:get(ln, ?DEFAULTS),
            col        = maps:get(col, ?DEFAULTS),
            buffer_pos = maps:get(buffer_pos, ?DEFAULTS),
            pos        = maps:get(pos, ?DEFAULTS),
            len        = maps:get(len, ?DEFAULTS)
        }, new(params(<<>>)))}
    ].

string_test() ->
    Input = <<"foo\nbar">>,
    State = string([], new(params(Input))),
    [ { "Should scan and return the tokens"
      , ?assertEqual([], get_tokens(State))}
    , { "Should return correct ln"
      , ?assertEqual(2, get_ln(State))}
    , { "Should return correct col"
      , ?assertEqual(4, get_col(State))}
    , { "Should return correct buffer_pos"
      , ?assertEqual(7, get_buffer_pos(State))}
    , { "Should return correct pos"
      , ?assertEqual(0, get_pos(State))}
    , { "Should return correct len"
      , ?assertEqual(7, get_len(State))}
    , { "Should return correct pos_text"
      , ?assertEqual(Input, pos_text(State))}
    ].

-endif.
