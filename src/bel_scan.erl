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
        , skip_new_lns/2
        , terminate/1
        , new_ln/1
        , incr_col/1
        , incr_col/2
        , snapshot/1
        , update_pos/1
        , pos_text/1
        , anno/1
        , anno/2
        , anno/3
        , token/2
        , token/3
        , push_token/2
        , push_tokens/2
        , fold/2
        ]).

% State get/set
-export([ get/2
        , set/3
        , get_input/1
        , set_input/2
        , get_handler/1
        , set_handler/2
        , get_handler_state/1
        , set_handler_state/2
        , get_tokens/1
        , set_tokens/2
        , get_ln/1
        , set_ln/2
        , get_col/1
        , set_col/2
        , get_loc/1
        , set_loc/2
        , get_snap_loc/1
        , set_snap_loc/2
        , get_buffer_pos/1
        , set_buffer_pos/2
        , get_pos/1
        , set_pos/2
        , get_len/1
        , set_len/2
        , get_source/1
        , set_source/2
        ]).

-export_type([ t/0
             , input/0
             , rest/0
             , handler/0
             , handler_opts/0
             , handler_state/0
             , tag/0
             , metadata/0
             , anno/0
             , value/0
             , token/0
             , line/0
             , column/0
             , location/0
             , position/0
             , length/0
             , result/0
             ]).

% Callbacks

-callback init(handler_opts()) -> {ok, handler_state()}.

-callback handle_char(char(), rest(), t()) -> t().

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

-define(is_loc(X), (
    is_tuple(X)
    andalso tuple_size(X) =:= 2
    andalso ?is_ln(element(1, X))
    andalso ?is_col(element(2, X))
)).

-define(is_position(X), (
    is_integer(X) andalso X >= 0
)).

-define(is_length(X), (
    is_integer(X) andalso X >= 0
)).

-define(is_filename(X), (
    is_list(X) orelse is_binary(X)
)).

-define(is_source(X), (
    X =:= undefined
    orelse (
        is_tuple(X)
        andalso tuple_size(X) =:= 2
        andalso (
            (
                element(1, X) =:= file
                andalso ?is_filename(element(2, X))
            )
            orelse (
                element(1, X) =:= module
                andalso is_atom(element(2, X))
            )
        )
    )
)).

-define(valid_params(Input, Handler), (
    is_binary(Input) andalso is_atom(Handler)
)).

-define(is_anno(X), (
    is_tuple(X)
    andalso tuple_size(X) =:= 3
    andalso ?is_loc(element(1, X))
    andalso (
        element(2, X) =:= undefined
        orelse ?is_filename(element(2, X))
    )
)).

-define(is_token(X), (
    is_tuple(X)
    andalso tuple_size(X) =:= 3
    andalso ?is_anno(element(2, X))
)).

-define(DEFAULTS, #{
    handler_state => undefined,
    tokens => [],
    ln => 1,
    col => 1,
    snap_loc => {1, 1},
    buffer_pos => 0,
    pos => 0,
    len => 0,
    source => undefined
}).

% Types

-record(state, { input         :: input()
               , handler       :: handler()
               , handler_state :: handler_state()
               , tokens        :: [token()]
               , ln            :: line()
               , col           :: column()
               , snap_loc      :: location()
               , buffer_pos    :: position()
               , pos           :: position()
               , len           :: length()
               , source        :: source()
               }).

-opaque t()           :: #state{}.
-type input()         :: binary().
-type rest()          :: bitstring().
-type handler()       :: module().
-type handler_opts()  :: term().
-type handler_state() :: term().
-type line()          :: pos_integer().
-type column()        :: pos_integer().
-type location()      :: {line(), column()}.
-type position()      :: non_neg_integer().
-type length()        :: non_neg_integer().
-type tag()           :: term().
-type metadata()      :: term().
-type filename()      :: file:filename_all() | undefined.
-type anno()          :: {location(), source(), metadata()}.
-type value()         :: term().
-type token()         :: {tag(), anno(), value()}.
-type source()        :: {file, filename()}
                       | {module, module()}
                       | undefined
                       .
-type result()        :: term().

%%%=====================================================================
%%% API
%%%=====================================================================

% Fixes no return warning because of the false positive of the #state{}.
-dialyzer({nowarn_function, [new/1]}).

new(#{input := I, handler := H} = Params) when ?valid_params(I, H) ->
    maps:fold(fun set/3, #state{}, maps:merge(?DEFAULTS, Params)).

string(Opts, #state{} = State) ->
    Handler = State#state.handler,
    {ok, HandlerState} = Handler:init(Opts),
    continue(State#state.input, State#state{handler_state = HandlerState}).

continue(<<Rest0/bitstring>>, State0) ->
    case skip_new_lns(Rest0, State0) of
        {ok, {Char, Rest, #state{handler = Handler} = State}} ->
            Handler:handle_char(Char, Rest, State);
        {eof, State} ->
            terminate(State)
    end;
continue(<<>>, #state{} = State) ->
    terminate(State).

skip_new_lns(<<$\r, $\n, Rest/bitstring>>, State) ->
    skip_new_lns(Rest, new_ln(incr_col(State)));
skip_new_lns(<<$\r, Rest/bitstring>>, State) ->
    skip_new_lns(Rest, new_ln(incr_col(State)));
skip_new_lns(<<$\n, Rest/bitstring>>, State) ->
    skip_new_lns(Rest, new_ln(incr_col(State)));
skip_new_lns(<<$\f, Rest/bitstring>>, State) ->
    skip_new_lns(Rest, new_ln(incr_col(State)));
skip_new_lns(<<Char, Rest/bitstring>>, State) ->
    {ok, {Char, Rest, State}};
skip_new_lns(<<>>, State) ->
    {eof, State}.

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
        col        = State#state.col + N,
        buffer_pos = State#state.buffer_pos + N,
        len        = State#state.len + N
    }.

snapshot(#state{} = State) ->
    State#state{
        snap_loc = {State#state.ln, State#state.col}
    }.

update_pos(#state{} = State) ->
    State#state{
        pos = State#state.buffer_pos,
        len = 0
    }.

pos_text(#state{} = State) ->
    binary_part(State#state.input, State#state.pos, State#state.len).

anno(State) ->
    anno(undefined, State).

anno(Metadata, #state{} = State) ->
    anno(get_snap_loc(State), get_source(State), Metadata).

anno(Location, Source, Metadata) when ?is_loc(Location), ?is_source(Source) ->
    {Location, Source, Metadata}.

token(Tag, #state{} = State) ->
    {Tag, anno(State), pos_text(State)}.

token(Tag, Anno, Metadata) when ?is_anno(Anno) ->
    {Tag, Anno, Metadata}.

push_token(Token, #state{} = State) when ?is_token(Token) ->
    State#state{tokens = [Token | State#state.tokens]}.

push_tokens(Tokens, #state{} = State) when is_list(Tokens) ->
    lists:foldl(fun push_token/2, State, Tokens).

fold(#state{} = State, Funs) when is_list(Funs) ->
    lists:foldl(fun(Fun, Acc) when is_function(Fun, 1) ->
        Fun(Acc)
    end, State, Funs).

%%%=====================================================================
%%% State get/set
%%%=====================================================================

get(input, State) ->
    get_input(State);
get(handler, State) ->
    get_handler(State);
get(handler_state, State) ->
    get_handler_state(State);
get(tokens, State) ->
    get_tokens(State);
get(ln, State) ->
    get_ln(State);
get(col, State) ->
    get_col(State);
get(loc, State) ->
    get_loc(State);
get(snap_loc, State) ->
    get_snap_loc(State);
get(buffer_pos, State) ->
    get_buffer_pos(State);
get(pos, State) ->
    get_pos(State);
get(len, State) ->
    get_len(State);
get(source, State) ->
    get_source(State).

set(input, Value, State) ->
    set_input(Value, State);
set(handler, Value, State) ->
    set_handler(Value, State);
set(handler_state, Value, State) ->
    set_handler_state(Value, State);
set(tokens, Value, State) ->
    set_tokens(Value, State);
set(ln, Value, State) ->
    set_ln(Value, State);
set(col, Value, State) ->
    set_col(Value, State);
set(loc, Value, State) ->
    set_loc(Value, State);
set(snap_loc, Value, State) ->
    set_snap_loc(Value, State);
set(buffer_pos, Value, State) ->
    set_buffer_pos(Value, State);
set(pos, Value, State) ->
    set_pos(Value, State);
set(len, Value, State) ->
    set_len(Value, State);
set(source, Value, State) ->
    set_source(Value, State).

get_input(#state{input = Input}) ->
    Input.

set_input(Input, #state{} = State) when is_binary(Input) ->
    State#state{input = Input}.

get_handler(#state{handler = Handler}) ->
    Handler.

set_handler(Handler, #state{} = State) when is_atom(Handler) ->
    State#state{handler = Handler}.

get_handler_state(#state{handler_state = HandlerState}) ->
    HandlerState.

set_handler_state(HandlerState, #state{} = State) ->
    State#state{handler_state = HandlerState}.

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

get_loc(#state{ln = Ln, col = Col}) ->
    {Ln, Col}.

set_loc({Ln, Col}, State) when ?is_ln(Ln), ?is_col(Col) ->
    State#state{
        ln  = Ln,
        col = Col
    }.

get_snap_loc(#state{snap_loc = Col}) ->
    Col.

set_snap_loc({Ln, Col}, #state{} = State) when ?is_ln(Ln), ?is_col(Col) ->
    State#state{snap_loc = {Ln, Col}}.

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

get_source(#state{source = Source}) ->
    Source.

set_source(Source, #state{} = State) when ?is_source(Source) ->
    State#state{source = Source}.

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
    continue(Rest, snapshot(incr_col(State))).

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
            input         = <<>>,
            handler       = ?MODULE,
            handler_state = maps:get(handler_state, ?DEFAULTS),
            tokens        = maps:get(tokens, ?DEFAULTS),
            ln            = maps:get(ln, ?DEFAULTS),
            col           = maps:get(col, ?DEFAULTS),
            snap_loc      = maps:get(snap_loc, ?DEFAULTS),
            buffer_pos    = maps:get(buffer_pos, ?DEFAULTS),
            pos           = maps:get(pos, ?DEFAULTS),
            len           = maps:get(len, ?DEFAULTS),
            source        = maps:get(source, ?DEFAULTS)
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
    , { "Should return correct loc"
      , ?assertEqual({2, 4}, get_loc(State))}
    , { "Should return correct snap_loc"
      , ?assertEqual({2, 4}, get_snap_loc(State))}
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
