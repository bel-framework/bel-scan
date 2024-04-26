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

% API functions
-export([ new/1
        , bin/2
        , state/1
        , state/2
        , fold/2
        , text_token/2
        , text_token/3
        , token/2
        , token/3
        , push_token/2
        , push_tokens/2
        , init_engines/1
        , lookup_engine/2
        ]).

% State getters and setters functions
-export([ get_src/1
        , set_src/2
        , get_engines/1
        , set_engines/2
        , get_bpart/1
        , set_bpart/2
        , get_loc/1
        , set_loc/2
        , get_prev_loc/1
        , set_prev_loc/2
        , get_tokens/1
        , set_tokens/2
        ]).

-export_type([ t/0
             , src/0
             , engine/0
             , bpart/0
             , loc/0
             , token/0
             , token_id/0
             , token_anno/0
             , token_metadata/0
             , token_loc/0
             , token_value/0
             , pos/0
             ]).

-import(bel_scan_loc,   [ new_ln/1, incr_col/2 ]).
-import(bel_scan_bpart, [ incr_len/2, get_part/1 ]).

-include("bel_scan_eng.hrl").

-define(DEFAULT_OPTS, #{}).
-define(DEFAULT_META, undefined).

-record(state, { src      :: src()
               , engines  :: [{module(), engine()}]
               , bpart    :: bpart()
               , loc      :: loc()
               , prev_loc :: loc()
               , tokens   :: [token()]
               , init_pos :: pos()
               }).

-opaque t()            :: #state{}.
-type src()            :: bel_scan_anno:src().
-type engine()         :: bel_scan_eng:t().
-type bpart()          :: bel_scan_bpart:t().
-type loc()            :: bel_scan_loc:t().
-type token()          :: {token_id(), token_anno(), token_value()}.
-type token_id()       :: atom().
-type token_anno()     :: {{token_loc(), token_loc()}, token_metadata()}.
-type token_metadata() :: term().
-type token_loc()      :: bel_scan_loc:pos_tuple().
-type token_value()    :: binary().
-type pos()            :: bel_scan_loc:pos().

%%%=====================================================================
%%% API functions
%%%=====================================================================

new(Params) when is_map(Params) ->
    Loc = maps:get(loc, Params, bel_scan_loc:new(#{})),
    #state{
        src = maps:get(src, Params, string),
        engines = init_engines(maps:get(engines, Params)),
        bpart = maps:get(bpart, Params, bel_scan_bpart:new(#{
            bin => maps:get(bin, Params, <<>>)
        })),
        loc = Loc,
        prev_loc = maps:get(prev_loc, Params, Loc),
        tokens = maps:get(tokens, Params, []),
        init_pos = maps:get(init_pos, Params, bel_scan_loc:get_pos(Loc))
    }.

bin(Bin, Opts) when is_binary(Bin) ->
    start(Bin, new(Opts)).

state(#state{bpart = BPart} = State) ->
    start(bel_scan_bpart:get_bin(BPart), State).

state(Bin, #state{bpart = BPart} = State) when is_binary(Bin) ->
    start(Bin, State#state{
        bpart = bel_scan_bpart:set_bin(Bin, BPart)
    }).

fold(#state{} = State, Funs) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).

text_token(Text, State) ->
    text_token(Text, ?DEFAULT_META, State).

text_token(Text, Metadata, #state{} = State) ->
    Anno = bel_scan_anno:new(#{
        src => State#state.src,
        loc => State#state.prev_loc,
        end_loc => State#state.loc,
        text => Text
    }),
    token(text, Anno, Metadata).

token(Id, Anno) ->
    {Id, Anno, ?DEFAULT_META}.

token(Id, Anno, Metadata) when is_atom(Id) ->
    true = bel_scan_anno:is_anno(Anno),
    {Id, Anno, Metadata}.

push_token(Token, #state{tokens = Tokens} = State) ->
    State#state{tokens = Tokens ++ [Token]}.

push_tokens(Tokens, State) when is_list(Tokens) ->
    lists:foldl(fun push_token/2, State, Tokens).

init_engines(Modules) ->
    [init_engine(Mod) || Mod <- Modules].

lookup_engine(Mod, #state{engines = Engines}) ->
    proplists:lookup(Mod, Engines).

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_src(#state{src = Src}) ->
    Src.

set_src(Src, #state{} = State) ->
    State#state{src = Src}.

get_engines(#state{engines = Engines}) ->
    Engines.

set_engines(Engines, #state{} = State) ->
    State#state{engines = Engines}.

get_bpart(#state{bpart = BPart}) ->
    BPart.

set_bpart(BPart, #state{} = State) ->
    State#state{bpart = BPart}.

get_loc(#state{loc = Loc}) ->
    Loc.

set_loc(Loc, #state{} = State) ->
    State#state{loc = Loc}.

get_prev_loc(#state{prev_loc = PrevLoc}) ->
    PrevLoc.

set_prev_loc(PrevLoc, #state{} = State) ->
    State#state{prev_loc = PrevLoc}.

get_tokens(#state{tokens = Tokens}) ->
    Tokens.

set_tokens(Tokens, #state{} = State) ->
    State#state{tokens = Tokens}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

init_engine(Mod) when is_atom(Mod) ->
    init_engine({Mod, ?DEFAULT_OPTS});
init_engine({Mod, Opts}) when is_atom(Mod), is_map(Opts) ->
    {Mod, bel_scan_eng:compile(Mod:init(Opts))};
init_engine({Mod, #engine{} = Eng}) when is_atom(Mod) ->
    {Mod, Eng}.

start(Bin0, State0) ->
    State = handle_start(Bin0, State0),
    Bin = bel_scan_bpart:get_bin(State#state.bpart),
    continue(find_start_markers, Bin, State).

continue(scan, <<>>, State) ->
    terminate(State);
continue(find_start_markers, <<>>, State) ->
    terminate(State);
continue(scan, <<Rest0/binary>>, State) ->
    case bel_scan_read:bin(Rest0) of
        {{new_ln, Incr}, Rest} ->
            continue(find_start_markers, Rest, fold(State, [
                fun(S) -> S#state{loc = new_ln(S#state.loc)} end,
                fun(S) -> S#state{bpart = incr_len(Incr, S#state.bpart)} end
            ]));
        {{continue, Incr}, Rest} ->
            continue(find_start_markers, Rest, fold(State, [
                fun(S) -> S#state{loc = incr_col(Incr, S#state.loc)} end,
                fun(S) -> S#state{bpart = incr_len(Incr, S#state.bpart)} end
            ]));
        terminate ->
            terminate(State)
    end;
continue(find_start_markers, <<Rest0/binary>>, State0) ->
    case find_marker(State0#state.engines, Rest0) of
        {match, {Mod, MarkerId, MatchText, Captured, Rest}} ->
            State1 = handle_text(State0),
            Loc = State1#state.loc,
            EndLoc = bel_scan_loc:read(MatchText, State1#state.loc),
            Anno = bel_scan_anno:new(#{
                src => State1#state.src,
                loc => Loc,
                end_loc => EndLoc,
                text => MatchText
            }),
            Match = {Mod, MarkerId, Captured, Anno},
            Pos = bel_scan_loc:get_pos(EndLoc),
            BPart = reset_bpart_pos(Pos, State1),
            continue({handle_match, Match}, Rest, State1#state{
                loc = EndLoc,
                prev_loc = EndLoc,
                bpart = BPart
            });
        nomatch ->
            continue(scan, Rest0, State0)
    end;
continue({handle_match, Match}, Rest, State0) ->
    State = handle_match(Match, State0),
    continue(find_start_markers, Rest, State).

terminate(State0) ->
    State = handle_text(State0),
    handle_terminate(State).

find_marker([{Mod, Eng} | Engs], Bin) ->
    Markers = bel_scan_eng:get_markers(Eng),
    case do_find_marker(Markers, Bin) of
        {match, {Marker, MatchText, Captured, Rest}} ->
            MarkerId = bel_scan_mark:get_id(Marker),
            {match, {Mod, MarkerId, MatchText, Captured, Rest}};
        nomatch ->
            find_marker(Engs, Bin)
    end;
find_marker([], _) ->
    nomatch.

do_find_marker([Marker | Markers], Bin) ->
    case bel_scan_mark:re_match(Marker, Bin) of
        {match, {MatchText, Captured, Rest}} ->
            {match, {Marker, MatchText, Captured, Rest}};
        nomatch ->
            do_find_marker(Markers, Bin)
    end;
do_find_marker([], _) ->
    nomatch.

handle_start(Bin, State) ->
    do_handle_start(State#state.engines, Bin, State).

do_handle_start([{Mod, _Eng} | Engs], Bin0, State0) ->
    case Mod:handle_start(Bin0, State0) of
        {noreply, State} ->
            do_handle_start(Engs, Bin0, State);
        {reply, Bin, State} ->
            do_handle_start(Engs, Bin, State);
        {halt, State} ->
            State
    end;
do_handle_start([], Bin, State) ->
    BPart = bel_scan_bpart:set_bin(Bin, State#state.bpart),
    State#state{bpart = BPart}.

handle_text(State) ->
    handle_text(get_part(State#state.bpart), State).

handle_text(<<>>, State) ->
    State;
handle_text(Text, State) ->
    do_handle_text(State#state.engines, Text, State).

do_handle_text([{Mod, _Eng} | Engs], Text0, State0) ->
    case Mod:handle_text(Text0, State0) of
        {noreply, State} ->
            do_handle_text(Engs, Text0, State);
        {reply, Text, State} ->
            do_handle_text(Engs, Text, State);
        {halt, State} ->
            State
    end;
do_handle_text([], Text, State) ->
    fold(State, [
        fun(S) -> push_token(text_token(Text, S), S) end,
        fun(S) -> clear_text(S) end
    ]).

handle_match(Match, State) ->
    do_handle_match(State#state.engines, Match, State).

do_handle_match([{Mod, _Eng} | Engs], Match, State0) ->
    case Mod:handle_match(Match, State0) of
        {noreply, State} ->
            do_handle_match(Engs, Match, State);
        {reply, Tokens, State} ->
            do_handle_match(Engs, Match, push_tokens(Tokens, State));
        {halt, State} ->
            State
    end;
do_handle_match([], _Match, State) ->
    State.

handle_terminate(State) ->
    do_handle_terminate(State#state.engines, State#state.tokens, State).

do_handle_terminate([{Mod, _Eng} | Engs], Tokens0, State0) ->
    case Mod:handle_terminate(Tokens0, State0) of
        {noreply, State} ->
            do_handle_terminate(Engs, Tokens0, State);
        {reply, Tokens, State} ->
            do_handle_terminate(Engs, Tokens, State);
        {halt, State} ->
            State
    end;
do_handle_terminate([], Tokens, State) ->
    State#state{tokens = Tokens}.

clear_text(#state{loc = Loc} = State) ->
    Pos = bel_scan_loc:get_pos(Loc),
    State#state{bpart = reset_bpart_pos(Pos, State)}.

reset_bpart_pos(Pos, #state{init_pos = InitPos, bpart = BPart}) ->
    bel_scan_bpart:reset_pos(Pos - InitPos, BPart).
