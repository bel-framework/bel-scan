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

-export([ new/1
        , bin/1
        , state/1
        , fold/2
        , token/3
        , text_token/2
        , push_token/2
        , push_tokens/2
        ]).

-export_type([ t/0, input/0 ]).

-import(bel_scan_loc,   [ new_ln/1, incr_col/2 ]).
-import(bel_scan_bpart, [ incr_len/2, get_part/1 ]).

-define(DEFAULT_OPTS, #{}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% TODO: rename loc to start_loc and add end_loc
-record(state, { engines, bpart, loc, tokens }).

-opaque t()   :: #state{}.
-type input() :: binary().

%%%=====================================================================
%%% API
%%%=====================================================================

new(Params) when is_map(Params) ->
    #state{
        engines = init_engines(maps:get(engines, Params, [bel_scan_eel_eng])),
        bpart = maps:get(bpart, Params, bel_scan_bpart:new(#{
            bin => maps:get(bin, Params, <<>>)
        })),
        loc = maps:get(loc, Params, bel_scan_loc:new(#{})),
        tokens = maps:get(tokens, Params, [])
    }.

bin(Bin) ->
    start(Bin, new(#{})).

state(#state{bpart = BPart} = State) ->
    start(bel_scan_bpart:get_bin(BPart), State).

fold(#state{} = State, Funs) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).

text_token(Text, State) ->
    token(text, Text, State).

token(Id, Value, State) ->
    {Id, anno(undefined, State), Value}.

% TODO
anno(Metadata, _State) ->
    {start_loc, end_loc, Metadata}.

% TODO
clear_text(#state{bpart = BPart} = State) ->
    State#state{bpart = bel_scan_bpart:set_len(0, BPart)}.

push_token(Token, #state{tokens = Tokens} = State) ->
    State#state{tokens = Tokens ++ [Token]}.

push_tokens(Tokens, State) when is_list(Tokens) ->
    lists:foldl(fun push_token/2, State, Tokens).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

init_engines(Modules) ->
    [init_engine(Mod) || Mod <- Modules].

init_engine(Mod) when is_atom(Mod) ->
    init_engine({Mod, ?DEFAULT_OPTS});
init_engine({Mod, Opts}) when is_atom(Mod) ->
    {Mod, bel_scan_eng:compile(Mod:init(Opts))}.

start(Bin0, State0) ->
    {Bin, State} = handle_start(State0#state.engines, Bin0, State0),
    BPart = bel_scan_bpart:set_bin(Bin, State#state.bpart),
    continue(scan, Bin, State#state{bpart = BPart}).

continue(scan, <<>>, State) ->
    terminate(State);
continue(scan, <<Rest0/binary>>, State) ->
    case bel_scan_read:bin(Rest0) of
        {{new_ln, Incr}, Rest} ->
            continue(scan, Rest, fold(State, [
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
continue(find_start_markers, <<Rest0/binary>>, State) ->
    case find_marker(State#state.engines, Rest0) of
        {match, {Match, Rest}} ->
            continue({match, Match}, Rest, State);
        nomatch ->
            continue(scan, Rest0, State)
    end;
continue({match, Match}, Rest, State0) ->
    State1 = case get_part(State0#state.bpart) of
        <<>> ->
            State0;
        Text0 ->
            {Text, State2} = handle_text(State0#state.engines, Text0, State0),
            fold(State2, [
                fun(S) -> push_token(text_token(Text, S), S) end,
                fun(S) -> clear_text(S) end
            ])
    end,
    State = handle_match(State0#state.engines, Match, State1),
    continue(scan, Rest, State).

terminate(State0) ->
    State = case get_part(State0#state.bpart) of
        <<>> ->
            State0;
        Text0 ->
            {Text, State1} = handle_text(State0#state.engines, Text0, State0),
            fold(State1, [
                fun(S) -> push_token(text_token(Text, S), S) end,
                fun(S) -> clear_text(S) end
            ])
    end,
    handle_terminate(State#state.engines, State#state.tokens, State).

find_marker([{Mod, Eng} | Engs], Bin) ->
    Markers = bel_scan_eng:get_markers(Eng),
    case do_find_marker(Markers, Bin) of
        {match, {Marker, Groups, Rest}} ->
            MarkerId = bel_scan_mark:get_id(Marker),
            Match = {Mod, MarkerId, Groups},
            {match, {Match, Rest}};
        nomatch ->
            find_marker(Engs, Bin)
    end;
find_marker([], _) ->
    nomatch.

do_find_marker([Marker | Markers], Bin) ->
    case bel_scan_mark:re_match(Marker, Bin) of
        {match, {Groups, Rest}} ->
            {match, {Marker, Groups, Rest}};
        nomatch ->
            do_find_marker(Markers, Bin)
    end;
do_find_marker([], _) ->
    nomatch.

handle_start([{Mod, _Eng} | Engs], Bin0, State0) ->
    case Mod:handle_start(Bin0, State0) of
        {noreply, State} ->
            handle_start(Engs, Bin0, State);
        {reply, Bin, State} ->
            handle_start(Engs, Bin, State);
        {halt, State} ->
            State
    end;
handle_start([], Bin, State) ->
    {Bin, State}.

handle_text([{Mod, _Eng} | Engs], Text0, State0) ->
    case Mod:handle_text(Text0, State0) of
        {noreply, State} ->
            handle_text(Engs, Text0, State);
        {reply, Text, State} ->
            handle_text(Engs, Text, State);
        {halt, State} ->
            State
    end;
handle_text([], Text, State) ->
    {Text, State}.

handle_match([{Mod, _Eng} | Engs], Match, State0) ->
    case Mod:handle_match(Match, State0) of
        {noreply, State} ->
            handle_match(Engs, Match, State);
        {reply, Tokens, State} ->
            handle_match(Engs, Match, push_tokens(Tokens, State));
        {halt, State} ->
            State
    end;
handle_match([], _Match, State) ->
    State.

handle_terminate([{Mod, _Eng} | Engs], Tokens0, State0) ->
    case Mod:handle_terminate(Tokens0, State0) of
        {noreply, State} ->
            handle_terminate(Engs, Tokens0, State);
        {reply, Tokens, State} ->
            handle_terminate(Engs, Tokens, State);
        {halt, State} ->
            State
    end;
handle_terminate([], Tokens, State) ->
    {Tokens, State}.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).


-endif.
