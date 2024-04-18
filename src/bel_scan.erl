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

-define(is_lf(X), (
    X =:= $\r orelse X =:= $\n orelse X =:= $\f
)).

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
            bin => maps:get(bin, Params)
        })),
        loc = maps:get(loc, Params, bel_scan_loc:new(#{})),
        tokens = maps:get(tokens, Params, [])
    }.

bin(Bin) ->
    state(new(#{bin => Bin})).

state(#state{bpart = BPart} = State) ->
    continue(scan, bel_scan_bpart:get_bin(BPart), State).

fold(#state{} = State, Funs) ->
    lists:foldl(fun(F, S) -> F(S) end, State, Funs).

text_token(Text, State) ->
    token(text, Text, State).

token(Id, Value, State) ->
    {Id, anno(undefined, State), Value}.

anno(Metadata, _State) ->
    {start_loc, end_loc, Metadata}.

push_token(Token, #state{tokens = Tokens} = State) ->
    State#state{tokens = [Token | Tokens]}.

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

continue(scan, <<>>, State) ->
    terminate(State);
continue(scan, <<Rest0/binary>>, State) ->
    case scan(Rest0) of
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
continue(find_start_markers, <<Rest/binary>>, State) ->
    case find_start_markers(State#state.engines, Rest, []) of
        [] ->
            continue(scan, Rest, State);
        StartMarkers ->
            continue({find_end_markers, StartMarkers}, Rest, State)
    end;
continue({find_end_markers, StartMarkers}, <<Rest0/binary>>, State) ->
    case find_end_markers(StartMarkers, []) of
        [{Mod, [{Match, Rest}]}] ->
            continue({match, {Mod, Match}}, Rest, State);
        [{Mod, [_|_] = EndMarkers}] ->
            error({markers_conflict, {Mod, EndMarkers}}, [
                {find_end_markers, StartMarkers}, Rest0, State
            ]);
        [_|_] = Engs ->
            error({engines_markers_conflict, Engs}, [
                {find_end_markers, StartMarkers}, Rest0, State
            ]);
        [] ->
            error(miss_end_marker, [
                {find_end_markers, StartMarkers}, Rest0, State
            ])
    end;
continue({match, Match}, Rest, State0) ->

    io:format("[MATCH] ~p~n", [get_part(State0#state.bpart)]),

    State = resolve_match(State0#state.engines, Match, State0),
    continue(scan, Rest, State).

% TODO: Check text
% TODO: handle_tokens.
terminate(State) ->

    io:format("[TERMINATE] ~p~n", [get_part(State#state.bpart)]),

    State.

scan(<<$\r, $\n, Rest/binary>>) ->
    {{new_ln, 2}, Rest};
scan(<<Char, Rest/binary>>) when ?is_lf(Char) ->
    {{new_ln, 1}, Rest};
scan(<<_, Rest/binary>>) ->
    {{continue, 1}, Rest};
scan(<<>>) ->
    terminate.

find_start_markers([{Mod, Eng} | Engs], Bin, Acc) ->
    Markers = bel_scan_eng:get_markers(Eng),
    case do_find_start_markers(Markers, Bin, []) of
        [] ->
            find_start_markers(Engs, Bin, Acc);
        StartMarkers ->
            find_start_markers(Engs, Bin, [{Mod, Eng, StartMarkers} | Acc])
    end;
find_start_markers([], _, Acc) ->
    lists:reverse(Acc).

do_find_start_markers([Marker | Markers], Bin, Acc) ->
    case bel_scan_mark:re_start_match(Marker, Bin) of
        {match, {Text, Groups, Rest}} ->
            do_find_start_markers(Markers, Bin, [{Marker, Text, Groups, Rest} | Acc]);
        nomatch ->
            do_find_start_markers(Markers, Bin, Acc)
    end;
do_find_start_markers([], _, Acc) ->
    lists:reverse(Acc).

find_end_markers([{Mod, _Eng, StartMarkers} | Engs], Acc) ->
    case do_find_end_markers(StartMarkers, []) of
        [] ->
            find_end_markers(Engs, Acc);
        MatchMarkers ->
            find_end_markers(Engs, [{Mod, MatchMarkers} | Acc])
    end;
find_end_markers([], Acc) ->
    lists:reverse(Acc).

do_find_end_markers([{Marker, Bin, StartGroups, Rest} | Markers], Acc) ->
    case end_marker_match(Bin, Marker) of
        {true, {Text, EndGroups, <<>>}} ->
            Captured = {StartGroups, EndGroups},
            MarkerId = bel_scan_mark:get_id(Marker),
            Match = {MarkerId, Text, Captured},
            do_find_end_markers(Markers, [{Match, Rest} | Acc]);
        % TODO: Check here. Not tested.
        {true, nomarker} ->
            Captured = {StartGroups, []},
            MarkerId = bel_scan_mark:get_id(Marker),
            Match = {MarkerId, Bin, Captured},
            do_find_end_markers(Markers, [{Match, Rest} | Acc]);
        false ->
            do_find_end_markers(Markers, Acc)
    end;
do_find_end_markers([], Acc) ->
    lists:reverse(Acc).

end_marker_match(<<>>, _Marker) ->
    false;
end_marker_match(Bin, Marker) ->
    case bel_scan_mark:re_end_match(Marker, Bin) of
        {match, {Text, Groups, Rest}} ->
            {true, {Text, Groups, Rest}};
        nomatch ->
            <<_, Rest/binary>> = Bin,
            end_marker_match(Rest, Marker);
        nomarker ->
            {true, nomarker}
    end.

resolve_match([{Mod, _Eng} | Engs], Match, State0) ->
    case Mod:handle_match(Match, State0) of
        {noreply, State} ->
            resolve_match(Engs, Match, State);
        {reply, Tokens, State} ->
            resolve_match(Engs, Match, push_tokens(Tokens, State));
        {halt, State} ->
            State
    end;
resolve_match([], _Match, State) ->
    State.

%%%=====================================================================
%%% Tests
%%%=====================================================================

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).


-endif.
