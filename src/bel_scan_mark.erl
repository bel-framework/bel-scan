%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Marker module.
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
-module(bel_scan_mark).

% API functions
-export([ compile/1, re_match/2 ]).

% State getters and setters functions
-export([ get_id/1
        , set_id/2
        , get_re/1
        , set_re/2
        ]).

-export_type([ t/0, id/0, re/0 ]).

-define(is_re_pattern(X), (
    is_tuple(X)
    andalso tuple_size(X) =:= 5
    andalso element(1, X) =:= re_pattern
)).

-include("bel_scan_eng.hrl").

-type t()  :: #marker{}.
-type id() :: atom().
-type re() :: binary()
            | {re_pattern, _, _, _, _} % re:mp/0 isn't exported.
            .

%%%=====================================================================
%%% API functions
%%%=====================================================================

compile(#marker{} = Marker) ->
    case compile_re(Marker#marker.re) of
        {ok, RE} ->
            Marker#marker{re = RE};
        {error, Reason} ->
            error({re, Reason}, [Marker])
    end.

compile_re(RE) when is_binary(RE) ->
    re:compile(RE, [anchored, multiline, ucp, {newline, anycrlf}]);
compile_re(Pattern) when ?is_re_pattern(Pattern) ->
    {ok, Pattern}.

re_match(#marker{re = RE}, Bin) ->
    case re:run(Bin, RE, [{capture, all, binary}]) of
        {match, [MatchText | Groups]} ->
            <<_:(byte_size(MatchText))/binary, Rest/binary>> = Bin,
            {match, {MatchText, Groups, Rest}};
        nomatch ->
            nomatch
    end.

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_id(#marker{id = Id}) ->
    Id.

set_id(Id, #marker{} = Marker) ->
    Marker#marker{id = Id}.

get_re(#marker{re = RE}) ->
    RE.

set_re(RE, #marker{} = Marker) ->
    Marker#marker{re = RE}.

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

% nothing here yet!
