%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Token module.
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
-module(bel_scan_token).

% API functions
-export([ new/1, to_yecc/1 ]).

% State getters and setters functions
-export([ get_id/1
        , set_id/2
        , get_anno/1
        , set_anno/2
        , get_metadata/1
        , set_metadata/2
        ]).

-export_type([ t/0, id/0, anno/0, metadata/0 ]).

-record(token, { id       :: id()
               , anno     :: anno()
               , metadata :: metadata()
               }).

-opaque t()      :: #token{}.
-type id()       :: atom().
-type anno()     :: bel_scan_anno:t().
-type metadata() :: term().

%%%=====================================================================
%%% API functions
%%%=====================================================================

new(Params) when is_map(Params) ->
    #token{
        id = maps:get(id, Params),
        anno = maps:get(anno, Params),
        metadata = maps:get(metadata, Params, undefined)
    }.

to_yecc(#token{id = Id, anno = Anno, metadata = Metadata}) ->
    {Id, bel_scan_anno:to_yecc(Anno), Metadata}.

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_id(#token{id = Id}) ->
    Id.

set_id(Id, #token{} = Token) ->
    Token#token{id = Id}.

get_anno(#token{anno = Anno}) ->
    Anno.

set_anno(Anno, #token{} = Token) ->
    Token#token{anno = Anno}.

get_metadata(#token{metadata = Metadata}) ->
    Metadata.

set_metadata(Metadata, #token{} = Token) ->
    Token#token{metadata = Metadata}.
