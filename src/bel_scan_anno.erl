%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc Annotation module.
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
-module(bel_scan_anno).

% API functions
-export([ new/1 ]).

% State getters and setters functions
-export([ get_src/1
        , set_src/2
        , get_loc/1
        , set_loc/2
        , get_end_loc/1
        , set_end_loc/2
        , get_text/1
        , set_text/2
        ]).

-export_type([ t/0, src/0, ln/0, col/0, loc/0, text/0 ]).

-record(anno, { src     :: src()
              , loc     :: loc()
              , end_loc :: loc()
              , text    :: text()
              }).

-opaque t()  :: #anno{}.
-type src()  :: {file, file:filename_all()}
              | {module, module()}
              | string
              .
-type ln()   :: pos_integer().
-type col()  :: pos_integer().
-type loc()  :: bel_scan_loc:t().
-type text() :: binary().

%%%=====================================================================
%%% API functions
%%%=====================================================================

new(Params) when is_map(Params) ->
    #anno{
        src = maps:get(src, Params),
        loc = maps:get(loc, Params),
        end_loc = maps:get(end_loc, Params),
        text = maps:get(text, Params)
    }.

%%%=====================================================================
%%% State getters and setters functions
%%%=====================================================================

get_src(#anno{src = Src}) ->
    Src.

set_src(Src, #anno{} = Anno) ->
    Anno#anno{src = Src}.

get_loc(#anno{loc = Loc}) ->
    Loc.

set_loc(Loc, #anno{} = Anno) ->
    Anno#anno{loc = Loc}.

get_end_loc(#anno{end_loc = EndLoc}) ->
    EndLoc.

set_end_loc(EndLoc, #anno{} = Anno) ->
    Anno#anno{end_loc = EndLoc}.

get_text(#anno{text = Text}) ->
    Text.

set_text(Text, #anno{} = Anno) ->
    Anno#anno{text = Text}.
