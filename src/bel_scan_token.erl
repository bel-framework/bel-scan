% TODO: Delete.
-module(bel_scan_token).

-export([ new/1
        , get_id/1
        , set_id/2
        , get_anno/1
        , set_anno/2
        , get_value/1
        , set_value/2
        ]).

-export_type([ t/0, id/0, anno/0, value/0 ]).

-record(token, {
    id    :: id(),
    anno  :: anno(),
    value :: term()
}).

% -type t()   :: #token{}.
-type t()     :: {id(), anno(), value()}.
-type id()    :: atom().
-type anno()  :: bel_scan_anno:t().
-type value() :: term().

new(Params) when is_map(Params) ->
    #token{
        id = maps:get(id, Params),
        anno = maps:get(anno, Params),
        value = maps:get(value, Params)
    }.

get_id(#token{id = Id}) ->
    Id.

set_id(Id, #token{} = Token) ->
    Token#token{id = Id}.

get_anno(#token{anno = Anno}) ->
    Anno.

set_anno(Anno, #token{} = Token) ->
    Token#token{anno = Anno}.

get_value(#token{value = Value}) ->
    Value.

set_value(Value, #token{} = Token) ->
    Token#token{value = Value}.
