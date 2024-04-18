-module(bel_scan_mark).

-export([ compile/1
        , re_start_match/2
        , re_end_match/2
        , get_id/1
        , set_id/2
        , get_re_start/1
        , set_re_start/2
        , get_re_end/1
        , set_re_end/2
        ]).

-export_type([ t/0, id/0, re_start/0, re_end/0 ]).

-define(is_re_pattern(X), (
    is_tuple(X)
    andalso tuple_size(X) =:= 5
    andalso element(1, X) =:= re_pattern
)).

-include("bel_scan_eng.hrl").

-opaque t()      :: #marker{}.
-type id()       :: term().
-type re()       :: binary() | re:mp().
-type re_start() :: re().
-type re_end()   :: re() | undefined.

compile(#marker{} = Marker) ->
    case compile_re_start(Marker#marker.re_start) of
        {ok, REStart} ->
            case compile_re_end(Marker#marker.re_end) of
                {ok, REEnd} ->
                    Marker#marker{
                        re_start = REStart,
                        re_end = REEnd
                    };
                {error, Reason} ->
                    error({re_end, Reason}, [Marker])
            end;
        {error, Reason} ->
            error({re_start, Reason}, [Marker])
    end.

compile_re_start(RE) when is_binary(RE) ->
    re:compile(RE, [anchored]);
compile_re_start(Pattern) when ?is_re_pattern(Pattern) ->
    {ok, Pattern}.

compile_re_end(RE) when is_binary(RE) ->
    re:compile(RE);
compile_re_end(Pattern) when ?is_re_pattern(Pattern) ->
    {ok, Pattern};
compile_re_end(undefined) ->
    {ok, undefined}.

re_start_match(#marker{re_start = RE}, Bin) ->
    re_match(Bin, RE, []).

re_end_match(#marker{re_end = undefined}, _Bin) ->
    nomarker;
re_end_match(#marker{re_end = RE}, Bin) ->
    re_match(Bin, RE, [anchored]).

re_match(Bin, RE, Opts) ->
    case re:run(Bin, RE, [{capture, all, binary}]) of
        {match, [MatchText | Groups]} ->
            Len = byte_size(Bin) - byte_size(MatchText),
            MarkerText = binary:part(Bin, 0, Len),
            Text = re:replace(MarkerText, RE, <<>>, [{return, binary} | Opts]),
            Rest = binary:part(Bin, Len, Len - byte_size(Text)),
            % TODO: Return a byte_size to adjust the position.
            {match, {Text, Groups, Rest}};
        nomatch ->
            nomatch
    end.

get_id(#marker{id = Id}) ->
    Id.

set_id(Id, #marker{} = Marker) ->
    Marker#marker{id = Id}.

get_re_start(#marker{re_start = REStart}) ->
    REStart.

set_re_start(REStart, #marker{} = Marker) ->
    Marker#marker{re_start = REStart}.

get_re_end(#marker{re_end = REEnd}) ->
    REEnd.

set_re_end(REEnd, #marker{} = Marker) ->
    Marker#marker{re_end = REEnd}.
