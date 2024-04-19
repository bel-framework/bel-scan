-module(bel_scan_mark).

-export([ compile/1
        , re_match/2
        , get_id/1
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

-opaque t() :: #marker{}.
-type id()  :: term().
-type re()  :: binary() | re:mp().

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
            {match, {Groups, Rest}};
        nomatch ->
            nomatch
    end.

get_id(#marker{id = Id}) ->
    Id.

set_id(Id, #marker{} = Marker) ->
    Marker#marker{id = Id}.

get_re(#marker{re = RE}) ->
    RE.

set_re(RE, #marker{} = Marker) ->
    Marker#marker{re = RE}.
