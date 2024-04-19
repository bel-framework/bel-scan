-module(bel_scan_read).

-export([ bin/1 ]).

-define(is_lf(X), (
    X =:= $\r orelse X =:= $\n orelse X =:= $\f
)).

bin(<<$\r, $\n, Rest/binary>>) ->
    {{new_ln, 2}, Rest};
bin(<<Char, Rest/binary>>) when ?is_lf(Char) ->
    {{new_ln, 1}, Rest};
bin(<<_, Rest/binary>>) ->
    {{continue, 1}, Rest};
bin(<<>>) ->
    terminate.
