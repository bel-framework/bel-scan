-module(my_scan).
-behaviour(bel_scan).

% API
-export([ string/1 ]).

% bel_scan callbacks
-export([ init/1, handle_char/3, handle_tokens/2 ]).

-import(bel_scan, [ incr_col/1
                  , incr_col/2
                  , new_ln/1
                  , continue/2
                  , skip_new_lns/2
                  , update_pos/1
                  , token/2
                  , push_token/2
                  , snapshot/1
                  , fold/2
                  ]).

-record(state, {}).

%%%=====================================================================
%%% API
%%%=====================================================================

string(Text) ->
    Scan = bel_scan:new(#{
        input => Text,
        handler => ?MODULE
    }),
    bel_scan:string(#{}, Scan).

%%%=====================================================================
%%% bel_scan callbacks
%%%=====================================================================

init(Opts) when is_map(Opts) ->
    {ok, #state{}}.

handle_char(${, <<${, $\s, Rest/bitstring>>, Scan) ->
    scan_param(Rest, fold(Scan, [
        fun(S) -> push_token(token(text, S), S) end,
        fun(S) -> snapshot(S) end,
        fun(S) -> incr_col(3, S) end,
        fun(S) -> update_pos(S) end
    ]));
handle_char(_Char, <<>>, Scan) ->
    continue(<<>>, fold(Scan, [
        fun(S) -> incr_col(S) end,
        fun(S) -> push_token(token(text, S), S) end
    ]));
handle_char(_Char, <<Rest/bitstring>>, Scan) ->
    continue(Rest, incr_col(Scan)).

handle_tokens(Tokens, _Scan) ->
    lists:reverse(Tokens).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

scan_param(<<$\s, $}, $}, Rest/bitstring>>, Scan) ->
    continue(Rest, fold(Scan, [
        fun(S) -> push_token(token(param, S), S) end,
        fun(S) -> incr_col(3, S) end,
        fun(S) -> update_pos(S) end,
        fun(S) -> snapshot(S) end
    ]));
scan_param(<<Rest0/bitstring>>, Scan0) ->
    {ok, {_Char, Rest, Scan}} = skip_new_lns(Rest0, Scan0),
    scan_param(Rest, incr_col(Scan)).
