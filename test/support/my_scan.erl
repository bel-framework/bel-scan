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
                  , update_pos/1
                  , pos_text/1
                  , get_tokens/1
                  ]).

-record(state, {param_loc}).

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

% The Opts arg is any term passed to bel_scan(Opts, State).
% The #state{} is stored in the metadata of the scan state.
init(Opts) when is_map(Opts) ->
    {ok, #state{
        param_loc = maps:get(initial_loc, Opts, {1, 1})
    }}.

handle_char(${, <<${, $\s, Rest/bitstring>>, Scan) ->
    State = update_loc(get_state(Scan), Scan),
    scan_param(Rest, update_pos(incr_col(3, set_state(State, push_text_token(Scan)))));
handle_char(_, Rest, Scan) ->
    continue(Rest, incr_col(Scan)).

handle_tokens(_Tokens, Scan) ->
    lists:reverse(get_tokens(push_text_token(Scan))).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

scan_param(<<$\s, $}, $}, Rest/bitstring>>, Scan0) ->
    Scan = update_pos(incr_col(3, push_param_token(Scan0))),
    State = update_loc(get_state(Scan), Scan),
    continue(Rest, set_state(State, Scan));
scan_param(<<$\r, $\n, Rest/bitstring>>, Scan) ->
    scan_param(Rest, new_ln(incr_col(2, Scan)));
scan_param(<<$\r, Rest/bitstring>>, Scan) ->
    scan_param(Rest, new_ln(incr_col(Scan)));
scan_param(<<$\n, Rest/bitstring>>, Scan) ->
    scan_param(Rest, new_ln(incr_col(Scan)));
scan_param(<<$\f, Rest/bitstring>>, Scan) ->
    scan_param(Rest, new_ln(incr_col(Scan)));
scan_param(<<_, Rest/bitstring>>, Scan) ->
    scan_param(Rest, incr_col(Scan)).

get_state(Scan) ->
    bel_scan:get_metadata(Scan).

set_state(#state{} = State, Scan) ->
    bel_scan:set_metadata(State, Scan).

get_loc(Scan) ->
    (get_state(Scan))#state.param_loc.

update_loc(#state{} = State, Scan) ->
    State#state{param_loc = get_scan_loc(Scan)}.

push_text_token(Scan) ->
    push_token(text_token(Scan), Scan).

push_param_token(Scan) ->
    push_token(param_token(Scan), Scan).

text_token(Scan) ->
    token(text, Scan).

param_token(Scan) ->
    token(param, Scan).

token(Kind, Scan) ->
    {Kind, get_loc(Scan), pos_text(Scan)}.

push_token({_Kind, _Loc, <<>>}, Scan) ->
    Scan;
push_token({_Kind, _Loc, _Text} = Token, Scan) ->
    bel_scan:push_token(Token, Scan).

get_scan_loc(Scan) ->
    Ln = bel_scan:get_ln(Scan),
    Col = bel_scan:get_col(Scan),
    {Ln, Col}.
