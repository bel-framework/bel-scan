-module(bel_parser_example).
-behaviour(bel_parser).

% API
-export([ parse/1 ]).

% bel_parser callbacks
-export([ init/1, handle_parse/3, handle_tokens/2 ]).

-import(bel_parser, [ incr_col/1
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

parse(Text) ->
    Parser = bel_parser:new(#{
        input => Text,
        handler => ?MODULE
    }),
    bel_parser:parse(#{}, Parser).

%%%=====================================================================
%%% bel_parser callbacks
%%%=====================================================================

% The Opts arg is any term passed to bel_parser(Opts, State).
% The #state{} is stored in the metadata of the parser state.
init(Opts) when is_map(Opts) ->
    {ok, #state{
        param_loc = maps:get(initial_loc, Opts, {1, 1})
    }}.

handle_parse(${, <<${, $\s, Rest/bitstring>>, Parser) ->
    State = update_loc(get_state(Parser), Parser),
    parse_param(Rest, update_pos(incr_col(3, set_state(State, push_text_token(Parser)))));
handle_parse(_, Rest, Parser) ->
    continue(Rest, incr_col(Parser)).

handle_tokens(_Tokens, Parser) ->
    lists:reverse(get_tokens(push_text_token(Parser))).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

parse_param(<<$\s, $}, $}, Rest/bitstring>>, Parser0) ->
    Parser = update_pos(incr_col(3, push_param_token(Parser0))),
    State = update_loc(get_state(Parser), Parser),
    continue(Rest, set_state(State, Parser));
parse_param(<<$\r, $\n, Rest/bitstring>>, Parser) ->
    parse_param(Rest, new_ln(incr_col(2, Parser)));
parse_param(<<$\r, Rest/bitstring>>, Parser) ->
    parse_param(Rest, new_ln(incr_col(Parser)));
parse_param(<<$\n, Rest/bitstring>>, Parser) ->
    parse_param(Rest, new_ln(incr_col(Parser)));
parse_param(<<$\f, Rest/bitstring>>, Parser) ->
    parse_param(Rest, new_ln(incr_col(Parser)));
parse_param(<<_, Rest/bitstring>>, Parser) ->
    parse_param(Rest, incr_col(Parser)).

get_state(Parser) ->
    bel_parser:get_metadata(Parser).

set_state(#state{} = State, Parser) ->
    bel_parser:set_metadata(State, Parser).

get_loc(Parser) ->
    (get_state(Parser))#state.param_loc.

update_loc(#state{} = State, Parser) ->
    State#state{param_loc = get_parser_loc(Parser)}.

push_text_token(Parser) ->
    push_token(text_token(Parser), Parser).

push_param_token(Parser) ->
    push_token(param_token(Parser), Parser).

text_token(Parser) ->
    token(text, Parser).

param_token(Parser) ->
    token(param, Parser).

token(Kind, Parser) ->
    {Kind, get_loc(Parser), pos_text(Parser)}.

push_token({_Kind, _Loc, <<>>}, Parser) ->
    Parser;
push_token({_Kind, _Loc, _Text} = Token, Parser) ->
    bel_parser:push_token(Token, Parser).

get_parser_loc(Parser) ->
    Ln = bel_parser:get_ln(Parser),
    Col = bel_parser:get_col(Parser),
    {Ln, Col}.
