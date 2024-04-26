%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc bel_scan tests.
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
-module(bel_scan_SUITE).

% -include_lib("common_test/include/ct.hrl").

%% Callback functions
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ bin/1 ]).

%%%=====================================================================
%%% Callback functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Returns list of tuples to set default properties for the suite.
%%
%% @param Info List of key/value pairs.
%%
%% @end
%%----------------------------------------------------------------------
-spec suite() -> Info when
    Info :: [tuple()].

suite() ->
    [].

%%----------------------------------------------------------------------
%% @doc Initialization before the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_suite(Config0) -> Config when
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_suite(Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_suite(Config) -> Result when
    Config :: [tuple()],
    Result :: term().

end_per_suite(_Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Initialization before each test case.
%%
%% @param TestCase Name of the test case that is about to run.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_testcase(TestCase, Config0) -> Config when
    TestCase :: atom(),
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_testcase(_TestCase, Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after each test case.
%%
%% @param TestCase Name of the test case that is finished.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_testcase(TestCase, Config) -> Result when
    TestCase :: atom(),
    Config :: [tuple()],
    Result :: term().

end_per_testcase(_TestCase, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Returns the list of groups and test cases that are to be executed.
%%
%% @param GroupName Name of a test case group.
%% @param TestCase Name of a test case.
%%
%% @end
%%----------------------------------------------------------------------
-spec all() -> GroupsAndTestCases when
    GroupsAndTestCases :: [Group | TestCase],
    Group :: {group, GroupName},
    GroupName :: atom(),
    TestCase :: atom().

all() ->
    [ bin ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

bin(Config) when is_list(Config) ->
    Opts = #{
        engines => [support_scan_eng]
    },

    SingleLnBin = <<"foo {{ {{A, b}, {0, \"C\"}} }} bar">>,
    [
        {text,{{{1,1},{1,5}},undefined},<<"foo ">>},
        {expr,{{{1,5},{1,29}},undefined},<<"{{A, b}, {0, \"C\"}}">>},
        {text,{{{1,29},{1,33}},undefined},<<" bar">>}
    ] = bel_scan:get_tokens(bel_scan:bin(SingleLnBin, Opts)),

    MultiLnBin = <<"foo
    {{ {{A, b},
        {0, \"C\"}} }}
 bar

{{ {{ {{ d }} }} }}  {{ a

}}
">>,
    [
        {text,{{{1,1},{2,5}},undefined},<<"foo\n    ">>},
        {expr,{{{2,5},{3,21}},undefined},
                <<"{{A, b},\n        {0, \"C\"}}">>},
        {text,{{{3,21},{6,1}},undefined},<<"\n bar\n\n">>},
        {expr,{{{6,1},{6,20}},undefined},<<"{{ {{ d }} }}">>},
        {text,{{{6,20},{6,22}},undefined},<<"  ">>},
        {expr,{{{6,22},{8,3}},undefined},<<"a">>},
        {text,{{{8,3},{9,1}},undefined},<<"\n">>}
    ] = bel_scan:get_tokens(bel_scan:bin(MultiLnBin, Opts)),

    ok.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

% nothing here yet!
