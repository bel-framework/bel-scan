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
-include_lib("stdlib/include/assert.hrl").

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
    ?assertEqual([
        {token,text,
            {anno,string,{loc,0,1,1,1,1},{loc,4,1,5,1,1},<<"foo ">>},
            undefined},
        {token,expr,
            {anno,string,
                    {loc,4,1,5,1,1},
                    {loc,28,1,29,1,1},
                    <<"{{ {{A, b}, {0, \"C\"}} }}">>},
            [{tuple,1,
                    [{tuple,1,[{var,1,'A'},{atom,1,b}]},
                        {tuple,1,[{integer,1,0},{string,1,"C"}]}]}]},
        {token,text,
            {anno,string,
                    {loc,28,1,29,1,1},
                    {loc,32,1,33,1,1},
                    <<" bar">>},
            undefined}
    ], bel_scan:get_tokens(bel_scan:bin(SingleLnBin, Opts))),

    MultiLnBin = <<"foo
    {{ {{A, b},
        {0, \"C\"}} }}
 bar

{{ {{ {{ d }} }} }}  {{ a

}}
">>,
    ?assertEqual([
        {token,text,
            {anno,string,
                    {loc,0,1,1,1,1},
                    {loc,8,2,5,1,1},
                    <<"foo\n    ">>},
            undefined},
        {token,expr,
            {anno,string,
                    {loc,8,2,5,1,1},
                    {loc,40,3,21,1,1},
                    <<"{{ {{A, b},\n        {0, \"C\"}} }}">>},
            [{tuple,1,
                    [{tuple,1,[{var,1,'A'},{atom,1,b}]},
                        {tuple,2,[{integer,2,0},{string,2,"C"}]}]}]},
        {token,text,
            {anno,string,
                    {loc,40,3,21,1,1},
                    {loc,47,6,1,1,1},
                    <<"\n bar\n\n">>},
            undefined},
        {token,expr,
            {anno,string,
                    {loc,47,6,1,1,1},
                    {loc,66,6,20,1,1},
                    <<"{{ {{ {{ d }} }} }}">>},
            [{tuple,1,
                    [{tuple,1,
                            [{tuple,1,[{tuple,1,[{atom,1,d}]}]}]}]}]},
        {token,text,
            {anno,string,{loc,66,6,20,1,1},{loc,68,6,22,1,1},<<"  ">>},
            undefined},
        {token,expr,
            {anno,string,
                    {loc,68,6,22,1,1},
                    {loc,76,8,3,1,1},
                    <<"{{ a\n\n}}">>},
            [{atom,1,a}]},
        {token,text,
            {anno,string,{loc,76,8,3,1,1},{loc,77,9,1,1,1},<<"\n">>},
            undefined}
    ], bel_scan:get_tokens(bel_scan:bin(MultiLnBin, Opts))),

    ok.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

% nothing here yet!
