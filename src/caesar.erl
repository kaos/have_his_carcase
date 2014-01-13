%%% @author  Andreas Stenius <git@astekk.se>
%%% @copyright (C) 2014, Andreas Stenius
%%% @doc
%%%
%%% @end
%%% Created :  13 Jan 2014 by Andreas Stenius <git@astekk.se>
%%%
%%% Copyright 2014 Andreas Stenius
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
%%%-------------------------------------------------------------------
-module(caesar).

%% API
-export([encode/2, decode/2]).

%%%===================================================================
%%% API
%%%===================================================================

encode(Text, Shift) when is_integer(Shift) ->
    code(Text, Shift, []).

decode(Text, Shift) when is_integer(Shift) ->
    code(Text, -Shift, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

code([C|Cs], Count, Acc) when $a =< C, C =< $z ->
    code(Cs, Count, [shift($a, C, Count)|Acc]);
code([C|Cs], Count, Acc) when $A =< C, C =< $Z ->
    code(Cs, Count, [shift($A, C, Count)|Acc]);
code([C|Cs], Count, Acc) ->
    code(Cs, Count, [C|Acc]);
code([], _, Acc) ->
    lists:reverse(Acc).

shift(Z, C, Count) ->
    Z + normalize(C - Z + Count).

normalize(C) when C < 0 ->
    normalize(C + ($z - $a) + 1);
normalize(C) when C > ($z - $a) ->
    normalize(C - ($z - $a) - 1);
normalize(C) -> C.
