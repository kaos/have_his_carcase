%%% @author  Andreas Stenius <git@astekk.se>
%%% @copyright (C) 2014, Andreas Stenius, Pernilla Almgren
%%% @doc
%%%
%%% @end
%%% Created :  5 Jan 2014 by Andreas Stenius <git@astekk.se>, Pernilla Almgren
%%%
%%% Copyright 2014 Andreas Stenius, Pernilla Almgren
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
-module(carcase).

%% API
-export([new/1, new/2, encode/2, decode/2, print/1]).

%%%===================================================================
%%% API
%%%===================================================================

new(Key) when is_list(Key) ->
    new(Key, default).

new(Key, Alphabet) when is_list(Key) ->
    Table =
	if is_atom(Alphabet) ->
		get_alphabet(Alphabet);
	   is_list(Alphabet) ->
		Alphabet
	end,
    create_table(string:to_upper(Key), Table).

print(Table) ->
    io:format("~p~n",
	      [[element(2, lists:unzip(Row))
		|| Row <- element(2, lists:unzip(Table))]]).

encode(Text, Table) ->
    encode(string:to_upper(Text), Table, []).

decode(Text, Table) ->
    decode(Text, Table, []).

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_alphabet(default) ->
    lists:flatten(
      [lists:seq($A, $Z),
       $Å, $Ä, $Ö, $\s,
       $,, $., $?, $!,
       $(, $)
      ]).

encode([A, B|Rest], Table, Acc) ->
    Source =
	case [lookup(C, Table) || C <- [A, B]] of
	    [{Row, PosA}, {Row, PosB}] ->
		Len = length(Table),
		[{Row, (Pos + 1) rem Len} || Pos <- [PosA, PosB]];
	    [{RowA, Pos}, {RowB, Pos}] ->
		Len = length(Table),
		[{(Row + 1) rem Len, Pos} || Row <- [RowA, RowB]];
	    [{RowA, PosA}, {RowB, PosB}] ->
		[{RowA, PosB}, {RowB, PosA}]
	end,

    encode(Rest, Table,
	   [Acc|[table_col(Pos, table_row(Row, Table))
		 || {Row, Pos} <- Source]
	   ]);
encode([C], Table, Acc) ->
    encode([], Table, [Acc, C]);
encode([], _, Acc) ->
    lists:flatten(Acc).

decode([A, B|Rest], Table, Acc) ->
    Source =
	case [lookup(C, Table) || C <- [A, B]] of
	    [{Row, PosA}, {Row, PosB}] ->
		Len = length(Table),
		[{Row, (Pos + Len - 1) rem Len} || Pos <- [PosA, PosB]];
	    [{RowA, Pos}, {RowB, Pos}] ->
		Len = length(Table),
		[{(Row + Len - 1) rem Len, Pos} || Row <- [RowA, RowB]];
	    [{RowA, PosA}, {RowB, PosB}] ->
		[{RowA, PosB}, {RowB, PosA}]
	end,

    decode(Rest, Table,
	   [Acc|[table_col(Pos, table_row(Row, Table))
		 || {Row, Pos} <- Source]
	   ]);
decode([C], Table, Acc) ->
    decode([], Table, [Acc, C]);
decode([], _, Acc) ->
    lists:flatten(Acc).
    
table_row(Row, Table) ->
    element(2, lists:nth(Row + 1, Table)).

table_col(Col, Row) ->
    element(2, lists:nth(Col + 1, Row)).

lookup(Char, [{Id, Row}|Table]) ->
    case lists:keyfind(Char, 2, Row) of
	{Pos, Char} -> {Id, Pos};
	false -> lookup(Char, Table)
    end.

create_table(Key, Alphabet) ->
    Len = length(Alphabet),
    Dim = trunc(math:sqrt(Len)),
    if Dim * Dim == Len ->
	    KeyLen = length(Key),
	    if KeyLen > Dim ->
		    split_alphabet(
		      Dim, Dim, 
		      key_alphabet(Key, Alphabet),
		      []);
	       true -> error({key_too_short, Key})
	    end;
       true -> error({bad_alphabet_size, Len})
    end.

key_alphabet(Key, Alphabet) ->
    case Key ++ (Alphabet -- Key) of
	Keyed when length(Keyed) == length(Alphabet) ->
	    Keyed;
	_ ->
	    error({invalid_key, Key})
    end.

split_alphabet(_, 0, [], Acc) ->
    lists:reverse(Acc);
split_alphabet(Dim, Count, Alphabet, Acc) ->
    {Row, Rest} = lists:split(Dim, Alphabet),
    split_alphabet(
      Dim, Count - 1, Rest,
      [{Dim - Count, lists:zip(lists:seq(0, Dim - 1), Row)}
       |Acc]).
