-module(crafting_challenge).


-export([sort_example/0]).
-export([script_example/0]).
-export([sort/1]).
-export([script/1]).


-include("../include/crafting_challenge_data.hrl").


-spec sort_example() -> [#task{}].
sort_example() ->
	crafting_challenge_server:sort_example().


-spec script_example() -> nonempty_binary().
script_example() ->
	crafting_challenge_server:script_example().


-spec sort(nonempty_binary()) -> [#task{}].
sort(BinJson) ->
	crafting_challenge_server:sort(BinJson).


-spec script(nonempty_binary()) -> nonempty_binary().
script(BinJson) ->
	crafting_challenge_server:script(BinJson).






