-module(crafting_challenge_app).


-behaviour(application).


-export([start/2]).
-export([stop/1]).


-spec start(_Type , _Args) -> {ok , pid()}.
start(_Type , _Args) ->
	crafting_challenge_sup:start_link().


-spec stop(_) -> ok.
stop(_) ->
	ok.


