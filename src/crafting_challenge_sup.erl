-module(crafting_challenge_sup).


-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok , pid()}.
start_link() ->
	supervisor:start_link(?MODULE , []).


init([]) ->
	SupFlags =#{
		strategy => one_for_one,
		intensity => 1,
		period => 5
		},

	ChildSpecs =#{
		id => craftingchallengeserver,
		start => {crafting_challenge_server , start_link , []},
		restart => permanent,
		shutdown => brutal_kill,
		type => worker,
		modules => [crafting_challenge_server]
		},

	Children = [ChildSpecs],

	{ok , {SupFlags , Children}}.



