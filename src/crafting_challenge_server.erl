-module(crafting_challenge_server).


-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).


-export([sort_example/0]).
-export([script_example/0]).
-export([sort/1]).
-export([script/1]).


-include("../include/crafting_challenge_data.hrl").


-spec start_link() -> {ok , pid()}.
start_link() ->
	gen_server:start_link({local , ?MODULE} , ?MODULE , [] , []).


init([]) ->
	{ok , ok}.

%%======================================================================%%
%%			gen_server callback functions 			%%
%%======================================================================%%


handle_call(sort_example , _From , _State) ->
	Reply = crafting_challenge_utils:sort_data(example),
	{reply , Reply , ok};


handle_call(script_example , _From , _State) ->
	Reply = crafting_challenge_utils:generate_bash_script(example),
	{reply , Reply , ok};


handle_call({sort , BinJson} , _From , _State) ->
	Reply = crafting_challenge_utils:sort_data(BinJson),
	{reply , Reply , ok};


handle_call({script , BinJson} , _From , _State) ->
	Reply = crafting_challenge_utils:generate_bash_script(BinJson),
	{reply , Reply , ok};


handle_call(_Call , _From , _State) ->
	{noreply , ok}.


handle_cast(_Cast , _State) ->
	{noreply , ok}.


handle_info(_Info , _State) ->
	{noreply , ok}.


code_change(_OldVsn , _State , _) ->
	{ok , ok}.


terminate(_Reason , _State) ->
	ok.


%%=======================================================================%%
%%		end of gen_server callback functions			 %%
%%=======================================================================%%


-spec sort_example() -> [{#task_todo{} , pos_integer()}].
sort_example() ->
	gen_server:call(?MODULE , sort_example).


-spec script_example() -> nonempty_binary().
script_example() ->
	gen_server:call(?MODULE , script_example).


-spec sort(nonempty_binary()) -> [{#task_todo{} , pos_integer()}].
sort(BinJson) ->
	gen_server:call(?MODULE , {sort , BinJson}).


-spec script(nonempty_binary()) -> nonempty_binary().
script(BinJson) ->
	gen_server:call(?MODULE , {script , BinJson}).


 
