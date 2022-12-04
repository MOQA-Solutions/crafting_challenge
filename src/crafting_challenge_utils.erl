-module(crafting_challenge_utils).


-export([sort_data/1]).
-export([generate_bash_script/1]).


-include("../include/crafting_challenge_data.hrl").


-spec sort_data(example | nonempty_binary()) -> [{#task_todo{} , pos_integer()}].
sort_data(example) ->
	ExampleData = get_example_data(),
	sort_data(ExampleData);


sort_data(BinJson) ->
	ErlangData = binary_to_erlang(BinJson),
	Tasks = maps:get(<<"tasks">> , ErlangData),
	NewTasks = maps_to_records(Tasks),
	sort_tasks(NewTasks).


-spec generate_bash_script(example | nonempty_binary()) -> nonempty_binary().
generate_bash_script(example) ->
	ExampleData = get_example_data(),
	generate_bash_script(ExampleData);


generate_bash_script(BinJson) ->
	SortedData = sort_data(BinJson),
	generate_script(SortedData).	
	

-spec sort_tasks([#task{}]) -> [{#task_todo{} , pos_integer()}].
sort_tasks(Tasks) ->
	GroupedTasks = group_tasks(Tasks),
	Fun = fun({_Task1 , Level1} , {_Task2 , Level2}) ->
		Level1 =< Level2
	      end,
	lists:sort(Fun , GroupedTasks).	


-spec group_tasks([#task{}]) -> [{#task_todo{} , pos_integer()}].
group_tasks(Tasks) ->
	group_tasks(Tasks , [] , []).


group_tasks([] , [] , Res) ->
	Res;


group_tasks([] , ToRetry , Res) ->
	group_tasks(ToRetry , [] , Res);


group_tasks([Task|Tail] , ToRetry , Res) ->
	case valid_task(Task , Res) of
		{true , Level} ->
			group_tasks(Tail , ToRetry , [{task_todo(Task) , Level}|Res]);
		_ ->
			group_tasks(Tail , [Task|ToRetry] , Res)
	end.


-spec valid_task(#task{} , [{#task_todo{} , pos_integer()}]) -> boolean().
valid_task(#task{required_tasks = undefined} , _Tasks) ->
	{true , 0};


valid_task(_Task , []) ->
	false;


valid_task(#task{required_tasks = RequiredTasks} , Tasks) ->
	all_required_tasks_found(RequiredTasks , Tasks).



-spec all_required_tasks_found([nonempty_binary()] , [{#task_todo{} , pos_integer()}]) -> boolean().
all_required_tasks_found(RequiredTasks , TasksAndLevels) ->
	all_required_tasks_found(RequiredTasks , TasksAndLevels , 0).


all_required_tasks_found([] , _Tasks , Level) ->
	{true , Level};


all_required_tasks_found([RequiredTask|Tail] , Tasks , Level1) ->
	case required_task_found(RequiredTask , Tasks) of
		{true , Level2} ->
			NewLevel = if (Level1 >= Level2) -> Level1;
				   true 		 -> Level2
				   end,
			all_required_tasks_found(Tail , Tasks , NewLevel);
		_ ->
			false
	end.

		
-spec required_task_found(nonempty_binary() , [{#task_todo{} , pos_integer()}]) -> boolean().
required_task_found(_RequiredTask , []) ->
	false;


required_task_found(RequiredTask , [{Task , Level}|Tail]) ->
	case (RequiredTask =:= Task#task_todo.name) of
		true ->
			{true , Level+1};
		_ ->
			required_task_found(RequiredTask , Tail)
	end.


-spec task_todo(#task{}) -> #task_todo{}.
task_todo(#task{
		name = Name,
		command = Command
		}) ->
	#task_todo{
		name = Name,
		command = Command
		}.


-spec maps_to_records([map()]) -> [#task{}].
maps_to_records(Maps) ->
	maps_to_records(Maps , []).


maps_to_records([] , Records) ->
	Records;


maps_to_records([ Task | Tail ] , Records) ->
	Name = maps:get(<<"name">> , Task),
	Command = maps:get(<<"command">> , Task),
	RequiredTasks = maps:get(<<"requires">> , Task , undefined),
	NewTask =#task{
		name = Name,
		command = Command,
		required_tasks = RequiredTasks
		},
	maps_to_records(Tail , [NewTask|Records]).


-spec get_example_data() -> nonempty_binary().
get_example_data() ->
	
	<<"
	  
	   {

		\"tasks\":[

				{

					\"name\":\"task-1\",

					\"command\":\"touch /tmp/file1\"

				},

		   		{	

					\"name\":\"task-2\",

					\"command\":\"cat /tmp/file1\",

					\"requires\":[

						\"task-3\"

						]

				},

				{

					\"name\":\"task-3\",

					\"command\":\"echo 'Hello World!' > /tmp/file1\",

					\"requires\":[

						\"task-1\"

						]

				},

				{

					\"name\":\"task-4\",

					\"command\":\"rm /tmp/file1\",

					\"requires\":[

						\"task-2\",

						\"task-3\"

						]

				}

			  ]

	   }

	">>.

	
-spec binary_to_erlang(nonempty_binary()) -> any().
binary_to_erlang(BinJson) ->
	jsone:decode(BinJson).


-spec generate_script([{#task_todo{} , pos_integer()}]) -> nonempty_binary().
generate_script(Tasks) ->
	generate_script(Tasks , <<"#!/usr/bin/env bash\n">>).


generate_script([] , Script) ->
	Script;


generate_script([{#task_todo{
				command = Command
			} , _
		 } | Tail
		] , Script) ->
	NewScript = <<
			Script/binary , 
			Command/binary , 
			"\n"
		    >>,
	generate_script(Tail , NewScript).


