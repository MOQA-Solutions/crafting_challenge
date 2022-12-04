-record(task , {

	name::nonempty_binary(),

	command::nonempty_binary(),

	required_tasks::nonempty_binary()|undefined

	}
).

-record(task_todo , {

	name::nonempty_binary(),

	command::nonempty_binary()

	}

).
