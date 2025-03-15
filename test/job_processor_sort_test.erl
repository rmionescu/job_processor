-module(job_processor_sort_test).
-include_lib("eunit/include/eunit.hrl").

sort_tasks_default_test() ->
    % Test case: Default
    DefaultTask = [
        #{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>},
	#{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>, <<"requires">> => [<<"task-3">>]},
	#{<<"name">> => <<"task-3">>, <<"command">> => <<"cmd3">>, <<"requires">> => [<<"task-1">>]},
	#{<<"name">> => <<"task-4">>, <<"command">> => <<"cmd4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
    ],
    ExpectedDefault = [
        #{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>},
	#{<<"name">> => <<"task-3">>, <<"command">> => <<"cmd3">>, <<"requires">> => [<<"task-1">>]},
	#{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>, <<"requires">> => [<<"task-3">>]},
	#{<<"name">> => <<"task-4">>, <<"command">> => <<"cmd4">>, <<"requires">> => [<<"task-2">>, <<"task-3">>]}
    ],
    ?assertEqual(ExpectedDefault, job_processor_sort:sort_tasks(DefaultTask)).

sort_tasks_simple_test() ->
    % Test case: Simple task list without dependencies
    Tasks = [
        #{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>},
        #{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>}
    ],
    Expected = Tasks,
    ?assertEqual(Expected, job_processor_sort:sort_tasks(Tasks)).

sort_tasks_with_deps_test() ->
    % Test case: Tasks with dependencies
    TasksWithDeps = [
        #{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>, <<"requires">> => [<<"task-1">>]},
	#{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>}
    ],
    ExpectedWithDeps = [
        #{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>},
        #{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>, <<"requires">> => [<<"task-1">>]}
    ],
    ?assertEqual(ExpectedWithDeps, job_processor_sort:sort_tasks(TasksWithDeps)).

sort_task_cyclic_test() ->
    % Test case: Detecting cyclic dependencies
    CyclicTasks = [
        #{<<"name">> => <<"task-1">>, <<"command">> => <<"cmd1">>, <<"requires">> => [<<"task-2">>]},
        #{<<"name">> => <<"task-2">>, <<"command">> => <<"cmd2">>, <<"requires">> => [<<"task-1">>]}
    ],
    ?assertMatch({error, {cyclic_dependency, _}}, job_processor_sort:sort_tasks(CyclicTasks)).

