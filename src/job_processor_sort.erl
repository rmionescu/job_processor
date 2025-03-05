-module(job_processor_sort).
-export([sort_tasks/1]).

%% Given a list of Tasks, each with "name", "command" and optionally "requires",
%% sort_tasks/1 returns a list where each task appears after all its dependencies.
sort_tasks(Tasks) ->
    %% Build a graph: a map from task name to {Task, Requires}
    Graph = lists:foldl(fun(Task, Acc) ->
                                Name = maps:get(<<"name">>, Task),
                                Requires = maps:get(<<"requires">>, Task, []),
                                maps:put(Name, {Task, Requires}, Acc)
                          end, #{}, Tasks),

    %% Ensure topo_sort returns a list
    case topo_sort(maps:keys(Graph), Graph, sets:new(), []) of
        {error, {cyclic_dependency, Task}} -> {error, {cyclic_dependency, Task}};
        SortedNames when is_list(SortedNames) ->  %% Ensure this is a list
            [ Task || Name <- SortedNames, maps:is_key(Name, Graph), {Task, _Requires} <- [maps:get(Name, Graph)] ]
    end.

%% topo_sort/4 traverses the graph and produces a sorted list of task names.
topo_sort(Names, Graph, Visited, Sorted) ->
    try lists:foldl(fun(Name, {VAcc, SAcc}) ->
                            case dfs(Name, Graph, VAcc, SAcc, sets:new()) of
                                {error, _} = Err -> throw(Err);
                                {VNew, SNew} -> {VNew, SNew}
                            end
                    end, {Visited, Sorted}, Names) of
        {_, FinalSorted} when is_list(FinalSorted) -> lists:reverse(FinalSorted);
        _ -> throw({error, unexpected_topo_sort_result}) %% Ensure we never return a set
    catch
        throw:{error, {cyclic_dependency, Task}} -> {error, {cyclic_dependency, Task}}
    end.

%% dfs/5 performs depth-first search while detecting cycles.
dfs(Node, Graph, Visited, Sorted, Stack) ->
    case sets:is_element(Node, Stack) of
        true -> {error, {cyclic_dependency, Node}};
        false ->
            case sets:is_element(Node, Visited) of
                true -> {Visited, Sorted};
                false ->
                    NewStack = sets:add_element(Node, Stack),
                    Dependencies = get_requires(Node, Graph),

                    %% Process dependencies and propagate errors immediately
                    case fold_dfs(Dependencies, Graph, Visited, Sorted, NewStack) of
                        {error, _} = Err -> Err;  %% If an error occurred, return it
                        {VisitedAfterDeps, SortedAfterDeps} ->
                            VisitedFinal = sets:add_element(Node, VisitedAfterDeps),
                            {VisitedFinal, [Node | SortedAfterDeps]}
                    end
            end
    end.

%% Helper function to process dependencies recursively and handle errors properly
fold_dfs([], _Graph, Visited, Sorted, _Stack) ->
    {Visited, Sorted};
fold_dfs([Dep | Rest], Graph, Visited, Sorted, Stack) ->
    case dfs(Dep, Graph, Visited, Sorted, Stack) of
        {error, _} = Err -> Err;  %% Propagate error immediately
        {NewVisited, NewSorted} -> fold_dfs(Rest, Graph, NewVisited, NewSorted, Stack)
    end.

%% Helper to get the list of dependencies for a given task.
get_requires(Node, Graph) ->
    case maps:get(Node, Graph) of
         {_, Requires} -> Requires;
         _ -> []
    end.

