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
    SortedNames = topo_sort(maps:keys(Graph), Graph, sets:new(), []),
    [ Task || Name <- SortedNames, maps:is_key(Name, Graph), {Task, _Requires} <- [maps:get(Name, Graph)] ].

%% topo_sort/4 traverses the graph and produces a sorted list of task names.
topo_sort(Names, Graph, Visited, Sorted) ->
    {FinalVisited, FinalSorted} =
        lists:foldl(fun(Name, {VAcc, SAcc}) ->
                          {VNew, SNew} = dfs(Name, Graph, VAcc, SAcc, sets:new()),
                          {VNew, SNew}
                  end, {Visited, Sorted}, Names),
    lists:reverse(FinalSorted).

%% dfs/5 performs depth-first search while detecting cycles.
dfs(Node, Graph, Visited, Sorted, Stack) ->
    case sets:is_element(Node, Stack) of
         true -> erlang:error({cyclic_dependency, Node});
         false ->
             case sets:is_element(Node, Visited) of
                  true -> {Visited, Sorted};
                  false ->
                      NewStack = sets:add_element(Node, Stack),
                      Dependencies = get_requires(Node, Graph),
                      {VisitedAfterDeps, SortedAfterDeps} =
                          lists:foldl(fun(Dep, {V, S}) ->
                                          dfs(Dep, Graph, V, S, NewStack)
                                      end, {Visited, Sorted}, Dependencies),
                      VisitedFinal = sets:add_element(Node, VisitedAfterDeps),
                      {VisitedFinal, [Node | SortedAfterDeps]}
             end
    end.

%% Helper to get the list of dependencies for a given task.
get_requires(Node, Graph) ->
    case maps:get(Node, Graph) of
         {_, Requires} -> Requires;
         _ -> []
    end.

