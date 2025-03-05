-module(job_processor_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _Opts) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    job_processor_logger:info("job_processor_handler", "init", io_lib:format("Received ~s request for ~s", [Method, Path])),

    {ok, Body, Req2} = cowboy_req:read_body(Req),
    
    %% Parse the JSON request body using jiffy (with maps returned)
    case jiffy:decode(Body, [return_maps]) of
        {error, _} ->
            %% Return HTTP 400 for invalid JSON
	    job_processor_logger:error("job_processor_handler", "init", "Invalid JSON received"),
            Req3 = cowboy_req:reply(400,
                                    #{<<"content-type">> => <<"application/json">>},
                                    jiffy:encode(#{<<"error">> => <<"Invalid JSON">>}),
                                    Req2),
            {stop, Req3};
        Data ->
            %% Extract "tasks" from the JSON body
            Tasks = maps:get(<<"tasks">>, Data, []),

            case catch job_processor_sort:sort_tasks(Tasks) of
                {'EXIT', {cyclic_dependency, Task}} ->
                    %% Return error if a cyclic dependency is detected
		    job_processor_logger:error("job_processor_handler", "init", "Cyclic dependency detected"),
                    Req3 = cowboy_req:reply(400,
                                            #{<<"content-type">> => <<"application/json">>},
                                            jiffy:encode(#{<<"error">> => <<"Cyclic dependency detected">>,
                                                           <<"task">> => Task}),
                                            Req2),
                    {stop, Req3};
                SortedTasks when is_list(SortedTasks) ->
                    %% Parse query string
                    QsMap = maps:from_list(cowboy_req:parse_qs(Req2)),
                    FormatBin = maps:get(<<"format">>, QsMap, <<"json">>),

                    %% Handle response format
                    case FormatBin of
                        <<"bash">> ->
		            job_processor_logger:info("job_processor_handler", "init", "Returning Bash script"),
                            BashScript = tasks_to_bash(SortedTasks),
                            Req3 = cowboy_req:reply(200,
                                                    #{<<"content-type">> => <<"text/plain">>},
                                                    BashScript,
                                                    Req2),
                            {stop, Req3};
                        _ ->
		            job_processor_logger:info("job_processor_handler", "init", "Returning JSON output"),
			    CleanTasks = [maps:without([<<"requires">>], Task) || Task <- SortedTasks],
                            JSON = jiffy:encode(#{<<"tasks">> => CleanTasks}),
                            Req3 = cowboy_req:reply(200,
                                                    #{<<"content-type">> => <<"application/json">>},
                                                    JSON,
                                                    Req2),
                            {stop, Req3}
                    end;
                _ ->
                    %% Catch-all error response
		    job_processor_logger:error("job_processor_handler", "init", "Unexpected internal error"),
                    Req3 = cowboy_req:reply(500,
                                            #{<<"content-type">> => <<"application/json">>},
                                            jiffy:encode(#{<<"error">> => <<"Internal server error">>}),
                                            Req2),
                    {stop, Req3}
            end
    end.

%% Helper: Convert the sorted tasks into a bash script.
tasks_to_bash(Tasks) ->
    Lines = lists:map(fun(Task) ->
                            Command = maps:get(<<"command">>, Task, <<"">>),
                            binary_to_list(Command) %% Ensure proper text format
                      end, Tasks),
    Script = "#!/usr/bin/env bash\n" ++ string:join(Lines, "\n") ++ "\n",
    list_to_binary(Script).

