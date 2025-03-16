-module(job_processor_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, _Opts) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    job_processor_logger:info("job_processor_handler", "init", io_lib:format("Received ~s request for ~s", [Method, Path])),

    {ok, Body, Req2} = cowboy_req:read_body(Req),

    case decode_json(Body) of
        {error, _} ->
            handle_reply(400, <<"Invalid JSON">>, Req2);
        #{<<"tasks">> := Tasks} ->
            handle_tasks(Tasks, Req2);
        _ ->
            handle_reply(400, <<"Missing 'tasks' field in request">>, Req2)
    end.

%% Helper function to decode JSON safely
decode_json(Body) ->
    case jiffy:decode(Body, [return_maps]) of
        {error, _} -> {error, invalid_json};
        Data -> Data
    end.

%% Handle the processing of tasks
handle_tasks(Tasks, Req) ->
    case job_processor_sort:sort_tasks(Tasks) of
        {error, {cyclic_dependency, Task}} ->
            handle_reply(400, <<"Cyclic dependency detected">>, Req, #{<<"task">> => Task});
        SortedTasks when is_list(SortedTasks) ->
            handle_format(SortedTasks, Req);
        _ ->
            handle_reply(500, <<"Internal server error">>, Req)
    end.

%% Response format
handle_format(SortedTasks, Req) ->
    QsMap = maps:from_list(cowboy_req:parse_qs(Req)),
    FormatBin = maps:get(<<"format">>, QsMap, <<"json">>),

    case FormatBin of
        <<"bash">> ->
            job_processor_logger:info("job_processor_handler", "init", "Returning Bash script"),
            BashScript = tasks_to_bash(SortedTasks),
            handle_reply(200, BashScript, Req, #{<<"content-type">> => <<"text/plain">>});
        _ ->
            job_processor_logger:info("job_processor_handler", "init", "Returning JSON output"),
            CleanTasks = [maps:without([<<"requires">>], Task) || Task <- SortedTasks],
            JSON = jiffy:encode(#{<<"tasks">> => CleanTasks}),
            handle_reply(200, JSON, Req)
    end.

%% Reply to the client
handle_reply(Status, ResponseData, Req) ->
    ContentType = #{<<"content-type">> => <<"application/json">>},
    handle_reply(Status, ResponseData, Req, ContentType).

handle_reply(Status, ErrorMessage, Req, Headers) when Status =/= 200 ->
    %% For errors, wrap message inside an error object
    ErrorJson = jiffy:encode(#{<<"error">> => ErrorMessage}),
    {stop, cowboy_req:reply(Status, Headers, ErrorJson, Req)};

handle_reply(200, ResponseData, Req, Headers) ->
    {stop, cowboy_req:reply(200, Headers, ResponseData, Req)}.


%% Convert sorted tasks into a Bash script
tasks_to_bash(Tasks) ->
    Lines = [binary_to_list(maps:get(<<"command">>, Task, <<"">>)) || Task <- Tasks],
    Script = "#!/usr/bin/env bash\n" ++ string:join(Lines, "\n") ++ "\n",
    list_to_binary(Script).

