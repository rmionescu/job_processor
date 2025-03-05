-module(job_processor_http_test).
-include_lib("eunit/include/eunit.hrl").
-export([run_http_tests/0]).

-define(BASE_URL, "http://localhost:8080/jobs").

%% Read JSON from file
read_json(File) ->
    {ok, Bin} = file:read_file(File),
    binary_to_list(Bin).

%% Make a POST request
make_post_request(File) ->
    inets:start(),  %% Ensure HTTP client is started
    Json = read_json(File),
    Method = post,  %% Fix: Use lowercase atom
    Headers = [{"Content-Type", "application/json"}],
    Url = ?BASE_URL,
    case httpc:request(Method, {Url, Headers, "application/json", Json}, [], []) of
        {ok, {{_, StatusCode, _}, _, Body}} -> {StatusCode, Body};
        Error -> io:format("HTTP Request Failed: ~p~n", [Error]), {error, Error}
    end.

%% Valid sorting test
valid_sorting_test() ->
    {StatusCode, Response} = make_post_request("tests/tasks.json"),
    io:format("Valid sorting test response: ~s~n", [Response]),
    ?assertEqual(200, StatusCode).

%% Cyclic dependency test
cyclic_dependency_test() ->
    {StatusCode, Response} = make_post_request("tests/tasks2.json"),
    io:format("Cyclic dependency test response: ~s~n", [Response]),
    ?assertEqual(400, StatusCode).

%% Minimal valid input test
minimal_valid_test() ->
    {StatusCode, Response} = make_post_request("tests/tasks3.json"),
    io:format("Minimal valid input test response: ~s~n", [Response]),
    ?assertEqual(200, StatusCode).

%% Run all tests
run_http_tests() ->
    valid_sorting_test(),
    cyclic_dependency_test(),
    minimal_valid_test().

