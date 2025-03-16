%%%-------------------------------------------------------------------
%% @doc job_processor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(job_processor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% Define the Cowboy routing. All POST requests to "/jobs" go to our handler.
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/jobs", job_processor_handler, []}
        ]}
    ]),
    %% Start Cowboy on port 8080 (using cleartext HTTP)
    Port = application:get_env(job_processor, port, 8080),
    {ok, _} = cowboy:start_clear(http_listener,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}),
    {ok, {{one_for_one, 0, 1}, []}}.
