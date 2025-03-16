%%%-------------------------------------------------------------------
%% @doc job_processor public API
%% @end
%%%-------------------------------------------------------------------

-module(job_processor_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    job_processor_logger:init(),
    job_processor_sup:start_link().

stop(_State) ->
    job_processor_logger:info("job_processor_app", "stop", "Server stopped"),
    ok.

%% internal functions
