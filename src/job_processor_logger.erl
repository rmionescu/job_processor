-module(job_processor_logger).

-export([debug/3, info/3, warn/3, error/3, init/0]).

-define(LOG_FILE, "logs/job_processor.log").

%% Initialize the logger by ensuring the log directory exists
init() ->
    case filelib:is_dir("logs") of
        true -> ok;
        false -> file:make_dir("logs")
    end.

%% Log debug messages
debug(Module, Function, Message) ->
    log("DEBUG", Module, Function, Message).

%% Log info messages
info(Module, Function, Message) ->
    log("INFO", Module, Function, Message).

%% Log warnings
warn(Module, Function, Message) ->
    log("WARNING", Module, Function, Message).

%% Log errors
error(Module, Function, Message) ->
    log("ERROR", Module, Function, Message).

%% Internal log function
log(Level, Module, Function, Message) ->
    Timestamp = timestamp(),
    LogEntry = io_lib:format("[~s] [~s] [~s] [~s] ~s~n", 
                             [Timestamp, Module, Function, Level, Message]),
    file:write_file(?LOG_FILE, LogEntry, [append]).

%% Generate timestamp in format YYYY-MM-DD HH:MM:SS
timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [Y, M, D, H, Min, S]).

