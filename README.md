## 1. Install rebar3
```
git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
cp rebar3 /usr/local/bin/
```

## 2. Install C compiler
#### Required for compiling dependencies like jiffy (which includes C code).
```
vboxuser@Ubuntu:~/job_processor$ sudo apt install build-essential
```

## 3. Create a new app
#### "app" is used because we are building a modular application, not a full OTP release.
```
vboxuser@Ubuntu:~$ rebar3 new app job_processor
===> Writing job_processor/src/job_processor_app.erl
===> Writing job_processor/src/job_processor_sup.erl
===> Writing job_processor/src/job_processor.app.src
===> Writing job_processor/rebar.config
===> Writing job_processor/.gitignore
===> Writing job_processor/LICENSE.md
===> Writing job_processor/README.md
```

## 4. Add dependencies
#### Cowboy - HTTP server for handling requests
#### Jiffy  - JSON parser for encoding/decoding JSON
```
vboxuser@Ubuntu:~/job_processor$ cat rebar.config
{erl_opts, [debug_info]}.
{deps, [
    {cowboy, "2.12.0"},
    {jiffy, "1.1.2"}
]}.

{shell, [
    %% {config, "config/sys.config"},
    {apps, [job_processor]}
]}.
```

## 5. Fetch dependencies
```
vboxuser@Ubuntu:~/job_processor$ rebar3 get-deps
===> Verifying dependencies...
===> Fetching cowboy v2.12.0
===> Fetching jiffy v1.1.2
===> Fetching pc v1.15.0
===> Analyzing applications...
===> Compiling pc
===> Fetching cowlib v2.13.0
===> Fetching ranch v1.8.0
```

## 6. Compile the project
```
vboxuser@Ubuntu:~/job_processor$ rebar3 compile
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling jiffy
===> Compiling ranch
===> Compiling cowlib
===> Compiling cowboy
===> Compiling c_src/decoder.c
===> Compiling c_src/encoder.c
===> Compiling c_src/jiffy.c
===> Compiling c_src/termstack.c
===> Compiling c_src/utf8.c
===> Compiling c_src/util.c
===> Compiling c_src/doubles.cc
===> Compiling c_src/objects.cc
===> Compiling c_src/double-conversion/bignum-dtoa.cc
===> Compiling c_src/double-conversion/bignum.cc
===> Compiling c_src/double-conversion/cached-powers.cc
===> Compiling c_src/double-conversion/diy-fp.cc
===> Compiling c_src/double-conversion/double-conversion.cc
===> Compiling c_src/double-conversion/fast-dtoa.cc
===> Compiling c_src/double-conversion/fixed-dtoa.cc
===> Compiling c_src/double-conversion/strtod.cc
===> Linking /home/vboxuser/job_processor/_build/default/lib/jiffy/priv/jiffy.so
===> Analyzing applications...
===> Compiling job_processor
```

## 7. Start the web server
```
vboxuser@Ubuntu:~/job_processor$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling job_processor
Erlang/OTP 25 [erts-13.2.2.9] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns]

Eshell V13.2.2.9  (abort with ^G)
1> ===> Booted cowlib
===> Booted ranch
===> Booted cowboy
===> Booted xmerl
===> Booted jiffy
===> Booted job_processor
```

## 8. Call the api
#### json response
```
curl -X POST http://localhost:8080/jobs -H "Content-Type: application/json" -d @tests/tasks.json
```

#### bash response
```
curl -X POST "http://localhost:8080/jobs?format=bash" -H "Content-Type: application/json" -d @tests/tasks.json
```
