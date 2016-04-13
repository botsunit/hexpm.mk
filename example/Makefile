PROJECT = example
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = cowboy jsx
dep_jsx = hex ~> 2.8.0

include erlang.mk
include ../plugins.mk

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

