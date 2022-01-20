# HINT
# Use this file to override variables here.
# For example, to run with podman put `DOCKER=podman` there.
-include Makefile.env

SERVICE := dominant

# NOTE
# Variables specified in `.env` file are used to pick and setup specific
# component versions, both when building a development image and when running
# CI workflows on GH Actions. This ensures that tasks run with `wc-` prefix
# (like `wc-dialyze`) are reproducible between local machine and CI runners.
DOTENV := $(shell grep -v '^\#' .env)

DOCKER ?= docker
DOCKERCOMPOSE ?= docker-compose
REBAR ?= rebar3

all: compile

# Development images

DEV_IMAGE_TAG = $(SERVICE)-dev
DEV_IMAGE_ID = $(file < .image.dev)

.PHONY: dev-image clean-dev-image wc-shell test

dev-image: .image.dev

.image.dev: Dockerfile.dev .env
	env $(DOTENV) DEV_IMAGE_TAG=$(DEV_IMAGE_TAG) $(DOCKERCOMPOSE) build $(SERVICE)
	$(DOCKER) image ls -q -f "reference=$(DEV_IMAGE_ID)" | head -n1 > $@

clean-dev-image:
ifneq ($(DEV_IMAGE_ID),)
	$(DOCKER) image rm -f $(DEV_IMAGE_TAG)
	rm .image.dev
endif

DOCKER_WC_OPTIONS := -v $(PWD):$(PWD) --workdir $(PWD)
DOCKER_WC_EXTRA_OPTIONS ?= --rm
DOCKER_RUN = $(DOCKER) run $(DOCKER_WC_OPTIONS) $(DOCKER_WC_EXTRA_OPTIONS)

DOCKERCOMPOSE_RUN = DEV_IMAGE_TAG=$(DEV_IMAGE_TAG) $(DOCKERCOMPOSE) run --use-aliases --service-ports --rm $(DOCKER_WC_OPTIONS)

wc-shell: dev-image
	$(DOCKER_RUN) --interactive --tty $(DEV_IMAGE_TAG)

wc-%: dev-image
	$(DOCKER_RUN) $(DEV_IMAGE_TAG) make $*

#  TODO docker compose down doesn't work yet
wdeps-shell: dev-image
	$(DOCKERCOMPOSE_RUN) $(SERVICE) su

wdeps-%: dev-image
	$(DOCKERCOMPOSE_RUN) $(SERVICE) make $*

# Erlang-specific tasks

compile:
	$(REBAR) compile

xref:
	$(REBAR) xref

lint:
	$(REBAR) lint

check-format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) as test dialyzer

release:
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean: clean-build-image
	rm -rf _build

test:
	$(REBAR) do eunit, ct

test.%: test/dmt_%_tests_SUITE.erl
	$(REBAR) ct --suite=$^
