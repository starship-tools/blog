PROJECT = blog
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE = _build/default/lib/lfe/bin/lfe
BUILD_LIBS = $(shell find ./_build/*/* -maxdepth 1 -mindepth 1 -exec printf "%s:" {} \;)
CHECKOUT_LIBS = $(shell find ./_checkouts -maxdepth 1 -mindepth 1 -exec printf "%s:" {} \;)
ERL_LIBS=$(BUILD_LIBS):$(CHECKOUT_LIBS)
RIGHT_PAREN = )
LOCAL_DOCS_HOST = $(subst $(RIGHT_PAREN),,$(strip $(lastword $(shell grep host lfe.config))))
LOCAL_DOCS_PORT = $(subst $(RIGHT_PAREN),,$(strip $(lastword $(shell grep port lfe.config))))

include priv/make/base.mk
include priv/make/gen.mk
include priv/make/git.mk
