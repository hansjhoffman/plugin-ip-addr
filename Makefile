# Build configuration
# -------------------

APP_NAME = "Plugin Boolean"
GIT_BRANCH=`git rev-parse --abbrev-ref HEAD`
GIT_REVISION = `git rev-parse HEAD`

SRC = 'src/**/*.purs'

# Introspection targets
# ---------------------

.PHONY: help
help: header targets

.PHONY: header
header:
	@printf "\n\033[34mEnvironment\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@printf "\033[33m%-23s\033[0m" "APP_NAME"
	@printf "\033[35m%s\033[0m" $(APP_NAME)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_BRANCH"
	@printf "\033[35m%s\033[0m" $(GIT_BRANCH)
	@echo ""
	@printf "\033[33m%-23s\033[0m" "GIT_REVISION"
	@printf "\033[35m%s\033[0m" $(GIT_REVISION)
	@echo ""

.PHONY: targets
targets:
	@printf "\n\033[34mTargets\033[0m\n"
	@printf "\033[34m---------------------------------------------------------------\033[0m\n"
	@perl -nle'print $& if m{^[a-zA-Z_-]+:.*?## .*$$}' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-22s\033[0m %s\n", $$1, $$2}'

# Build targets
# -------------------

.PHONY: clean
clean: ## Remove build artifacts
	rm -rf output .spago .psci_modules

.PHONY: build
build: ## Make a production build
	spago build

.PHONY: bundle
bundle: ## Bundle
	spago bundle-module --main Main --to dist/index.js

# Development targets
# -------------------

.PHONY: deps
deps: ## Install all dependencies
	spago install

.PHONY: docs
docs: ## Generate docs
	spago docs --open

# Check, lint, format and test targets
# ------------------------------------

.PHONY: format
format: ## Format everything
	purs-tidy format-in-place $(SRC)

.PHONY: test
test: ## Test code
	spago -x test.dhall test
