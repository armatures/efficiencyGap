default: build

mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))

build:
	elm make src/Main.elm --output index.html
