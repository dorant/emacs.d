.PHONY: all

all: setup-golang setup-rustlang

setup-golang:
	GO111MODULE=on go get golang.org/x/tools/gopls@latest

setup-rustlang:
