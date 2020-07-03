SHELL := /bin/bash

.PHONY: all
all: setup-generic setup-golang setup-rustlang setup-erlang

setup-generic:
	sudo apt-get install gnutls-bin

setup-golang:
	GO111MODULE=on go get golang.org/x/tools/gopls@latest
	go get -x -u golang.org/x/tools/cmd/goimports
	go get -x -u golang.org/x/tools/cmd/guru
	go get -x -u golang.org/x/lint/golint
# 	go get -x -u github.com/rogpeppe/godef
# 	go get -x -u github.com/nsf/gocode
# 	go get -x -u golang.org/x/tools/cmd/godoc
# # copy oracle.el to go-guru.el in load-path
# # copy refactor/rename/rename.el to rename.el in load-path
# 	go get -x -u golang.org/x/tools/cmd/gorename
# 	go get -x -u github.com/derekparker/delve/cmd/dlv
# 	go get -x -u github.com/josharian/impl
# 	go get -x -u github.com/godoctor/godoctor
# 	go get -x -u github.com/davidrjenni/reftools/cmd/fillstruct

setup-rustlang:
	rustup update
	rustup component add rls rust-analysis rust-src
	rustup component add clippy
	rustup component add rustfmt
	@echo '>> Install rust-analyzer, like'
	@echo '   git clone https://github.com/rust-analyzer/rust-analyzer.git && cd rust-analyzer'
	@echo '   cargo xtask install'

setup-erlang:
	brew update
	-brew install kerl
	-export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --enable-shared-zlib --enable-dynamic-ssl-lib --enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --with-wx"
	-kerl build 22.2
	-kerl install 22.2 ~/erlang/22.2
	-wget https://github.com/erlang/rebar3/releases/download/3.13.0/rebar3 -O ~/bin/rebar3 && chmod u+x ~/bin/rebar3
	-source ~/erlang/22.2/activate && dialyzer --build_plt --apps kernel stdlib erts mnesia eunit
	-echo ". ~/erlang/22.2/activate" >> ~/.profile
