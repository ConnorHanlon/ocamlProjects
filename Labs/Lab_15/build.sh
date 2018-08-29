#!/bin/bash

set -eu

###############################################################################
############################# "Library" functions #############################
###############################################################################

function command_exists() {
	command -v $1 >/dev/null 2>&1
}
function echo_color() {
	if [ -t 1 ]; then
		echo -e "\x1b[1;3${1}m${2}\x1b[0m"
	else
		echo "${2}"
	fi
}
function error() {
	echo_color 1 "${@}" >&2
	exit -1
}
function warn() {
	echo_color 3 "${@}" >&2
}
function info() {
	echo_color 6 "${@}"
}
function require() {
	command_exists $1 || error "Command \`${1}' not found, but it is required"
}

###############################################################################
################################# Build Stages ################################
###############################################################################

function build_ocaml() {
	info "Building OCaml code..."
	require ocamlbuild
	ocamlbuild compiler.native
}
function build_lang() {
	info "Compiling..."
	mkdir -p out
	./compiler.native ${@} > out/main.c
}
function build_c() {
	info "Building C code..."
	require gcc
	gcc -D_DEFAULT_SOURCE -fcilkplus -std=c99 -I runtime -o out/main ${CFLAGS:-} out/main.c
}
function run() {
	info "Running C code..."
	time ./out/main
}

###############################################################################
################################ Main Function ################################
###############################################################################

function main() {
	# Output a backtrace on exception.
	export OCAMLRUNPARAM=b 

	build_ocaml;
	build_lang ${@};
	build_c;
	run;
}
main ${@};
