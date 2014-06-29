PACKAGENAME=toml
COLLECTS=toml

setup:
	raco setup $(COLLECTS)

link:
	raco pkg install --link -n $(PACKAGENAME) $$(pwd)

unlink:
	raco pkg remove $(PACKAGENAME)

test: raco-test toml-test

raco-test:
	raco test -x .

toml-test:
	(export GOPATH=~/go; export PATH=$(PWD):~/go:$(PATH); ~/go/bin/toml-test toml-test.rkt)

