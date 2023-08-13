LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load ulid.asd \
		--eval '(ql:quickload :ulid)' \
		--eval '(asdf:make :ulid)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
