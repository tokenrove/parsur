
URWEB := urweb

.PHONY: default check clean demos all

default: check

all: check demos

parse_deps := parse.ur parse.urs parse.urp

tests/tests.exe: tests/test*.ur tests/test*.ur[sp] $(parse_deps)
	urweb -protocol static tests/tests

demo/sexp.exe: demo/sexp.ur demo/sexp.ur[sp] demo/sexp.css $(parse_deps)
	urweb demo/sexp

check: tests/tests.exe
	prove

demos: demo/sexp.exe

clean:
	$(RM) tests/tests.exe demo/sexp.exe
