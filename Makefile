
URWEB := urweb

.PHONY: default check clean

default: check

parse_deps := parse.ur parse.urs parse.urp

tests/tests.exe: tests/test*.ur tests/test*.ur[sp] $(parse_deps)
	urweb -protocol static tests/tests

check: tests/tests.exe
	prove

clean:
	$(RM) tests/tests.exe
