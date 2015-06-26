# Copyright (c) 2015 Radek Micek

.PHONY: all program doc scripts check clean

all: program doc scripts

program:
	omake program

doc:
	omake doc

scripts:
	omake scripts all

check:
	omake test

clean:
	omake clean
