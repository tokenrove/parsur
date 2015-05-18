#!/bin/sh
./tests/tests.exe /Tests/main | sed '0,/^\r$/d'
