#!/usr/bin/env python
from os         import environ, path
from subprocess import check_call
from timeit     import default_timer
from sys        import stderr

def time_mk_defs():
    check_call(['mk_defs'])
time_mk_defs.timer = default_timer

def time_tests():
    check_call(['run_tests'])
time_tests.timer   = default_timer
time_tests.timeout = 300
