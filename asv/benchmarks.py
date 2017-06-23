#!/usr/bin/env python
from subprocess import check_call, check_output
from timeit     import default_timer

def time_mk_defs():
    check_output(['mk_defs'])
time_mk_defs.timer = default_timer

def time_tests():
    check_call(['run_tests'])
time_tests.timer   = default_timer
time_tests.timeout = 300
