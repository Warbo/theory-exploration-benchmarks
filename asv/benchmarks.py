#!/usr/bin/env python
from subprocess import check_call, check_output
from timeit     import default_timer

def time_run_tests():
    check_call(['run_tests'])
time_run_tests.timer   = default_timer
time_run_tests.timeout = 300

def time_mk_defs():
    check_output(['mk_defs'])
time_mk_defs.timer = default_timer

def time_mk_thms():
    check_output(['mk_thms'])
time_mk_thms.timer = default_timer

def time_mk_sdata():
    check_output(['mk_sdata'])
time_mk_sdata.timer = default_timer

def time_mk_fin_defs():
    check_output(['mk_fin_defs'])
time_mk_fin_defs.timer = default_timer
