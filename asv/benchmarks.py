#!/usr/bin/env python
from os         import path
from subprocess import check_call
from timeit     import default_timer
from sys        import stderr

def time_mk_defs():
    args = [
        path.join(path.dirname(__file__), '..', 'scripts',
                  'make_normalised_definitions.rkt')
    ]
    stderr.write(repr(args))
    check_call(args)

time_mk_defs.timer = default_timer
