#! /usr/bin/env nix-shell
#! nix-shell -p python -i python

'''
Map the symbols found in TIP benchmarks to their equivalent Haskell names in
benchmark_package.

This is manually hard-coded, but we perform some checks with our automated test
suite (e.g. that every symbol found in the benchmarks appears in the mapping).
'''

import fileinput

def add(module, new, old):
    '''Add the contents of dictionary 'new' to dictionary 'old', but prefix all
    new values with module and '.'.'''
    qual = {k: module + '.' + new[k] for k in new}
    return {k: v for d in [old, qual] for k, v in d.items()}

def combine(mod_dicts, old):
    if len(mod_dicts) == 0:
        return old
    return combine(mod_dicts[1:], add(mod_dicts[0][0], mod_dicts[0][1], old))

mapping = combine([
    ['Prod.Definitions', {
        '<=' : '(<=)',
        '='  : '(==)',
        '+'  : '(+)',
        '^1' : 'one'
    }],
    ['Koen.Sort', {
        '>' : '(>)'
    }],
    ['Koen.Propositional', {
        '&'  : ':&:',
        '=2' : '(|=)'
    }],
    ['Tip2015.Nat', {
        'factorial' : 'factorial'
    }],
    ['Prelude', {
        '-': '(-)',
        '@': '($)'
    }]],
    {})

for line in fileinput.input():
    if line.strip() != "":
        print mapping[line.strip()]
