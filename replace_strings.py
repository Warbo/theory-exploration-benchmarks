#!/usr/bin/env python
"""Replace strings in stdio. Takes a file of replacements to make, one
replacement per line, with original and replacement separated by tabs.

For example, with files:

DATA:

foo
bar
foobar

REPS:

foo	BAZ
bar	qUUx

The command './replace_strings.py <(cat REPS) < DATA' will give:

BAZ
qUUx
BAZqUUx
"""

import sys

s = sys.stdin.read()

if len(sys.argv) < 2:
    raise Exception("Please give file containing tab-separated replacements")

with open(sys.argv[1]) as reps:
  for line in reps.readlines():
      (src, dst) = line.strip().split('\t')
      s = s.replace(src, dst)

print s
