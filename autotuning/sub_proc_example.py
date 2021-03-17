#!/usr/bin/python

import sys

# read in something
read = sys.stdin.readline()

# write something
sys.stdout.write(read)
sys.stdout.flush()

# write more
sys.stdout.write("round 1 successfull\n")
sys.stdout.flush()

# read in something
read = sys.stdin.readline()

# write something
sys.stdout.write(read )
sys.stdout.flush()
