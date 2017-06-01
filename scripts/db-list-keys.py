#!/usr/bin/env python
import base64
import dbhash
import sys

def main():
    if len(sys.argv) == 1:
        usage()
        return
    for k, v in dbhash.open(sys.argv[1]).iteritems():
        print(k)

def usage():
    print("Usage: {} <path to .db file, ...>".format(sys.argv[0]))

if __name__ == "__main__":
    main()
