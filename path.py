#!/usr/bin/env python3
import json
import sys

# Find path in dictionary tree d from root
def s(d, t, w=None):
    if w is None:
        w = []
    for k, v in d.items():
        nw = w + [k]
        if k == t:
            return nw
        if isinstance(v, dict):
            r = s(v, t, nw)
            if r:
                return r
    return None

def p(w):
    if w is None:
        w = []
    for i, n in enumerate(w):
        s = "    " * i
        print(f"{s}└── {n}")

def main():
    t = sys.argv[1].replace('\"', '')
    with open("data/acm.json", 'r') as f:
        p(s(json.load(f), t))

if __name__ == "__main__":
    main()
