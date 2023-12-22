import sys
import math

modules = {}
outputs = {}
with open(sys.argv[1]) as f:

    def module(line):
        m = line.split(" -> ")[0]
        if m == "broadcaster":
            return m, ""
        return m[1:], m[0]

    def connections(line):
        cons = line.split(" -> ")[1]
        return tuple([c.strip() for c in cons.split(",")])

    lines = f.read().splitlines()
    for line in lines:
        name, kind = module(line)
        cons = connections(line)
        modules[name] = kind
        outputs[name] = cons
    nops = set()
    for mods in outputs.values():
        for mod in mods:
            if not mod in outputs:
                nops.add(mod)
    for nop in nops:
        modules[nop] = ""
        outputs[nop] = ()


colors = {"": "black", "%": "red", "&": "blue"}
print("digraph {")
print("{ broadcaster }")
print("{")
ffs = [name for (name, typ) in modules.items() if typ == "%"]
for ff in ffs:
    print("%s [color=%s]" % (ff, colors["%"]))
print("}")
print("{")
conjs = [name for (name, typ) in modules.items() if typ == "&"]
for conj in conjs:
    print("%s [color=%s]" % (conj, colors["&"]))
print("}")

for src, dsts in outputs.items():
    for dst in dsts:
        print("%s -> %s" % (src, dst))
print("}")
