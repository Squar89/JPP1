import os
import sys
import argparse
import subprocess
import difflib

parser = argparse.ArgumentParser(description='Test solution')
parser.add_argument('directory', type=str, default="examples/", nargs='?')
parser.add_argument('--no_print_stderr', action='store_false')
args = parser.parse_args()
directory = args.directory
print(f"Testing with '{directory}'")
d = map(lambda f: directory + "/" + f, sorted(os.listdir(directory)))
good = 0
bad = 0
try:
    for f in d:
        if ".in" not in f:
            continue

        if "good" in f:
            proc = subprocess.run(["bash", "run.sh"], stdin=open(f), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            out = proc.stdout.decode()
            tru = f[:-3] + ".ps"
            with open(tru, "r") as fl:
                truout = fl.read()
            if out == truout:
                good += 1
                print(f, "OK")
            else:
                bad += 1
                print(f, "error")
                for l in difflib.unified_diff(out.split("\n"), truout.split("\n"), "got", "expected"):
                    print(l)
                if not args.no_print_stderr:
                    print('\x1b[1;31;40m' + proc.stderr.decode() + '\x1b[0m')
        if "bad" in f:
            proc = subprocess.run(["bash", "run.sh"], stdin=open(f), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            out = proc.stdout.decode()
            if "(Error)" in out:
                good += 1
                print(f, "OK")
            else:
                bad += 1
                print(f, "error")
                print("Got:")
                print(out)
                if not args.no_print_stderr:
                    print("Stderr:")
                    print('\x1b[1;31;40m' + proc.stderr.decode() + '\x1b[0m')
except KeyboardInterrupt:
    print("Interrupted.")

print(good, "OK")
print(bad, "errors")
