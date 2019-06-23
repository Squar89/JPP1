import argparse
import subprocess
import os
import os.path
import sys

parser = argparse.ArgumentParser(description='Generate random tests')
parser.add_argument('directory', type=str, default="randomtests/", nargs='?')
parser.add_argument('num', type=int, default=30, nargs='?')
parser.add_argument('--generate_outs', action='store_true')
parser.add_argument('--print_stderr', action='store_true')
args = parser.parse_args()

try:
    os.makedirs(args.directory)
except FileExistsError:
    r = input("Directory exists, do you want to overwrite? [y/n]")
    if r != "y":
        print("Aborting")
        sys.exit(0)

for i in range(args.num):
    print("Generating test ", i)
    fname = os.path.join(args.directory, "good" + str(i) + ".in")

    with open(fname, "w") as f:
        subprocess.run(["python", "generator.py"], stdout=f)

    if args.generate_outs:
        outname = fname[:-3] + ".ps"
        proc = subprocess.run(["bash", "run.sh"], stdin=open(fname), stdout=open(outname, "w"), stderr=subprocess.PIPE)
        if args.print_stderr:
            print('\x1b[1;31;40m' + proc.stderr.decode() + '\x1b[0m')
