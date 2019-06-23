import random


def makerandomnum():
    return random.randint(-100, 100)

def makerandomop():
    return random.choice(["moveto", "lineto", "closepath", "add", "div", "mul", "sub", "translate", "rotate"])

def makerandomelement():
    which = random.choice(["op"] + 3 * ["num"])
    if which == "op":
        return makerandomop()
    else:
        return str(makerandomnum())

N = random.randrange(5, 700)

# first add some numbers so we don't run out of stack too often
print(makerandomnum())
print(makerandomnum())
print(makerandomnum())

# most of the time preset the current point so we don't crash on this too often
if random.choice([False] + 5 * [True]):
    print(makerandomnum())
    print(makerandomnum())
    print("moveto")

for i in range(N):
    print(makerandomelement())
