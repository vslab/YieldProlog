import sys
# Hack sys.path for the examples.
sys.path.append("../..")
from YP import *
from Variable import *
from Atom import *
from ListPair import *
from time import *

def main():
    startTime = clock()
    nAnswers = 0
    Qs = Variable()
    for l1 in queens(11, Qs):
        nAnswers += 1
    finishTime = clock()
    print "Naive queens:", (finishTime - startTime), "seconds,", \
          nAnswers, "answers"

def queens(N, Qs):
    Ns = Variable()
    for l1 in rangeList(1, N, Ns):
        for l2 in queens3(Ns, Atom.NIL, Qs):
            yield False

def queens3(UnplacedQs, SafeQs, Qs):
    UnplacedQs1 = Variable()
    Q = Variable()
    for l1 in selectq(Q, UnplacedQs, UnplacedQs1):
        for l2 in notHasAttack(Q, SafeQs):
            for l3 in queens3(UnplacedQs1, ListPair(Q, SafeQs), Qs):
                yield False
    for l1 in YP.unify(UnplacedQs, Atom.NIL):
        for l2 in YP.unify(Qs, SafeQs):
            yield False

def notHasAttack(X, Xs):
    for l1 in attack(X, Xs):
        return
    yield False

def attack(X, Xs):
    for l1 in attack3(X, 1, Xs):
        yield False

def attack3(X, N, Arg3):
    Y = Variable()
    for l1 in ListPair(Y, Variable()).unify(Arg3):
        if YP.getValue(X) == Y.getValue() + YP.getValue(N):
            yield False
        if YP.getValue(X) == Y.getValue() - YP.getValue(N):
            yield False

    Ys = Variable()
    N1 = Variable()
    for l1 in ListPair(Variable(), Ys).unify(Arg3):
        for l2 in N1.unify(YP.getValue(N) + 1):
            for l3 in attack3(X, N1, Ys):
                yield False

def rangeList(M, N, List):
    if YP.getValue(M) >= YP.getValue(N):
        for l1 in YP.unify(List, ListPair(N, Atom.NIL)):
            yield False
    else:
        Tail = Variable()
        for l1 in rangeList(YP.getValue(M) + 1, YP.getValue(N), Tail):
            for l2 in YP.unify(List, ListPair(M, Tail)):
                yield False


def selectq(X, Arg2, Arg3):
    for l1 in ListPair(X, Arg3).unify(Arg2):
        yield False

    Y = Variable()
    Ys = Variable()
    Zs = Variable()
    for l1 in ListPair(Y, Ys).unify(Arg2):
        for l2 in ListPair(Y, Zs).unify(Arg3):
            for l3 in selectq(X, Ys, Zs):
                yield False

main()


