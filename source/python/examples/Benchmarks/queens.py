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
    print "Optimized queens:", (finishTime - startTime), "seconds,", \
          nAnswers, "answers"

def queens(N, Qs):
    Ns = Variable()
    for l1 in rangeList(1, N, Ns):
        for l2 in queens3(Ns, Atom.NIL, Qs):
            yield False

def queens3(UnplacedQs, SafeQs, Qs):
    UnplacedQsListPair = YP.getValue(UnplacedQs)
    if isinstance(UnplacedQsListPair, ListPair):
        UnplacedQs1 = Variable()
        Q = Variable()
        for l1 in selectq(Q, UnplacedQsListPair, UnplacedQs1):
            if not (isinstance(SafeQs, ListPair) and hasAttack(Q.getValue(), SafeQs)):
                for l2 in queens3(UnplacedQs1, ListPair(Q, SafeQs), Qs):
                    yield False
    else:
        for l1 in Qs.unify(SafeQs):
            yield False

def hasAttack(X, Xs):
    return hasAttack3(X, 1, Xs)

def hasAttack3(X, N, Arg3):
    if X == YP.getValue(Arg3._arg1) + N or X == YP.getValue(Arg3._arg1) - N:
        return True
    if isinstance(Arg3._arg2, ListPair):
        return hasAttack3(X, N + 1, YP.getValue(Arg3._arg2))
    else:
        return False

def rangeList(M, N, List):
    if M >= N:
        for l1 in List.unify(ListPair(N, Atom.NIL)):
            yield False
    else:
        Tail = Variable()
        for l1 in rangeList(M + 1, N, Tail):
            for l2 in List.unify(ListPair(M, Tail)):
                yield False

def selectq(X, Arg2, Arg3):
    for l1 in X.unify(Arg2._arg1):
        for l2 in Arg3.unify(Arg2._arg2):
            yield False

    Arg2Tail = YP.getValue(Arg2._arg2)
    if isinstance(Arg2Tail, ListPair):
        Zs = Variable()
        for l1 in selectq(X, Arg2Tail, Zs):
            for l2 in Arg3.unify(ListPair(Arg2._arg1, Zs)):
                yield False
                
main()


