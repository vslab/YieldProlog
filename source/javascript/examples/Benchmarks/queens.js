function main() {
    var startTime = new Date().getTime();
    var nAnswers = 0;
    var Qs = new Variable();
    for each (var l1 in queens(11, Qs))
        nAnswers += 1;
    var finishTime = new Date().getTime();
    document.write("Optimized queens: " + 
		(finishTime - startTime) / 1000.0 + " seconds, " +
		nAnswers + " answers<br>");
}

function queens(N, Qs) {
    var Ns = new Variable();
     for each (var l1 in rangeList(1, N, Ns)) {
         for each (var l2 in queens3(Ns, Atom.NIL, Qs))
            yield false;
	}
}

function queens3(UnplacedQs, SafeQs, Qs) {
    var UnplacedQsListPair = YP.getValue(UnplacedQs);
    if (UnplacedQsListPair instanceof ListPair) {
        var UnplacedQs1 = new Variable();
        var Q = new Variable();
         for each (var l1 in selectq(Q, UnplacedQsListPair, UnplacedQs1)) {
            if (!((SafeQs instanceof ListPair) && hasAttack(Q.getValue(), SafeQs))) {
                 for each (var l2 in queens3(UnplacedQs1, new ListPair(Q, SafeQs), Qs))
                    yield false;
			}
		}
	}
    else {
         for each (var l1 in Qs.unify(SafeQs))
            yield false;
	}
}

function hasAttack(X, Xs) {
    return hasAttack3(X, 1, Xs);
}

function hasAttack3(X, N, Arg3) {
    if (X == YP.getValue(Arg3._arg1) + N || X == YP.getValue(Arg3._arg1) - N)
        return true;
    if (Arg3._arg2 instanceof ListPair)
        return hasAttack3(X, N + 1, YP.getValue(Arg3._arg2));
    else
        return false;
}

function rangeList(M, N, List) {
    if (M >= N) {
        for each (var l1 in List.unify(new ListPair(N, Atom.NIL)))
            yield false;
	}
    else {
    	var Tail = new Variable();
        for each (var l1 in rangeList(M + 1, N, Tail)) {
        	for each (var l2 in List.unify(new ListPair(M, Tail))) 
                yield false;
		}
	}
}

function selectq(X, Arg2, Arg3) {
     for each (var l1 in X.unify(Arg2._arg1)) {
         for each (var l2 in Arg3.unify(Arg2._arg2))
            yield false;
	}

    var Arg2Tail = YP.getValue(Arg2._arg2);
    if (Arg2Tail instanceof ListPair) {
        var Zs = new Variable();
         for each (var l1 in selectq(X, Arg2Tail, Zs)) {
             for each (var l2 in Arg3.unify(new ListPair(Arg2._arg1, Zs)))
                yield false;
		}
	}
}

main();


