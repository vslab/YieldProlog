function main() {
    var startTime = new Date().getTime();
    var nAnswers = 0;
    var Qs = new Variable();
    for each (var l1 in queens(11, Qs))
        nAnswers += 1;
    var finishTime = new Date().getTime();
    document.write("Naive queens: " + 
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
    var UnplacedQs1 = new Variable();
    var Q = new Variable();
    for each (var l1 in selectq(Q, UnplacedQs, UnplacedQs1)) {
        for each (var l2 in notHasAttack(Q, SafeQs)) {
            for each (var l3 in queens3(UnplacedQs1, new ListPair(Q, SafeQs), Qs))
                yield false;
		}
	}
    for each (var l1 in YP.unify(UnplacedQs, Atom.NIL)) {
        for each (var l2 in YP.unify(Qs, SafeQs))
            yield false;
	}
}

function notHasAttack(X, Xs) {
    for each (var l1 in attack(X, Xs))
        return;

    yield false;
}

function attack(X, Xs) {
    for each (var l1 in attack3(X, 1, Xs))
        yield false;
}

function attack3(X, N, Arg3) {
    var Y = new Variable();
    for each (var l1 in new ListPair(Y, new Variable()).unify(Arg3)) {
        if (YP.getValue(X) == Y.getValue() + YP.getValue(N))
            yield false;
        if (YP.getValue(X) == Y.getValue() - YP.getValue(N))
            yield false;
	}

    var Ys = new Variable();
    var N1 = new Variable();
    for each (var l1 in new ListPair(new Variable(), Ys).unify(Arg3)) {
        for each (var l2 in N1.unify(YP.getValue(N) + 1)) {
            for each (var l3 in attack3(X, N1, Ys))
                yield false;
		}
	}
}

function rangeList(M, N, List) {
    if (YP.getValue(M) >= YP.getValue(N)) {
        for each (var l1 in YP.unify(List, new ListPair(N, Atom.NIL)))
            yield false;
	}
    else {
        var Tail = new Variable();
        for each (var l1 in rangeList(YP.getValue(M) + 1, YP.getValue(N), Tail)) {
            for each (var l2 in YP.unify(List, new ListPair(M, Tail)))
                yield false;
		}
	}
}

function selectq(X, Arg2, Arg3) {
    for each (var l1 in new ListPair(X, Arg3).unify(Arg2))
        yield false;

    var Y = new Variable();
    var Ys = new Variable();
    var Zs = new Variable();
    for each (var l1 in new ListPair(Y, Ys).unify(Arg2)) {
        for each (var l2 in new ListPair(Y, Zs).unify(Arg3)) {
            for each (var l3 in selectq(X, Ys, Zs))
                yield false;
		}
	}
}

main()
