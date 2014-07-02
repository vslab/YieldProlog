/*
 * Copyright (C) 2007-2008, Jeff Thompson
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright 
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright 
 *       notice, this list of conditions and the following disclaimer in the 
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its contributors 
 *       may be used to endorse or promote products derived from this software 
 *       without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

function YP() {
}

YP.MAX_ARITY = 255

// If value is a Variable, then return its getValue.  Otherwise, just
// return value.  You should call YP.getValue on any object that
// may be a Variable to get the value to pass to other functions in
// your system that are not part of Yield Prolog, such as math functions
// or file I/O.
// For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
YP.getValue = function(value) {
    if (value instanceof Variable)
        return value.getValue();
    else
        return value;
}

// If arg1 or arg2 is an object with a unify method (such as Variable or
// Functor) then just call its unify with the other argument.  The object's
// unify method will bind the values or check for equals as needed.
// Otherwise, both arguments are "normal" (atomic) values so if they
// are equal then succeed (yield once), else fail (don't yield).
// For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
YP.unify = function(arg1, arg2) {
    arg1 = YP.getValue(arg1);
    arg2 = YP.getValue(arg2);
    if (YP.isIUnifiable(arg1))
        return arg1.unify(arg2);
    else if (arg2.unify != undefined)
        return arg2.unify(arg1);
    else {
        // Arguments are "normal" types.
        if (arg1 == arg2)
            return new YP.Succeed();
        else
            return YP._fail;
	}
}

// Return true if obj implements IUnifiable.
// This does not call YP.getValue(obj).
// Debug: need a better way to check for this.  (Use it as a base class?)
YP.isIUnifiable = function(obj) {
  return typeof(obj) == 'object' && obj.unify !== undefined;
}

// Convert term to a number.
// If term is a single-element List, use its first element
// (to handle the char types like "a").
// If can't convert, throw a PrologException for type_error evaluable (because this is only
//   called from arithmetic functions).
YP.convertNumber = function(term) {
  term = YP.getValue(term);
  if ((term instanceof Functor2) && term._name == Atom.DOT && YP.getValue(term._arg2) == Atom.NIL)
    // Assume it is a char type like "a".
    term = YP.getValue(term._arg1);
  if (term instanceof Variable)
    throw new PrologException(Atom.a("instantiation_error"), 
        "Expected a number but the argument is an unbound variable");
             
  if (typeof(term) == "number")
    return term;
  else
    throw new PrologException
      (new Functor2
       ("type_error", Atom.a("evaluable"), 
        new Functor2(Atom.SLASH, YP.getFunctorName(term), YP.getFunctorArgs(term).length)), 
        "Term must be a number");
}

YP.equal = function(x, y) {
  return YP.convertNumber(x) == YP.convertNumber(y);
}

YP.notEqual = function(x, y) {
  return YP.convertNumber(x) != YP.convertNumber(y);
}

YP.greaterThan = function(x, y) {
  return YP.convertNumber(x) > YP.convertNumber(y);
}

YP.lessThan = function(x, y) {
  return YP.convertNumber(x) < YP.convertNumber(y);
}

YP.greaterThanOrEqual = function(x, y) {
  return YP.convertNumber(x) >= YP.convertNumber(y);
}

YP.lessThanOrEqual = function(x, y) {
  return YP.convertNumber(x) <= YP.convertNumber(y);
}

YP.negate = function(x) {
  return -YP.convertNumber(x);
}

YP.abs = function(x) {
  return Math.abs(YP.convertNumber(x));
}

YP.sign = function(x) {
  var num = YP.convertNumber(x);
  if (num > 0)
    return 1;
  else if (num < 0)
    return -1;
  else
    return 0;
}

// Use toFloat instead of float because it is a reserved keyword.
YP.toFloat = function(x) {
  return YP.convertNumber(x);
}

YP.floor = function(x) {
  return Math.floor(YP.convertNumber(x));
}

YP.truncate = function(x) {
  var num = YP.convertNumber(x);
  if (num > 0)
    return Math.floor(num);
  else
    return Math.ceil(num);
}

YP.round = function(x) {
  return Math.round(YP.convertNumber(x));
}

YP.ceiling = function(x) {
  return Math.ceil(YP.convertNumber(x));
}

YP.sin = function(x) {
  return Math.sin(YP.convertNumber(x));
}

YP.cos = function(x) {
  return Math.cos(YP.convertNumber(x));
}

YP.atan = function(x) {
  return Math.atan(YP.convertNumber(x));
}

YP.exp = function(x) {
  return Math.exp(YP.convertNumber(x));
}

YP.log = function(x) {
  return Math.log(YP.convertNumber(x));
}

YP.sqrt = function(x) {
  return Math.sqrt(YP.convertNumber(x));
}

YP.bitwiseComplement = function(x) {
  return ~YP.convertNumber(x);
}

YP.add = function(x, y) {
  return YP.convertNumber(x) + YP.convertNumber(y);
}

YP.subtract = function(x, y) {
  return YP.convertNumber(x) - YP.convertNumber(y);
}

YP.multiply = function(x, y) {
  return YP.convertNumber(x) * YP.convertNumber(y);
}

YP.divide = function(x, y) {
  return YP.convertNumber(x) / YP.convertNumber(y);
}

YP.intDivide = function(x, y) {
  return YP.truncate(YP.convertNumber(x) / YP.convertNumber(y));
}

YP.mod = function(x, y) {
  return YP.truncate(YP.convertNumber(x) % YP.convertNumber(y));
}

YP.pow = function(x, y) {
  return Math.pow(YP.convertNumber(x), YP.convertNumber(y));
}

YP.bitwiseShiftRight = function(x, y) {
  return YP.convertNumber(x) >> YP.convertNumber(y);
}

YP.bitwiseShiftLeft = function(x, y) {
  return YP.convertNumber(x) << YP.convertNumber(y);
}

YP.bitwiseAnd = function(x, y) {
  return YP.convertNumber(x) & YP.convertNumber(y);
}

YP.bitwiseOr = function(x, y) {
  return YP.convertNumber(x) | YP.convertNumber(y);
}

YP.min = function(x, y) {
  return Math.min(YP.convertNumber(x), YP.convertNumber(y));
}

YP.max = function(x, y) {
  return Math.max(YP.convertNumber(x), YP.convertNumber(y));
}

YP.copy_term = function(inTerm, outTerm) {
    return YP.unify(outTerm, YP.makeCopy(inTerm, new Variable.CopyStore()));
}

YP.addUniqueVariables = function(term, variableSet)         {
    term = YP.getValue(term);
    if (YP.isIUnifiable(term))
        term.addUniqueVariables(variableSet);
}

YP.makeCopy = function(term, copyStore) {
    term = YP.getValue(term);
    if (YP.isIUnifiable(term))
        return term.makeCopy(copyStore);
    else
        // term is a "normal" type. Assume it is ground.
        return term;
}

// Sort the array in place according to termLessThan.  This does not remove duplicates.
YP.sortArray = function(array) {
    array.sort(YP.compareTerms);
}

// Sort List according to termLessThan, remove duplicates and unify with Sorted.
YP.sort = function(List, Sorted) {
    array = ListPair.toArray(List);
    if (array == null)
        return YP.fail();
    if (array.length > 1)
		YP.sortArray(array);
	return YP.unify(Sorted, ListPair.makeWithoutRepeatedTerms(array));
}

// Use YP.unify to unify each of the elements of the two arrays, and yield
// once if they all unify.
YP.unifyArrays = function(array1, array2) {
    if (array1.length != array2.length)
        return;
		
    var iterators = [];
    var gotMatch = true;
    var nIterators = 0;
    // Try to bind all the arguments.
    for (var i = 0; i < array1.length; ++i) {
        var iterator = Iterator(YP.unify(array1[i], array2[i]));
        iterators[nIterators++] = iterator;
        // next() returns if YP.unify succeeds.
        try {
            iterator.next();
		}
        catch (e if e instanceof StopIteration) {
            gotMatch = false;
            break;
		}
	}

	try {
    	if (gotMatch)
        	yield false;
	}
	finally {
    	// Manually finalize all the iterators.
    	for (var i = 0; i < nIterators; ++i)
        	iterators[i].close();
	}
}

// Return an iterator (which you can use in a for-in loop) which does
// zero iterations.  This returns a pre-existing iterator which is
// more efficient than letting the compiler generate a new one.
YP.fail = function() {
  return YP._fail;
}

// Return an iterator (which you can use in a for-in loop) which does
// one iteration.  This returns a pre-existing iterator which is
// more efficient than letting the compiler generate a new one.
YP.succeed = function() {
  return new YP.Succeed();
}

// Return an iterator (which you can use in a for-in loop) which repeats
// indefinitely.  This returns a pre-existing iterator which is
// more efficient than letting the compiler generate a new one.
YP.repeat = function() {
  return YP._repeat;
}

YP.univ = function(Term, List) {
    Term = YP.getValue(Term);
    List = YP.getValue(List);

    if (YP.nonvar(Term))
        return YP.unify(new ListPair
            (YP.getFunctorName(Term), ListPair.make(YP.getFunctorArgs(Term))), List);
			
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l1 in new ListPair(Name, ArgList).unify(List)) {
        var args = ListPair.toArray(ArgList);
	    if (args == null)
            throw new PrologException
                (new Functor2("type_error", Atom.a("list"), ArgList),
                 "Expected a list. Got: " + ArgList.getValue());
	    if (args.length == 0)
	        // Return the Name, even if it is not an Atom.
	        return YP.unify(Term, Name);
        if (args.length > YP.MAX_ARITY)
            throw new PrologException
                (new Functor1("representation_error", Atom.a("max_arity")),
                 "Functor arity " + args.length + " may not be greater than " + YP.MAX_ARITY);

	    if (!YP.atom(Name))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), Name),
                 "Expected an atom. Got: " + Name.getValue());

        return YP.unify(Term, Functor.make(YP.getValue(Name), args));	
    }

    return YP.fail();
}

YP.functor = function(Term, FunctorName, Arity) {
    Term = YP.getValue(Term);
    FunctorName = YP.getValue(FunctorName);
    Arity = YP.getValue(Arity);

    if (Term instanceof Variable) {
        if (FunctorName instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 2 FunctorName is an unbound variable");
        if (Arity instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 3 Arity is an unbound variable");
        if (typeof(Arity) != "number")
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), Arity), "Arity is not an integer");
        if (!YP.atomic(FunctorName))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atomic"), FunctorName), "FunctorName is not atomic");

        if (Arity < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), Arity),
                           "Arity may not be less than zero");
        else if (Arity == 0)
        {
            // Just unify Term with the atomic FunctorName.
            for each (var l1 in YP.unify(Term, FunctorName))
                yield false;
        }
        else
        {
            if (Arity > YP.MAX_ARITY)
                throw new PrologException
                    (new Functor1("representation_error", Atom.a("max_arity")),
                     "Functor arity " + Arity + " may not be greater than " + YP.MAX_ARITY);
            if (!(FunctorName instanceof Atom))
                throw new PrologException
                    (new Functor2("type_error", Atom.a("atom"), FunctorName), "FunctorName is not an atom");
            // Construct a functor with unbound variables.
            var args = [];
            for (var i = 0; i < Arity; ++i)
                args[i] = new Variable();
            for each (var l1 in YP.unify(Term, Functor.make(FunctorName, args)))
                yield false;
        }
    }
    else {
        for each (var l1 in YP.unify(FunctorName, YP.getFunctorName(Term))) {
            for each (var l2 in YP.unify(Arity, YP.getFunctorArgs(Term).length))
                yield false;
        }
    }
}

YP.arg = function(ArgNumber, Term, Value) {
    if (YP.var(ArgNumber))
        throw new PrologException(Atom.a("instantiation_error"), "Arg 1 ArgNumber is an unbound variable");
    if (!YP.integer(ArgNumber))
        throw new PrologException
            (new Functor2("type_error", Atom.a("integer"), ArgNumber), "Arg 1 ArgNumber must be integer");
    var argNumberInt = YP.convertNumber(ArgNumber);
    if (argNumberInt < 0)
        throw new PrologException
            (new Functor2("domain_error", Atom.a("not_less_than_zero"), argNumberInt),
            "ArgNumber may not be less than zero"); 
            
    if (YP.var(Term))
        throw new PrologException(Atom.a("instantiation_error"),
            "Arg 2 Term is an unbound variable");
    if (!YP.compound(Term))
        throw new PrologException
            (new Functor2("type_error", Atom.a("compound"), Term), "Arg 2 Term must be compound");
            
    var termArgs = YP.getFunctorArgs(Term);
    // Silently fail if argNumberInt is out of range.
    if (argNumberInt >= 1 && argNumberInt <= termArgs.length) {
        // The first ArgNumber is at 1, not 0.
        for each (var l1 in YP.unify(Value, termArgs[argNumberInt - 1]))
            yield false;
    }
}

YP.termEqual = function(Term1, Term2) {
    Term1 = YP.getValue(Term1);
    if (YP.isIUnifiable(Term1))
        return Term1.termEqual(Term2);
    return Term1 == YP.getValue(Term2);
}

YP.termNotEqual = function(Term1, Term2) {
  return !YP.termEqual(Term1, Term2);
}

YP._nextVariableID = 0;
YP.termLessThan = function(Term1, Term2) {
    Term1 = YP.getValue(Term1);
    Term2 = YP.getValue(Term2);
    var term1TypeCode = YP.getTypeCode(Term1);
    var term2TypeCode = YP.getTypeCode(Term2);
    if (term1TypeCode != term2TypeCode)
        return term1TypeCode < term2TypeCode;

    // The terms are the same type code.
    if (term1TypeCode == -2) {
        // Variable.
        // We always check for equality first because we want to be sure 
        //   that less than returns false if the terms are equal, in 
        //   case that the less than check really behaves like less than or equal.
        if (Term1 != Term2) {
		    // We need to set a unique ID so that we consistently compare.
			if (Term1._id === undefined)
			  Term1._id = (++YP._nextVariableID);
			if (Term2._id === undefined)
			  Term2._id = (++YP._nextVariableID);

            return Term1._id < Term2._id;
	    }
        return false;
    }
    if (term1TypeCode == 0)
        return Term1._name < Term2._name;
    if (term1TypeCode == 1)
        return Term1.lessThan(Term2);
    if (term1TypeCode == 2)
        return Term1.lessThan(Term2);
    if (term1TypeCode == 3)
        return Term1.lessThan(Term2);
    if (term1TypeCode == 4)
        return Term1.lessThan(Term2);

    // Type code is -1 for general objects.  First compare their type names.
    var term1TypeName = typeof(Term1);
    var term2TypeName = typeof(Term2);
    if (term1TypeName != term2TypeName)
        return term1TypeName < term2TypeName;
		
    // The terms are the same type name.
	if (term1TypeName == 'string' || term1TypeName == 'number' || term1TypeName == 'boolean')
        return Term1 < Term2;

    // Debug: Should we try Date, arrays, etc.?

    if (Term1 != Term2)
        // Could be equal or greater than.  Just compare strings representations.
        return Term1.toString() < Term2.toString();
    return false;
}

// Type code is -2 if term is a Variable, 0 if it is an Atom, 
// 1 if it is a Functor1, 2 if it is a Functor2, 3 if it is a Functor3, 
// 4 if it is Functor.
// Otherwise, type code is -1.
// This does not call YP.getValue(term).
YP.getTypeCode = function(term) {
    if (term instanceof Variable)
        return -2;
    else if (term instanceof Atom)
        return 0;
    else if (term instanceof Functor1)
        return 1;
    else if (term instanceof Functor2)
        return 2;
    else if (term instanceof Functor3)
        return 3;
    else if (term instanceof Functor)
        return 4;
    else
        return -1;
}

YP.termLessThanOrEqual = function(Term1, Term2) {
    if (YP.termEqual(Term1, Term2))
        return true;
    return YP.termLessThan(Term1, Term2);
}

YP.termGreaterThan = function(Term1, Term2) {
    return !YP.termLessThanOrEqual(Term1, Term2);
}

YP.termGreaterThanOrEqual = function(Term1, Term2) {
    // termLessThan should ensure that it returns false if terms are equal,
    //   so that this would return true.
    return !YP.termLessThan(Term1, Term2);
}

YP.compareTerms = function(Term1, Term2) {
    if (YP.termEqual(Term1, Term2))
        return 0;
    else if (YP.termLessThan(Term1, Term2))
        return -1;
    else
        return 1;
}

YP.ground = function(Term) {
    Term = YP.getValue(Term);
    if (YP.isIUnifiable(Term))
        return Term.ground();
    return true;
}

YP._operatorTable = null;
YP.current_op = function(Priority, Specifier, Operator) {
    if (YP._operatorTable == null) {
        // Initialize.
        YP._operatorTable = new IndexedAnswers(3);
        YP._operatorTable.addAnswer([1200, Atom.a("xfx"), Atom.a(":-")]);
        YP._operatorTable.addAnswer([1200, Atom.a("xfx"), Atom.a("-->")]);
        YP._operatorTable.addAnswer([1200, Atom.a("fx"), Atom.a(":-")]);
        YP._operatorTable.addAnswer([1200, Atom.a("fx"), Atom.a("?-")]);
        YP._operatorTable.addAnswer([1100, Atom.a("xfy"), Atom.a(";")]);
        YP._operatorTable.addAnswer([1050, Atom.a("xfy"), Atom.a("->")]);
        YP._operatorTable.addAnswer([1000, Atom.a("xfy"), Atom.a(",")]);
        YP._operatorTable.addAnswer([900, Atom.a("fy"), Atom.a("\\+")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("\\=")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("==")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("\\==")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@<")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@=<")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@>")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("@>=")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=..")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("is")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=:=")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=\\=")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("<")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a("=<")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a(">")]);
        YP._operatorTable.addAnswer([700, Atom.a("xfx"), Atom.a(">=")]);
        YP._operatorTable.addAnswer([600, Atom.a("xfy"), Atom.a(":")]);
        YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("+")]);
        YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("-")]);
        YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("/\\")]);
        YP._operatorTable.addAnswer([500, Atom.a("yfx"), Atom.a("\\/")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("*")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("/")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("//")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("rem")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("mod")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a("<<")]);
        YP._operatorTable.addAnswer([400, Atom.a("yfx"), Atom.a(">>")]);
        YP._operatorTable.addAnswer([200, Atom.a("xfx"), Atom.a("**")]);
        YP._operatorTable.addAnswer([200, Atom.a("xfy"), Atom.a("^")]);
        YP._operatorTable.addAnswer([200, Atom.a("fy"), Atom.a("-")]);
        YP._operatorTable.addAnswer([200, Atom.a("fy"), Atom.a("\\")]);
        // Debug: This is hacked in to run the Prolog test suite until we implement op/3.
        YP._operatorTable.addAnswer([20, Atom.a("xfx"), Atom.a("<--")]);
    }

    return YP._operatorTable.match([Priority, Specifier, Operator]);
}

YP.atom_length = function(atom, Length) {
    atom = YP.getValue(atom);
    Length = YP.getValue(Length);
    if (atom instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"),
            "Expected atom(Arg1) but it is an unbound variable");
    if (!(atom instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not an atom");
    if (!(Length instanceof Variable))
    {
        if (typeof(Length) != "number")
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), Length), "Length must be var or integer");
        if (Length < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), Length),
                "Length must not be less than zero");
    }
    return YP.unify(Length, atom._name.length);
}

YP.atom_concat = function(Start, End, Whole) {
    // Debug: Should we try to preserve the _declaringClass?
    Start = YP.getValue(Start);
    End = YP.getValue(End);
    Whole = YP.getValue(Whole);
    if (Whole instanceof Variable) {
        if (Start instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Start and arg 3 Whole are both var");
        if (End instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 2 End and arg 3 Whole are both var");
        if (!(Start instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), Start), "Arg 1 Start is not an atom");
        if (!(End instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), End), "Arg 2 End is not an atom");

        for each (var l1 in YP.unify(Whole, Atom.a(Start._name + End._name)))
            yield false;
    }
    else {
        if (!(Whole instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), Whole), "Arg 3 Whole is not an atom");
        var gotStartLength = false;
        var startLength = 0;
        if (!(Start instanceof Variable))
        {
            if (!(Start instanceof Atom))
                throw new PrologException
                    (new Functor2("type_error", Atom.a("atom"), Start), "Arg 1 Start is not var or atom");
            startLength = Start._name.length;
            gotStartLength = true;
        }

        var gotEndLength = false;
        var endLength = 0;
        if (!(End instanceof Variable)) {
            if (!(End instanceof Atom))
                throw new PrologException
                    (new Functor2("type_error", Atom.a("atom"), End), "Arg 2 End is not var or atom");
            endLength = End._name.length;
            gotEndLength = true;
        }

        // We are doing a search through all possible Start and End which concatenate to Whole.
        var wholeString = Whole._name;
        for (var i = 0; i <= wholeString.length; ++i) {
            // If we got either startLength or endLength, we know the lengths have to match so check
            //   the lengths instead of constructing an Atom to do it.
            if (gotStartLength && startLength != i)
                continue;
            if (gotEndLength && endLength != wholeString.length - i)
                continue;
            for each (var l1 in YP.unify(Start, Atom.a(wholeString.substr(0, i))))
            {
                for each (var l2 in YP.unify(End, Atom.a(wholeString.substr(i, wholeString.length - i))))
                    yield false;
            }
        }
    }
}

YP.sub_atom = function(atom, Before, Length, After, Sub_atom) {
    // Debug: Should we try to preserve the _declaringClass?
    atom = YP.getValue(atom);
    Before = YP.getValue(Before);
    Length = YP.getValue(Length);
    After = YP.getValue(After);
    Sub_atom = YP.getValue(Sub_atom);
    if (atom instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"),
            "Expected atom(Arg1) but it is an unbound variable");
    if (!(atom instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not an atom");
    if (!(Sub_atom instanceof Variable)) {
        if (!(Sub_atom instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), Sub_atom), "Sub_atom is not var or atom");
    }

    var beforeIsInt = false;
    var lengthIsInt = false;
    var afterIsInt = false;
    if (!(Before instanceof Variable)) {
        if (typeof(Before) != "number")
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), Before), "Before must be var or integer");
        beforeIsInt = true;
        if (Before < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), Before),
                "Before must not be less than zero");
    }
    if (!(Length instanceof Variable)) {
        if (typeof(Length) != "number")
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), Length), "Length must be var or integer");
        lengthIsInt = true;
        if (Length < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), Length),
                "Length must not be less than zero");
    }
    if (!(After instanceof Variable)) {
        if (typeof(After) != "number")
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), After), "After must be var or integer");
        afterIsInt = true;
        if (After < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), After),
                "After must not be less than zero");
    }

    var atomLength = atom._name.length;
    if (beforeIsInt && lengthIsInt) {
        // Special case: the caller is just trying to extract a substring, so do it quickly.
        var xAfter = atomLength - Before - Length;
        if (xAfter >= 0) {
            for each (var l1 in YP.unify(After, xAfter)) {
                for each (var l2 in YP.unify
                    (Sub_atom, Atom.a(atom._name.substr(Before, Length))))
                    yield false;
            }
        }
    }
    else if (afterIsInt && lengthIsInt) {
        // Special case: the caller is just trying to extract a substring, so do it quickly.
        var xBefore = atomLength - After - Length;
        if (xBefore >= 0) {
            for each (var l1 in YP.unify(Before, xBefore)) {
                for each (var l2 in YP.unify
                    (Sub_atom, Atom.a(atom._name.substr(xBefore, Length))))
                    yield false;
            }
        }
    }
    else {
        // We are underconstrained and doing a search, so go through all possibilities.
        for (var xBefore = 0; xBefore <= atomLength; ++xBefore) {
            for each (var l1 in YP.unify(Before, xBefore)) {
                for (var xLength = 0; xLength <= (atomLength - xBefore); ++xLength) {
                    for each (var l2 in YP.unify(Length, xLength)) {
                        for each (var l3 in YP.unify(After, atomLength - (xBefore + xLength))) {
                            for each (var l4 in YP.unify
                                (Sub_atom, Atom.a(atom._name.substr(xBefore, xLength))))
                                yield false;
                        }
                    }
                }
            }
        }
    }
}

YP.atom_chars = function(atom, List) {
    atom = YP.getValue(atom);
    List = YP.getValue(List);

    if (atom instanceof Variable) {
        if (List instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Atom and arg 2 List are both unbound variables");
        var codeArray = ListPair.toArray(List);
        if (codeArray == null)
            throw new PrologException
                (new Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list");

        var charArray = [];
        for (var i = 0; i < codeArray.length; ++i) {
            var listAtom = YP.getValue(codeArray[i]);
            if (listAtom instanceof Variable)
                throw new PrologException(Atom.a("instantiation_error"),
                    "Arg 2 List has an element which is an unbound variable");
            if (!(listAtom instanceof Atom && listAtom._name.length == 1))
                throw new PrologException
                    (new Functor2("type_error", Atom.a("character"), listAtom), 
                     "Arg 2 List has an element which is not a one character atom");
            charArray[i] = listAtom._name[0];
        }
        return YP.unify(atom, Atom.a(charArray.join("")));
    }
    else {
        if (!(atom instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not var or atom");

        var atomString = atom._name;
        var charList = Atom.NIL;
        // Start from the back to make the list.
        for (var i = atomString.length - 1; i >= 0; --i)
            charList = new ListPair(Atom.a(atomString.substr(i, 1)), charList);
        return YP.unify(List, charList);
    }
}

YP.atom_codes = function(atom, List) {
    atom = YP.getValue(atom);
    List = YP.getValue(List);

    if (atom instanceof Variable) {
       if (List instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Atom and arg 2 List are both unbound variables");
        var codeArray = ListPair.toArray(List);
        if (codeArray == null)
            throw new PrologException
                (new Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list");

        for (var i = 0; i < codeArray.length; ++i) {
            codeArray[i] = YP.getValue(codeArray[i]);
            if (typeof(codeArray[i]) != "number" || codeArray[i] < 0)
                throw new PrologException
                    (new Functor1("representation_error", Atom.a("character_code")), 
                     "Element of Arg 2 List is not a character code");
        }
		  // fromCharCode takes N arguments, so we use apply so we can pass an argument array.
        return YP.unify(atom, Atom.a(String.fromCharCode.apply(null, codeArray)));
    }
    else {
        if (!(atom instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), atom), "Arg 1 Atom is not var or atom");

        var atomString = atom._name;
        var codeList = Atom.NIL;
        // Start from the back to make the list.
        for (var i = atomString.length - 1; i >= 0; --i)
            codeList = new ListPair(atomString.charCodeAt(i), codeList);
        return YP.unify(List, codeList);
    }
}

YP.number_chars = function(Number, List) {
    Number = YP.getValue(Number);
    List = YP.getValue(List);

    if (Number instanceof Variable) {
        if (List instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Number and arg 2 List are both unbound variables");
        var codeArray = ListPair.toArray(List);
        if (codeArray == null)
            throw new PrologException
                (new Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list");

        var charArray = [];
        for (var i = 0; i < codeArray.length; ++i) {
            var listAtom = YP.getValue(codeArray[i]);
            if (listAtom instanceof Variable)
                throw new PrologException(Atom.a("instantiation_error"),
                    "Arg 2 List has an element which is an unbound variable");
            if (!(listAtom instanceof Atom && listAtom._name.length == 1))
                throw new PrologException
                    (new Functor2("type_error", Atom.a("character"), listAtom),
                     "Arg 2 List has an element which is not a one character atom");
            charArray[i] = listAtom._name[0];
        }
        return YP.unify(Number, YP.parseNumberString(charArray.join("")));
    }
    else  {
        if (!YP.number(Number))
            throw new PrologException
                (new Functor2("type_error", Atom.a("number"), Number),
                "Arg 1 Number is not var or number");
        // We just checked, so convertNumber shouldn't throw an exception.
        var numberString = YP.convertNumber(Number).toString();

        var charList = Atom.NIL;
        // Start from the back to make the list.
        for (var i = numberString.length - 1; i >= 0; --i)
            charList = new ListPair(Atom.a(numberString.substr(i, 1)), charList);
        return YP.unify(List, charList);
    }
}

YP.number_codes = function(Number, List) {
    Number = YP.getValue(Number);
    List = YP.getValue(List);

    if (Number instanceof Variable) {
        if (List instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Number and arg 2 List are both unbound variables");
        var codeArray = ListPair.toArray(List);
        if (codeArray == null)
             throw new PrologException
                (new Functor2("type_error", Atom.a("list"), List), "Arg 2 List is not a list");

        for (var i = 0; i < codeArray.length; ++i) {
            codeArray[i] = YP.getValue(codeArray[i]);
            if (typeof(codeArray[i]) != "number" || codeArray[i] < 0)
                throw new PrologException
                    (new Functor1("representation_error", Atom.a("character_code")), 
                     "Element of Arg 2 List is not a character code");
        }
		  // fromCharCode takes a N arguments, so we us apply so we can pass an argument array.
        var numberString = String.fromCharCode.apply(null, codeArray);
        return YP.unify(Number, YP.parseNumberString(numberString));
    }
    else {
        if (!YP.number(Number))
            throw new PrologException
                (new Functor2("type_error", Atom.a("number"), Number), 
                 "Arg 1 Number is not var or number");
        var numberString = YP.convertNumber(Number).toString();

        var codeList = Atom.NIL;
        // Start from the back to make the list.
        for (var i = numberString.length - 1; i >= 0; --i)
            codeList = new ListPair(numberString.charCodeAt(i), codeList);
        return YP.unify(List, codeList);
    }
}
                                 
// Used by number_chars and number_codes.  Return the number in numberString or
// throw an exception if can't parse.
YP.parseNumberString = function(numberString) {
    if (numberString.length == 3 && numberString.substr(0, 2) == "0'")
        // This is a char code.
        return numberString.charCodeAt(2);
    if (numberString.length >= 2 && numberString.substr(0, 2) == "0x") {
        var fromHex = parseInt(numberString.substr(2), 16);
        if (!isNaN(fromHex))
            return fromHex;
        else
            throw new PrologException
                (new Functor1("syntax_error", Atom.a("number_format: " + numberString)),
                 "Arg 2 List is not a list for a hexadecimal number");
    }

    try {
        // Debug: Should use something other than eval because if numberString is a variable name, 
        //   it will use it too.
        return eval(numberString);
    }
    catch (FormatException) {
        throw new PrologException
            (new Functor1("syntax_error", Atom.a("number_format: " + numberString)),
             "Arg 2 List is not a list for a number");
    }
}

YP.char_code = function(Char, Code) {
    Char = YP.getValue(Char);
    Code = YP.getValue(Code);

    var codeInt = 0;
    if (!(Code instanceof Variable)) {
        if (!YP.integer(Code))
            throw new PrologException
                (new Functor2("type_error", Atom.a("integer"), Code),
                 "Arg 2 Code is not var or a character code");
        codeInt = YP.convertNumber(Code);
        if (codeInt < 0)
            throw new PrologException
                (new Functor1("representation_error", Atom.a("character_code")),
                 "Arg 2 Code is not a character code");
    }

    if (Char instanceof Variable) {
        if (Code instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "Arg 1 Char and arg 2 Code are both unbound variables");

        return YP.unify(Char, Atom.a(String.fromCharCode(codeInt)));
    }
    else {
        if (!(Char instanceof Atom) || Char._name.length != 1)
            throw new PrologException
                (new Functor2("type_error", Atom.a("character"), Char), 
                 "Arg 1 Char is not var or one-character atom");

        if (Code instanceof Variable)
            return YP.unify(Code, Char._name.charCodeAt(0));
        else
            // Use codeInt to handle whether Code is supplied as, e.g., 97 or 0'a .
            return YP.unify(codeInt, Char._name.charCodeAt(0));
    }
}
                                 
// If term is an Atom or functor type, return its name.
// Otherwise, return term.
YP.getFunctorName = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor1)
        return term._name;
    else if (term instanceof Functor2)
        return term._name;
    else if (term instanceof Functor3)
        return term._name;
    else if (term instanceof Functor)
        return term._name;
    else
        return term;
}

// If term is an Atom or functor type, return an array of its args.
// Otherwise, return an empty array.
YP.getFunctorArgs = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor1)
        return [term._arg1];
    else if (term instanceof Functor2)
        return [term._arg1, term._arg2];
    else if (term instanceof Functor3)
        return [term._arg1, term._arg2, term._arg3];
    else if (term instanceof Functor)
        return term._args;
    else
        return [];
}

YP.var = function(Term) {
    return YP.getValue(Term) instanceof Variable;
}

YP.nonvar = function(Term) {
    return !YP.var(Term);
}

YP.atom = function(Term) {
    return YP.getValue(Term) instanceof Atom;
}

YP.integer = function(Term) {
   Term = YP.getValue(Term);
   // Debug: Test for no fractional part.  It would be better if Javascript had separate int and float.
   return YP.number(Term) && Math.round(Term) == Term;
}
                           
// Use isFloat instead of float because it is a reserved keyword.                           
YP.isFloat = function(Term) {
   Term = YP.getValue(Term);
   // Debug: Test for a fractional part.  This gives the wrong answer when Term is a float like 1.0 .
   return YP.number(Term) && Math.round(Term) != Term;
}

YP.number = function(Term) {
    return typeof(YP.getValue(Term)) == 'number'
}

YP.atomic = function(Term) {
    return YP.atom(Term) || YP.number(Term);
}

YP.compound = function(Term) {
    Term = YP.getValue(Term);
    return Term instanceof Functor1 || Term instanceof Functor2 || Term instanceof Functor3 || 
      Term instanceof Functor;
}

YP._inputStream = null;
// If input is a Prolog list, read the character codes from it.
// Otherwise input must be a object with a read and close function.
YP.see = function(input) {
   input = YP.getValue(input);
   if (input instanceof Variable)
     throw new PrologException(Atom.a("instantiation_error"), "Arg is an unbound variable");

   if (input == null) {
     YP._inputStream = null;
     return;
   }
   if (input instanceof Functor2 && input._name == Atom.DOT) {
     YP._inputStream = new YP.CodeListReader(input);
     return;
   }
   else if (input.read !== undefined && input.close !== undefined) {
     YP._inputStream = input;
     return;
   }
   else
     throw new PrologException
        (new Functor2("domain_error", Atom.a("stream_or_alias"), input),
         "Input stream specifier not recognized");
}

YP.seen = function() {
	if (YP._inputStream != null) {
		YP._inputStream.close();
		YP._inputStream = null;
	}
}
                        
YP.current_input = function(Stream) {
   return YP.unify(Stream, YP._inputStream);
}

YP._outputStream = null;
// output must be an object with write and close functions.
YP.tell = function(output) {
  output = YP.getValue(output);
  if (output instanceof Variable)
    throw new PrologException(Atom.a("instantiation_error"), "Arg is an unbound variable");

  if (output == null) {
    YP._outputStream = null;
    return;
  }
  if (output.write !== undefined && output.close != undefined) {
       YP._outputStream = output;
       return;
  }
  else
     throw new PrologException
        (new Functor2("domain_error", Atom.a("stream_or_alias"), output),
         "Can't open stream for " + output);
}

YP.told = function() {
	if (YP._outputStream != null) {
		YP._outputStream.close();
		YP._outputStream = null;
	}
}
                        
YP.current_output = function(Stream) {
   return YP.unify(Stream, YP._outputStream);
}

YP.write = function(x) {
    if (YP._outputStream == null)
        return;
	YP._outputStream.write(YP.getValue(x));
}

YP.put_code = function(x) {
    if (YP._outputStream == null)
        return;
    if (YP.var(x))
        throw new PrologException(Atom.a("instantiation_error"), "Arg 1 is an unbound variable");
    YP._outputStream.write(String.fromCharCode(YP.convertNumber(x)));
}

YP.nl = function() {
    if (YP._outputStream == null)
        return;
	YP._outputStream.writeLine("");
}

YP.get_code = function(code) {
    if (YP._inputStream == null)
        return YP.unify(code, -1);
    else
  	    return YP.unify(code, YP._inputStream.read());
}

YP.asserta = function(Term, declaringClass) {
    YP.assertDynamic(Term, declaringClass, true);
}

YP.assertz = function(Term, declaringClass) {
    YP.assertDynamic(Term, declaringClass, false);
}

// The object index is a name/arity string like "pred/2".  The value is an object 
//   like { _name: Atom.a("pred"), _arity:2, _clauses: [new IndexedAnswers()] }.
YP._predicatesStore = new Object();

YP.assertDynamic = function(Term, declaringClass, prepend) {
    Term = YP.getValue(Term);
    if (Term instanceof Variable)
        throw new PrologException("instantiation_error", "Term to assert is an unbound variable");

    var copyStore = new Variable.CopyStore();
    var TermCopy = YP.makeCopy(Term, copyStore);
    var Head, Body;
    if (TermCopy instanceof Functor2 && TermCopy._name == Atom.RULE) {
        Head = YP.getValue(TermCopy._arg1);
        Body = YP.getValue(TermCopy._arg2);
        if (Head instanceof Variable)
            throw new PrologException("instantiation_error", "Head to assert is an unbound variable");
        if (Body instanceof Variable)
            throw new PrologException("instantiation_error", "Body to assert is an unbound variable");
    }
    else {
        Head = TermCopy;
        Body = Atom.TRUE;
    }

    var name = YP.getFunctorName(Head);
    if (!(name instanceof Atom))
        // name is a non-Atom, such as a number.
        throw new PrologException
            (new Functor2("type_error", Atom.a("callable"), Head), "Term to assert is not callable");
    var args = YP.getFunctorArgs(Head);
    if (YP.isSystemPredicate(name, args.length))
        throw new PrologException
            (new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"),
                          new Functor2(Atom.SLASH, name, args.length)),
             "Assert cannot modify static predicate " + name + "/" + args.length);

    if (copyStore.getNUniqueVariables() == 0 && Body == Atom.TRUE) {
        // This is a fact with no unbound variables
        // assertFact and prependFact use IndexedAnswers, so don't we don't need to compile.
        if (prepend)
            YP.prependFact(name, args);
        else
            YP.assertFact(name, args);

        return;
    }

    var clause = Compiler.compileAnonymousClause(Head, Body, declaringClass);
    // We expect clause to be a ClauseHeadAndBody (from Compiler.compileAnonymousFunction)
    //   so we can set the Head and Body.
    if (clause instanceof YP.ClauseHeadAndBody)
        clause.setHeadAndBody(Head, Body);

    // Add the clause to the entry in _predicatesStore.
    var nameAndArity = name + "/" + args.length;
    var clauses = YP._predicatesStore[nameAndArity]; 
    if (clauses === undefined) {
        // Create an entry for the nameAndArity.
        clauses = { _name: name, _arity: args.length, _clauses: [] };
        YP._predicatesStore[nameAndArity] = clauses;
    }

    if (prepend)
        clauses._clauses.unshift(clause);
    else         
        clauses._clauses.push(clause);
}

YP.isSystemPredicate = function(name, arity) {
    if (arity == 2 && (name == Atom.a(",") || name == Atom.a(";") || name == Atom.DOT))
        return true;
    // Use the same mapping to static predicates in YP as the compiler.
    for each (var l1 in functorCallYPFunctionName(name, arity, new Variable()))
        return true;
    // Debug: Do we need to check if name._module is null?
    return false;
}

// Assert values at the end of the set of facts for the predicate with the
// name and with arity values.length.
// "name" must be an Atom.
// "values" is the array of arguments to the fact predicate.
// It is an error if an value has an unbound variable.</param>
YP.assertFact = function(name, values) {
    var nameAndArity = name + "/" + values.length;
    var clauses = YP._predicatesStore[nameAndArity]; 
    var indexedAnswers;
    if (clauses === undefined) {
        // Create an IndexedAnswers as the first clause of the predicate.                
        indexedAnswers = new IndexedAnswers(values.length);
        clauses = { _name: name, _arity: values.length, _clauses: [indexedAnswers] };
        YP._predicatesStore[nameAndArity] = clauses;
    }
    else {
        indexedAnswers = clauses._clauses[clauses._clauses.length - 1];
        if (!(indexedAnswers instanceof IndexedAnswers))
            // The latest clause is not an IndexedAnswers, so add one.
            clauses._clauses.push(indexedAnswers = new IndexedAnswers(values.length));
    }

   indexedAnswers.addAnswer(values);
}

// Assert values, prepending to the front of the set of facts for the predicate with the
// name and with arity values.length.
// "name" must be an Atom.
// "values" is the array of arguments to the fact predicate.
// It is an error if an value has an unbound variable.</param>
YP.prependFact = function(name, values) {
    var nameAndArity = name + "/" + values.length;
    var clauses = YP._predicatesStore[nameAndArity]; 
    var indexedAnswers;
    if (clauses === undefined) {
        // Create an IndexedAnswers as the first clause of the predicate.                
        indexedAnswers = new IndexedAnswers(values.length);
        clauses = { _name: name, _arity: values.length, _clauses: [indexedAnswers] };
        YP._predicatesStore[nameAndArity] = clauses;
    }
    else {
        indexedAnswers = clauses._clauses[0];
        if (!(indexedAnswers instanceof IndexedAnswers))
            // The first clause is not an IndexedAnswers, so prepend one.
            clauses._clauses.unshift(indexedAnswers = new IndexedAnswers(values.length));
    }

   indexedAnswers.prependAnswer(values);
}

// Match all clauses of the dynamic predicate with the name and with arity
// arguments.length.
// If the predicate is not defined, return the result of YP.unknownPredicate.
// "name" must be an Atom.
// "arguments" is an array of arity number of arguments
// Returns an iterator which you can use in foreach.
YP.matchDynamic = function(name, arguments) {
    var nameAndArity = name + "/" + arguments.length;
    var clauses = YP._predicatesStore[nameAndArity];
    if (clauses === undefined)
        return YP.unknownPredicate
            (name, arguments.length, 
             "Undefined dynamic predicate: " + name + "/" + arguments.length);

    if (clauses._clauses.length == 1)
        // Usually there is only one clause, so return it without needing to wrap it in an iterator.
        return clauses._clauses[0].match(arguments);
    else
        return YP.matchAllClauses(clauses._clauses, arguments);
}

// Call match(arguments) for each IClause in clauses.  We make this a separate
// function so that matchDynamic itself does not need to be an iterator object.
YP.matchAllClauses = function(clauses, arguments) {
    for each (var clause in clauses) {
        for each (var lastCall in clause.match(arguments)) {
            yield false;
            if (lastCall)
                // This happens after a cut in a clause.
                return;
        }
    }
}

// If _prologFlags["unknown"] is fail then return fail(), else if 
//   _prologFlags["unknown"] is warning then write the message to YP.write and
//   return fail(), else throw a PrologException for existence_error.  .
YP.unknownPredicate = function(name, arity, message) {
    YP.establishPrologFlags();

    if (YP._prologFlags["unknown"] == Atom.a("fail"))
        return YP.fail();
    else if (YP._prologFlags["unknown"] == Atom.a("warning")) {
        YP.write(message);
        YP.nl();
        return YP.fail();
    }
    else
        throw new PrologException
            (new Functor2
             (Atom.a("existence_error"), Atom.a("procedure"),
              new Functor2(Atom.SLASH, name, arity)), message);
}

// This is deprecated and just calls matchDynamic. This matches all clauses, 
// not just the ones defined with assertFact.
YP.matchFact = function(name, arguments) {
    return YP.matchDynamic(name, arguments);
}

YP.clause = function(Head, Body) {
    Head = YP.getValue(Head);
    Body = YP.getValue(Body);
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");

    var name = YP.getFunctorName(Head);
    if (!(name instanceof Atom))
        // name is a non-Atom, such as a number.
        throw new PrologException
            (new Functor2("type_error", Atom.a("callable"), Head), "Head is not callable");
    var args = YP.getFunctorArgs(Head);
    if (YP.isSystemPredicate(name, args.length))
        throw new PrologException
            (new Functor3("permission_error", Atom.a("access"), Atom.a("private_procedure"),
                          new Functor2(Atom.SLASH, name, args.length)),
             "clause cannot access private predicate " + name + "/" + args.length);
    if (!(Body instanceof Variable) && !(YP.getFunctorName(Body) instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("callable"), Body), "Body is not callable");

    var nameAndArity = name + "/" + args.length;
    var clauses = YP._predicatesStore[nameAndArity]; 
    if (clauses === undefined)
        return;
    // The caller can assert another clause into this same predicate during yield, so we have to
    //   make a copy of the clauses.
    for each (var predicateClause in [x for each (x in clauses._clauses)]) {
        for each (var l1 in predicateClause.clause(Head, Body))
            yield false;
    }
}

YP.retract = function(Term) {
    Term = YP.getValue(Term);
    if (Term instanceof Variable)
        throw new PrologException("instantiation_error", "Term to retract is an unbound variable");

    var Head, Body;
    if (Term instanceof Functor2 && Term._name == Atom.RULE) {
        Head = YP.getValue(Term._arg1);
        Body = YP.getValue(Term._arg2);
    }
    else {
        Head = Term;
        Body = Atom.TRUE;
    }
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");

    var name = YP.getFunctorName(Head);
    if (!(name instanceof Atom))
        // name is a non-Atom, such as a number.
        throw new PrologException
            (new Functor2("type_error", Atom.a("callable"), Head), "Head is not callable");
    var args = YP.getFunctorArgs(Head);
    if (YP.isSystemPredicate(name, args.length))
        throw new PrologException
            (new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"),
        new Functor2(Atom.SLASH, name, args.length)),
             "clause cannot access private predicate " + name + "/" + args.length);
    if (!(Body instanceof Variable) && !(YP.getFunctorName(Body) instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("callable"), Body), "Body is not callable");

    var nameAndArity = name + "/" + args.length;
    var clauses = YP._predicatesStore[nameAndArity]; 
    if (clauses === undefined)
        return;
    // The caller can assert another clause into this same predicate during yield, so we have to
    //   make a copy of the clauses.
    for each (var predicateClause in [x for each (x in clauses._clauses)]) {
        if (predicateClause instanceof IndexedAnswers) {
            // IndexedAnswers handles its own retract.  Even if it removes all of its
            //   answers, it is OK to leave it empty as one of the elements in clauses.
            for each (var l1 in predicateClause.retract(Head, Body))
                yield false;
        }
        else {
            for each (var l1 in predicateClause.clause(Head, Body)) {
                // Remove predicateClause.
                clauses.splice(clauses.indexOf(predicateClause), 1);
                yield false;
            }
        }
    }
}

// This is deprecated for backward compatibility.  You should use retractall.
// "name" must be an Atom.
// "arguments" is an array of arity number of arguments.
YP.retractFact = function(name, arguments) {
   YP.retractall(Functor.make(name, arguments));
}

// Retract all dynamic clauses which unify with Head.  If this matches all clauses in a predicate,
// the predicate is still defined.  To completely remove the predicate, see abolish.
YP.retractall = function(Head) {
    var name = YP.getFunctorName(Head);
    var arguments = YP.getFunctorArgs(Head);
    if (!(name instanceof Atom))
        return;
    var nameAndArity = name + "/" + arguments.length;
    var clauses = YP._predicatesStore[nameAndArity];
    if (clauses === undefined)
        // Can't find, so ignore.
        return;

    for each (var arg in arguments) {
        if (!YP.var(arg))
            throw "Until matching retractall is supported, all arguments must be unbound to retract all clauses";
    }
    // Set to a fresh empty IndexedAnswers.
    clauses = { _name: name, _arity: arguments.length, _clauses: [new IndexedAnswers(arguments.length)] };
    YP._predicatesStore[nameAndArity] = clauses;
}

// If NameSlashArity is var, match with all the dynamic predicates using the
// Name/Artity form.
// If NameSlashArity is not var, check if the Name/Arity exists as a static or
// dynamic predicate.
// declaringClass: if not null, used to resolve references to the default 
//   module Atom.a("") 
YP.current_predicate = function(NameSlashArity, declaringClass) {
    NameSlashArity = YP.getValue(NameSlashArity);
    // First check if Name and Arity are nonvar so we can do a direct lookup.
    if (YP.ground(NameSlashArity)) {
  		  if (!(NameSlashArity instanceof Functor2 && NameSlashArity._name == Atom.SLASH))
            throw new PrologException
                (new Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity), 
                 "Must be a name/arity predicate indicator");
        var name = YP.getValue(NameSlashArity._arg1);
        var arity = YP.getValue(NameSlashArity._arg2);
        if (name instanceof Variable || arity instanceof Variable)
            throw new PrologException
                ("instantiation_error", "Predicate indicator name or arity is an unbound variable");
        if (!(name instanceof Atom && typeof(arity) == "number"))
            throw new PrologException
                (new Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity),
                 "Must be a name/arity predicate indicator");
        if (arity < 0)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("not_less_than_zero"), arity),
                 "Arity may not be less than zero");

        if (Compiler.isCurrentPredicate(name, arity, declaringClass))
       	    // The predicate is defined.
            yield false;
    }
    else {
        for each (var nameArityAnswers in YP._predicatesStore) {
            for each (var l1 in YP.unify
                (new Functor2(Atom.SLASH, nameArityAnswers._name, nameArityAnswers._arity), NameSlashArity))
                yield false;
        }
    }
}

// Return true if the dynamic predicate store has an entry for the predicate
// with name and arity.
YP.isDynamicCurrentPredicate = function(name, arity) {
    var nameAndArity = name + "/" + arity;
    return YP._predicatesStore[nameAndArity] !== undefined;
}

YP.abolish = function(NameSlashArity) {
    NameSlashArity = YP.getValue(NameSlashArity);
    if (NameSlashArity instanceof Variable)
        throw new PrologException
            ("instantiation_error", "Predicate indicator is an unbound variable");
    if (!(NameSlashArity instanceof Functor2 && NameSlashArity._name == Atom.SLASH))
        throw new PrologException
            (new Functor2("type_error", Atom.a("predicate_indicator"), NameSlashArity), 
             "Must be a name/arity predicate indicator");
    var name = YP.getValue(NameSlashArity._arg1);
    var arity = YP.getValue(NameSlashArity._arg2);
    if (name instanceof Variable || arity instanceof Variable)
        throw new PrologException
            ("instantiation_error", "Predicate indicator name or arity is an unbound variable");
    if (!(name instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("atom"), name),
             "Predicate indicator name must be an atom");
    if (typeof(arity) != "number")
        throw new PrologException
            (new Functor2("type_error", Atom.a("integer"), arity),
             "Predicate indicator arity must be an integer");
    if (arity < 0)
        throw new PrologException
            (new Functor2("domain_error", Atom.a("not_less_than_zero"), arity),
             "Arity may not be less than zero");
    if (arity > YP.MAX_ARITY)
        throw new PrologException
            (new Functor1("representation_error", Atom.a("max_arity")),
             "Arity may not be greater than " + YP.MAX_ARITY);

    if (YP.isSystemPredicate(name, arity))
        throw new PrologException
            (new Functor3("permission_error", Atom.a("modify"), Atom.a("static_procedure"),
                          new Functor2(Atom.SLASH, name, arity)),
             "Abolish cannot modify static predicate " + name + "/" + arity);
             
    var nameAndArity = name + "/" + arity;
    // Remove the entry.
    YP._predicatesStore[nameAndArity] = undefined;
}

// If Goal is a simple predicate, call YP.getFunctorName(Goal) using arguments from 
// YP.getFunctorArgs(Goal). If not found, this throws a PrologException for existence_error.
// Otherwise, compile the goal as a single clause predicate and invoke it. 
// declaringClass: if not null, used to resolve references to the default 
//   module Atom.a("") 
YP.getIterator = function(Goal, declaringClass) {
    var name;
    var args;
    while (true) {
        Goal = YP.getValue(Goal);
        if (Goal instanceof Variable)
            throw new PrologException("instantiation_error", "Goal to call is an unbound variable");
        name = YP.getFunctorName(Goal);
        if (!(name instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("callable"), Goal), "Goal to call is not callable");
        args = YP.getFunctorArgs(Goal);
        if (name == Atom.HAT && args.length == 2)
            // Assume this is called from a bagof operation.  Skip the leading qualifiers.
            Goal = YP.getValue(Goal._arg2);
        else
            break;
    }

    var simpleIterator = Compiler.getSimpleIterator(name, args, declaringClass);
    if (simpleIterator != null)
        // We don't need to compile since the goal is a simple predicate which we call directly.
        return simpleIterator;

    // Compile the goal as a clause.
    var variableSet = [];
    YP.addUniqueVariables(Goal, variableSet);

    // Use Atom.F since it is ignored.
    return Compiler.compileAnonymousClause
        (Functor.make(Atom.F, variableSet), Goal).match(variableSet);
}

YP.throwException = function(Term) {
	throw new PrologException(Term);
}

YP._prologFlags = new Object();

// This must be called by any function that uses YP._prologFlags to make sure
// the initial defaults are loaded.
YP.establishPrologFlags = function() {
    if (YP._prologFlags["bounded"] !== undefined)
        // Already established.
        return;

    // List these in the order they appear in the ISO standard.
    YP._prologFlags["bounded"] = Atom.TRUE;
    YP._prologFlags["max_integer"] = 2147483647;
    YP._prologFlags["min_integer"] = -2147483648;
    YP._prologFlags["integer_rounding_function"] = Atom.a("toward_zero");
    YP._prologFlags["char_conversion"] = Atom.a("off");
    YP._prologFlags["debug"] = Atom.a("off");
    YP._prologFlags["max_arity"] = YP.MAX_ARITY;
    YP._prologFlags["unknown"] = Atom.a("error");
    YP._prologFlags["double_quotes"] = Atom.a("codes");
}

YP.current_prolog_flag = function(Key, Value) {
    YP.establishPrologFlags();

    Key = YP.getValue(Key);
    Value = YP.getValue(Value);

    if (Key instanceof Variable) {
        // Bind all key values.
        for (var key in YP._prologFlags) {
            for each (var l1 in YP.unify(Key, Atom.a(key))) {
                for each (var l2 in YP.unify(Value, YP._prologFlags[key]))
                    yield false;
            }
        }
    }
    else
    {
        if (!(Key instanceof Atom))
            throw new PrologException
                (new Functor2("type_error", Atom.a("atom"), Key), "Arg 1 Key is not an atom");
        if (YP._prologFlags[Key._name] === undefined)
            throw new PrologException
                (new Functor2("domain_error", Atom.a("prolog_flag"), Key), 
                "Arg 1 Key is not a recognized flag");

        for each (var l1 in YP.unify(Value, YP._prologFlags[Key._name]))
            yield false;
    }
}

YP.set_prolog_flag = function(Key, Value) {
    YP.establishPrologFlags();

    Key = YP.getValue(Key);
    Value = YP.getValue(Value);

    if (Key instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"),
            "Arg 1 Key is an unbound variable");
    if (Value instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"),
            "Arg 1 Key is an unbound variable");
    if (!(Key instanceof Atom))
        throw new PrologException
            (new Functor2("type_error", Atom.a("atom"), Key), "Arg 1 Key is not an atom");

    var keyName = Key._name;
    if (YP._prologFlags[keyName] === undefined)
        throw new PrologException
            (new Functor2("domain_error", Atom.a("prolog_flag"), Key),
            "Arg 1 Key " + Key + " is not a recognized flag");

    var valueIsOK = false;
    if (keyName == "char_conversion")
        valueIsOK = (Value == YP._prologFlags[keyName]);
    else if (keyName == "debug")
        valueIsOK = (Value == YP._prologFlags[keyName]);
    else if (keyName == "unknown")
        valueIsOK = (Atom.a("fail").equals(Value) || Atom.a("warning").equals(Value) ||
            Atom.a("error").equals(Value));
    else if (keyName == "double_quotes")
        valueIsOK = (Value == Atom.a("codes") || Value == Atom.a("chars") ||
            Value == Atom.a("atom"));
    else
        throw new PrologException
            (new Functor3("permission_error", Atom.a("modify"), Atom.a("flag"), Key),
             "May not modify Prolog flag " + Key);

    if (!valueIsOK)
        throw new PrologException
            (new Functor2("domain_error", Atom.a("flag_value"), new Functor2("+", Key, Value)),
            "May not set arg 1 Key " + Key + " to arg 2 Value" + Value);

    YP._prologFlags[keyName] = Value;
}

// An iterator that does zero loops.
YP.Fail = function() {
}

YP.Fail.prototype.__iterator__ = function() {
    return this;
}

YP.Fail.prototype.next = function() {
    throw StopIteration;
}

YP.Fail.prototype.close = function() {
}

YP._fail = new YP.Fail();

// An iterator that does one loop.
YP.Succeed = function() {
    this._didIteration = false;
}

YP.Succeed.prototype.__iterator__ = function() {
    return this;
}

YP.Succeed.prototype.next = function() {
    if (!this._didIteration) {
        this._didIteration = true;
        return false;
	}
    else
        throw StopIteration;
}

YP.Succeed.prototype.close = function() {
}

// An iterator that repeats forever.
YP.Repeat = function() {
}

YP.Repeat.prototype.__iterator__ = function() {
    return this;
}        

YP.Repeat.prototype.next = function() {
    return false;
}

YP.Repeat.prototype.close = function() {
}

YP._repeat = new YP.Repeat();

// YP.Catch is an iterator that wraps another iterator in order to catch a PrologException.
// Call YP.getIterator(Goal, declaringClass) and save the returned iterator.
// If getIterator throws an exception, save it the same as next().
YP.Catch = function(Goal, declaringClass) {
    this._exception = null;
    try {
        this._enumerator = Iterator(YP.getIterator(Goal, declaringClass));
    }
    catch (exception) {
        if (exception instanceof PrologException)
            // next() will check this.
            this._exception = exception;
        else
            throw exception;
    }
}

YP.Catch.prototype.__iterator__ = function() {
    return this;
}        

// Call _enumerator.next().  If it throws a PrologException, set _exception
// and throw StopIteration.  After this throws StopIteration, call unifyExceptionOrThrow.
YP.Catch.prototype.next = function() {
    if (this._exception != null)
        throw StopIteration;
        
    try {
        return this._enumerator.next();
    }
    catch (exception) {
        if (exception instanceof PrologException) {
            this._exception = exception;
            throw StopIteration;
        }
        else
            // This includes StopIteration.
            throw exception;
    }
}

// Call this after next() returns false to check for an exception.  If
// next did not get a PrologException, don't yield.
// Otherwise, unify the exception with Catcher and yield so the caller can
// do the handler code.  However, if can't unify with Catcher then throw the exception.
YP.Catch.prototype.unifyExceptionOrThrow = function(Catcher) {
    if (this._exception != null) {
        var didUnify = false;
        for each (var l1 in YP.unify(this._exception._term, Catcher)) {
            didUnify = true;
            yield false;
        }
        if (!didUnify)
            throw this._exception;
    }
}

YP.Catch.prototype.close = function() {
    this._enumerator.close();
}

// A ClauseHeadAndBody is used in Compiler.compileAnonymousFunction as a base class
// in order to implement YP.IClause.  After creating the object, you must call setHeadAndBody.
YP.ClauseHeadAndBody = function() {
    this._Head = null;
    this._Body = null;
}

YP.ClauseHeadAndBody.prototype.setHeadAndBody = function(Head, Body) {
    this._Head = Head;
    this._Body = Body;
}

YP.ClauseHeadAndBody.prototype.clause = function(Head, Body) {
    if (this._Head == null || this._Body == null)
        return YP.fail();

    // First, check if we have a match without the cost of makeCopy.
    var gotMatch = false;
    for each (var l1 in YP.unify(Head, this._Head)) {
        gotMatch = true;
        break;
    }

    if (gotMatch) {
        // We have to return a copy of _Body where the variables from _Head are bound properly.
        var copyStore = new Variable.CopyStore();
        var RuleCopy = YP.makeCopy(new Functor2(Atom.RULE, this._Head, this._Body), copyStore);

        return YP.unify(new Functor2(Atom.RULE, Head, Body), RuleCopy);
    }
    else
        return YP.fail();
}

YP.StringReader = function(inputString) {
	this._inputString = inputString;
    this._inputReadIndex = 0;
}

// Return the character code of the next character in the inputString or -1 if past the end.
YP.StringReader.prototype.read = function() {
	if (this._inputReadIndex >= this._inputString.length)
		return -1;
	return this._inputString.charCodeAt(this._inputReadIndex++);
}

YP.StringReader.prototype.close = function() {
}

// A YP.StringWriter has write and writeLine to append to a string buffer.
// To get the result, call toString.
// A YP.StringWriter object can be used in YP.tell.
YP.StringWriter = function() {
	this._stringArray = [];
}

YP.StringWriter.prototype.write = function(text) {
	this._stringArray.push(text);
}

YP.StringWriter.prototype.writeLine = function(text) {
	if (text !== undefined)
		this._stringArray.push(text);
	this._stringArray.push("\n");
}

// Convert the results so far to a string and return it.
YP.StringWriter.prototype.toString = function() {
	return this._stringArray.join("");
}

YP.StringWriter.prototype.close = function() {
}

// Write to the HTML document, using <br> for newline.
YP.HtmlDocumentWriter = function() {
}

YP.HtmlDocumentWriter.prototype.write = function(text) {
    // Debug: should escape &lt;
	document.write(text);
}

YP.HtmlDocumentWriter.prototype.writeLine = function(text) {
	if (text !== undefined)
		document.write(text);
	document.write("<br>");
}

YP.HtmlDocumentWriter.prototype.close = function() {
}

// CodeListReader has a method to read the next code from
// the CodeList which is a Prolog list of integer character codes.
YP.CodeListReader = function(CodeList) {
    this._CodeList = YP.getValue(CodeList);
}

// If the head of _CodeList is an integer, return it and advance the list.  Otherwise,
// return -1 for end of file.
YP.CodeListReader.prototype.read = function() {
    if (!(this._CodeList instanceof Functor2 && this._CodeList._name == Atom.DOT &&
          YP.integer(this._CodeList._arg1))) {
        this._CodeList = Atom.NIL;
        return -1;
    }

    var code = YP.convertNumber(this._CodeList._arg1);
    // Advance.
    this._CodeList = YP.getValue(this._CodeList._arg2);
    return code;
}

YP.CodeListReader.prototype.close = function() {
}
