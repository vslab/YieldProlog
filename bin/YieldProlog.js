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
	
This file is a concatenation of all the source files for Yield Prolog so
  that you only need to include one file.
	
This file was created with the following script (see file "makeYieldProlog.bat"):
copy /B readme.js + makeYieldProlog.bat + endComment.js + ^
  YP.js + ^
  Variable.js + ^
  Atom.js + ^
  Functor1.js + ^
  Functor2.js + ^
  Functor3.js + ^
  Functor.js + ^
  ListPair.js + ^
  PrologException.js + ^
  IndexedAnswers.js + ^
  FindallAnswers.js + ^
  BagofAnswers.js + ^
  Parser.js + ^
  Compiler.js ^
  YieldProlog.js
rem Copy the result to the web interface and bin.
copy YieldProlog.js ..\..\doc
copy YieldProlog.js ..\..\bin
copy ..\..\doc\queryEditor.html ..\..\bin
*/
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

function Variable() {
    // Use _isBound separate from _value so that it can be bound to any value,
    //   including null.
	this._isBound = false;
}

// If this Variable is unbound, then just return this Variable.
// Otherwise, if this has been bound to a value with unify, return the value.
// If the bound value is another Variable, this follows the "variable chain"
// to the end and returns the final value, or the final Variable if it is unbound.
// For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
Variable.prototype.getValue = function() {
    if (!this._isBound)
        return this;

    var result = this._value;
    while (result instanceof Variable) {
        if (!result._isBound)
            return result;

        // Keep following the Variable chain.
        result = result._value;
	}

    return result;
}

// If this Variable is bound, then just call YP.unify to unify this with arg.
// (Note that if arg is an unbound Variable, then YP.unify will bind it to
// this Variable's value.)
// Otherwise, bind this Variable to YP.getValue(arg) and yield once.  After the
// yield, return this Variable to the unbound state.
// For more details, see http://yieldprolog.sourceforge.net/tutorial1.html
Variable.prototype.unify = function(arg) {
  if (!this._isBound) {
      this._value = YP.getValue(arg);
		if (this._value == this)
			// We are unifying this unbound variable with itself, so leave it unbound.
			yield false;
		else {
			this._isBound = true;
			try {
				yield false;
			} finally {
        		// Remove the binding.
        		this._isBound = false;
			}
		}
	}
   else {
	    for each (var l1 in YP.unify(this, arg))
            yield false;
	}
}

Variable.prototype.toString = function() {
    var value = this.getValue();
    if (value === this)
        return "_Variable";
    else
        return value.toString();
}

// If bound, call YP.addUniqueVariables on the value.  Otherwise, if this unbound
// variable is not already in variableSet, add it.
Variable.prototype.addUniqueVariables = function(variableSet) {
	if (this._isBound)
		YP.addUniqueVariables(this.getValue(), variableSet);
	else {
		if (variableSet.indexOf(this) < 0)
			variableSet.push(this);
	}
}

// If bound, return YP.makeCopy for the value, else return copyStore.getCopy(this).
// However, if copyStore is null, just return this.
Variable.prototype.makeCopy = function(copyStore) {
    if (this._isBound)
        return YP.makeCopy(this.getValue(), copyStore);
    else 
        return copyStore == null ? this : copyStore.getCopy(this);
}

Variable.prototype.termEqual = function(term) {
	if (this._isBound)
    	return YP.termEqual(this.getValue(), term);
    else
    	return this === YP.getValue(term);
}

Variable.prototype.ground = function() {
    if (this._isBound)
        // This is usually called by YP.ground which already did getValue, so this
        //   should never be reached, but check anyway.
        return YP.ground(this.getValue());
    else
        return false;
}

// A CopyStore is used by makeCopy to track which Variable objects have
// been copied.
Variable.CopyStore = function() {
 	this._inVariableList = []
	this._outVariableList = []
}

// If inVariable has already been copied, return its copy. Otherwise,
// return a fresh Variable associated with inVariable.
Variable.CopyStore.prototype.getCopy = function(inVariable) {
    var index = this._inVariableList.indexOf(inVariable);
    if (index >= 0)
        return this._outVariableList[index];
    else {
        var outVariable = new Variable();
        this._inVariableList.push(inVariable);
        this._outVariableList.push(outVariable);
        return outVariable;
    }
}

// Return the number of unique variables that have been copied.
Variable.CopyStore.prototype.getNUniqueVariables = function() {
    return this._inVariableList.length;
}
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

// You should not call this constructor, but use Atom.a instead.
function Atom(name, module) {
    this._name = name;
    if (module == undefined)
      this._module = null;
    else
      this._module = module;
}

Atom._atomStore = new Object();
// Return an Atom object with the name and module.  If module is null or Atom.NIL,
// this behaves like Atom.a(name, undefined) and returns the unique object where the module is null.  
// If module is null or Atom.NIL, return a unique Atom object.
// If module is not null or Atom.NIL, this may or may not be the same object as another Atom
// with the same name and module.
// You should use this to create an Atom instead of calling the Atom constructor. 
Atom.a = function(name, module) {
   if (module == undefined || module == null || module == Atom.NIL) {
      var atom = Atom._atomStore[name];
      if (atom === undefined) {
	      atom = new Atom(name);
         Atom._atomStore[name] = atom;
	  }
	  return atom;
   }
   else
     return new Atom(name, module);
}
                                               
// If Obj is an Atom unify its _module with Module.  If the Atom's _module is null, use Atom.NIL.
Atom.module = function(Obj, Module) {
    Obj = YP.getValue(Obj);
    if (Obj instanceof Atom) {
        if (Obj._module == null)
            return YP.unify(Module, Atom.NIL);
        else
            return YP.unify(Module, Obj._module);
    }
    return YP.fail();
}
                                               
Atom.NIL = Atom.a("[]");
Atom.DOT = Atom.a(".");
Atom.F = Atom.a("f");
Atom.SLASH = Atom.a("/");
Atom.HAT = Atom.a("^");
Atom.RULE = Atom.a(":-");
Atom.TRUE = Atom.a("true");
                                                                  
Atom.prototype.unify = function(arg)
{
    arg = YP.getValue(arg);
    if (arg instanceof Atom)
        return this.equals(arg) ? YP.succeed() : YP.fail();
    else if (arg instanceof Variable)
        return arg.unify(this);
    else
        return YP.fail();
}

Atom.prototype.addUniqueVariables = function(variableSet)
{
    // Atom does not contain variables.
}

Atom.prototype.makeCopy = function(copyStore)
{
    // Atom does not contain variables that need to be copied.
    return this;
}

Atom.prototype.termEqual = function(term)
{
    return this.equals(YP.getValue(term));
}

Atom.prototype.ground = function()
{
    // Atom is always ground.
    return true;
}
                                                          
Atom.prototype.equals = function(obj) {
    if (obj instanceof Atom) {
        if (this._module == null && obj._module == null)
            // When _declaringClass is null, we always use an identical object from _atomStore.
            return this == obj;
        // Otherwise, ignore _declaringClass and do a normal string compare on the _name.
        return this._name == obj._name;
    }
    return false;
}                                                          
                                                          
Atom.prototype.toString = function() {
    return this._name;
}

Atom.prototype.toQuotedString = function() {
    if (this._name.length == 0)
        return "''";
    else if (this == Atom.NIL)
        return "[]";

    var result = new Array("");
    var useQuotes = false;
    for each (var c in this._name) {
        if (c == '\'') {
            result.push("''");
            useQuotes = true;
	}
        else if (c == '_' || c >= 'a' && c <= 'z' || 
				 c >= 'A' && c <= 'Z' || c >= '0' && c <= '9')
           	result.push(c);
        else {
            // Debug: Need to handle non-printable chars.
            result.push(c);
            useQuotes = true;
		}
	}

    if (!useQuotes && this._name[0] >= 'a' && this._name[0] <= 'z')
        return result.join("");
    else
        // Surround in single quotes.
        result.push('\'');
        return "'" + result.join("");
}


// Return true if _name is lexicographically less than atom._name.
Atom.prototype.lessThan = function(atom) {
    return this._name < atom._name;
}
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

function Functor1(name, arg1) {
	if (name instanceof Atom)
    	this._name = name;
	else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._arg1 = arg1;
}

// If arg is another Functor1, then succeed (yield once) if this and arg have the
// same name and the functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor1.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor1)
    {
        if (this._name.equals(arg._name)) {
			for each (var l1 in YP.unify(this._arg1, arg._arg1))
				yield false;
		}
    }
    else if (arg instanceof Variable) {
        for each (var l1 in arg.unify(this))
			yield false;
	}
}

Functor1.prototype.toString = function() {
    return this._name + "(" + YP.getValue(this._arg1) + ")";
}

Functor1.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor1)
        return this._name.equals(term._name) && YP.termEqual(this._arg1, term._arg1);
    return false;
}

Functor1.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    return YP.termLessThan(this._arg1, functor._arg1);
}

Functor1.prototype.ground = function() {
    return YP.ground(this._arg1);
}

Functor1.prototype.addUniqueVariables = function(variableSet) {
    YP.addUniqueVariables(this._arg1, variableSet);
}

Functor1.prototype.makeCopy = function(copyStore) {
    return new Functor1(this._name, YP.makeCopy(this._arg1, copyStore));
}

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

function Functor2(name, arg1, arg2) {
	if (name instanceof Atom)
    	this._name = name;
	else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._arg1 = arg1;
    this._arg2 = arg2;
}

// If arg is another Functor2, then succeed (yield once) if this and arg have the
// same name and all functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor2.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor2)
    {
        if (this._name.equals(arg._name)) {
			for each (var l1 in YP.unify(this._arg1, arg._arg1)) {
				for each (var l1 in YP.unify(this._arg2, arg._arg2))
					yield false;
			}
		}
    }
    else if (arg instanceof Variable) {
        for each (var l1 in arg.unify(this))
			yield false;
	}
}

Functor2.prototype.toString = function() {
	if (this._name == Atom.DOT)
		return Functor2.listPairToString(this);
	else
    	return this._name + "(" + YP.getValue(this._arg1) + ", " + YP.getValue(this._arg2) + ")";
}

Functor2.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor2)
        return this._name.equals(term._name) && YP.termEqual(this._arg1, term._arg1) && 
		    YP.termEqual(this._arg2, term._arg2);
    return false;
}

Functor2.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    if (!YP.termEqual(this._arg1, functor._arg1))
        return YP.termLessThan(this._arg1, functor._arg1);

    return YP.termLessThan(this._arg2, functor._arg2);
}

Functor2.prototype.ground = function() {
    return YP.ground(this._arg1) && YP.ground(this._arg2);
}

Functor2.prototype.addUniqueVariables = function(variableSet) {
    YP.addUniqueVariables(this._arg1, variableSet);
    YP.addUniqueVariables(this._arg2, variableSet);
}

Functor2.prototype.makeCopy = function(copyStore) {
    return new Functor2(this._name, YP.makeCopy(this._arg1, copyStore), 
		YP.makeCopy(this._arg2, copyStore));
}

Functor2.listPairToString = function(listPair) {
    var result = "[";
    while (true) {
        var head = YP.getValue(listPair._arg1);
        var tail = YP.getValue(listPair._arg2);
        if (tail == Atom.NIL) {
            result += "" + head;
            break;
		}
        else if (tail instanceof Functor2 && tail._name == Atom.DOT) {
            result += head + ", ";
            listPair = tail;
            // Loop again.
		}
        else {
            // The list is not terminated with NIL.
            result += head + "|" + tail;
            break;
		}
    }
	
    result += "]";
    return result;
}

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

function Functor3(name, arg1, arg2, arg3) {
	if (name instanceof Atom)
    	this._name = name;
	else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._arg1 = arg1;
    this._arg2 = arg2;
    this._arg3 = arg3;
}

// If arg is another Functor3, then succeed (yield once) if this and arg have the
// same name and all functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor3.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor3)
    {
        if (this._name.equals(arg._name)) {
			for each (var l1 in YP.unify(this._arg1, arg._arg1)) {
				for each (var l1 in YP.unify(this._arg2, arg._arg2)) {
					for each (var l1 in YP.unify(this._arg3, arg._arg3))
						yield false;
				}
			}
		}
    }
    else if (arg instanceof Variable) {
        for each (var l1 in arg.unify(this))
			yield false;
	}
}

Functor3.prototype.toString = function() {
    return this._name + "(" + YP.getValue(this._arg1) + ", " + YP.getValue(this._arg2) + ", " +
        YP.getValue(this._arg3) + ")";
}

Functor3.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor3)
        return this._name.equals(term._name) && YP.termEqual(this._arg1, term._arg1) && 
		    YP.termEqual(this._arg2, term._arg2) && YP.termEqual(this._arg3, term._arg3);
    return false;
}

Functor3.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    if (!YP.termEqual(this._arg1, functor._arg1))
        return YP.termLessThan(this._arg1, functor._arg1);

    if (!YP.termEqual(this._arg2, functor._arg2))
        return YP.termLessThan(this._arg2, functor._arg2);

    return YP.termLessThan(this._arg3, functor._arg3);
}

Functor3.prototype.ground = function() {
    return YP.ground(this._arg1) && YP.ground(this._arg2) && YP.ground(this._arg3);
}

Functor3.prototype.addUniqueVariables = function(variableSet) {
    YP.addUniqueVariables(this._arg1, variableSet);
    YP.addUniqueVariables(this._arg2, variableSet);
    YP.addUniqueVariables(this._arg3, variableSet);
}

Functor3.prototype.makeCopy = function(copyStore) {
    return new Functor3(this._name, YP.makeCopy(this._arg1, copyStore), 
		YP.makeCopy(this._arg2, copyStore), YP.makeCopy(this._arg3, copyStore));
}

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

function Functor(name, args) {
    if (args.length < 3)
    {
        if (args.length == 0)
            throw "For arity 0 functor, just use name as an Atom";
        else if (args.length == 1)
            throw "For arity 1 functor, use Functor1";
        else if (args.length == 2)
            throw "For arity 2 functor, use Functor2";
        else if (args.length == 3)
            throw "For arity 3 functor, use Functor3";
        else
            // (This shouldn't happen, but include it for completeness.
            throw "Cannot create a Functor of arity " + args.length;
    }

    if (name instanceof Atom)
    	this._name = name;
    else
		// Assume name is a string.
		this._name = Atom.a(name);
    this._args = args;
}

// Return an Atom, Functor1, Functor2, Functor3 or Functor depending on the
// length of args.  
// Note that this is different than the Functor constructor which requires
// the length of args to be greater than 3.
Functor.make = function(name, args) {
   if (!(name instanceof Atom))
	    // Assume name is a string.
	    name = Atom.a(name);

    if (args.length <= 0)
        return name;
    else if (args.length == 1)
        return new Functor1(name, args[0]);
    else if (args.length == 2)
        return new Functor2(name, args[0], args[1]);
    else if (args.length == 3)
        return new Functor3(name, args[0], args[1], args[2]);
    else
        return new Functor(name, args);
}

// If arg is another Functor, then succeed (yield once) if this and arg have the
// same name and all functor args unify, otherwise fail (don't yield).
// If arg is a Variable, then call its unify to unify with this.
// Otherwise fail (don't yield).
Functor.prototype.unify = function(arg) {
    arg = YP.getValue(arg);
    if (arg instanceof Functor)
    {
        if (this._name.equals(arg._name))
            return YP.unifyArrays(this._args, arg._args);
        else
            return YP.fail();
    }
    else if (arg instanceof Variable)
        return arg.unify(this);
    else
        return YP.fail();
}

Functor.prototype.toString = function() {
    var result = this._name + "(" + YP.getValue(this._args[0]);
    for (var i = 1; i < this._args.length; ++i)
        result += ", " + YP.getValue(this._args[i]);
    result += ")";
    return result;
}

Functor.prototype.termEqual = function(term) {
    term = YP.getValue(term);
    if (term instanceof Functor) {
        if (this._name.equals(term._name) && this._args.length == term._args.length) {
            for (var i = 0; i < this._args.length; ++i)
            {
                if (!YP.termEqual(this._args[i], term._args[i]))
                    return false;
            }
            return true;
		}
	}
    return false;
}

Functor.prototype.lessThan = function(functor) {
    // Do the equal check first since it is faster.
    if (!this._name.equals(functor._name))
        return this._name.lessThan(functor._name);

    if (this._args.length != functor._args.length)
        return this._args.length < functor._args.length;

    for (var i = 0; i < this._args.length; ++i) {
        if (!YP.termEqual(this._args[i], functor._args[i]))
            return YP.termLessThan(this._args[i], functor._args[i]);
    }

    return false;
}

Functor.prototype.ground = function() {
	for (var i = 0; i < this._args.length; ++i) {
		if (!YP.ground(this._args[i]))
            return false;
	}
	return true;
}

Functor.prototype.addUniqueVariables = function(variableSet) {
	for (var i = 0; i < this._args.length; ++i)
    	YP.addUniqueVariables(this._args[i], variableSet);
}

Functor.prototype.makeCopy = function(copyStore) {
	var argsCopy = [YP.makeCopy(arg, copyStore) for each (arg in this._args)];
    return new Functor(this._name, argsCopy);
}

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

function ListPair(head, tail) {
    Functor2.call(this, Atom.DOT, head, tail);
}

ListPair.prototype = new Functor2;

ListPair.make = function(arg1, arg2, arg3) {
    if (arg3 !== undefined)
		return new ListPair(arg1, new ListPair(arg2, new ListPair(arg3, Atom.NIL)));
    if (arg2 !== undefined)
		return new ListPair(arg1, new ListPair(arg2, Atom.NIL));
	
    if (arg1 instanceof Array) {
        if (arg1.length <= 0)
            return Atom.NIL;

        var result = Atom.NIL;
        // Start from the end.
        for (var i = arg1.length - 1; i >= 0; --i)
            result = new ListPair(arg1[i], result);
        return result;
    }
    else
  	  return new ListPair(arg1, Atom.NIL);
}

// Return a ListPair version of array, where repeated elements (according to YP.termEqual) are removed.
ListPair.makeWithoutRepeatedTerms = function(array) {
	if (array.length <= 0)
		return Atom.NIL;

	// Start from the end.
	var previousTerm = array[array.length - 1];
	var result = new ListPair(previousTerm, Atom.NIL);
	for (var i = array.length - 2; i >= 0; --i) {
		var term = array[i];
		if (YP.termEqual(term, previousTerm))
			continue;
		result = new ListPair(term, result);
        previousTerm = term;
	}
	return result;
}

// Return an array of the elements in list or null if it is not
// a proper list.  If list is Atom.NIL, return an array of zero elements.
// If the list or one of the tails of the list is Variable, raise an instantiation_error.
// This does not call YP.getValue on each element.
ListPair.toArray = function(list) {
    list = YP.getValue(list);
    if (list == Atom.NIL)
        return [];

    var result = [];
    var element = list;
    while(true) {
        if (element == Atom.NIL)
            break;
        if (element instanceof Variable)
            throw new PrologException(Atom.a("instantiation_error"),
                "List tail is an unbound variable");
        if (!((element instanceof Functor2) && element._name == Atom.DOT))
            // Not a proper list.
            return null;
        result.push(element._arg1);
        element = YP.getValue(element._arg2)
    }

    if (result.length <= 0)
        return null;
    return result;
}
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

// A PrologException is used as the exception thrown by YP.throw(Term).
// One argument: Create a PrologException with the given arg1 term.  The printable exception message is 
//   the full Term.
// Two arguments: Create a PrologException where the Term is error(arg1, arg2).  If arg2 is a string, this
//   converts it to an Atom so that Prolog code can use it.
//   This uses YP.makeCopy to copy the arguments so that they are valid after unbinding.
function PrologException(arg1, arg2) {
    if (arg2 !== undefined) {
      arg2 = YP.getValue(arg2);
		this._message = arg2.toString();
		if (typeof(arg2) == 'string')
		  arg2 = Atom.a(arg2);
		this._term = YP.makeCopy(new Functor2(Atom.a("error"), arg1, arg2), new Variable.CopyStore());
	}
	else if (arg1 !== undefined) {
		this._message = YP.getValue(arg1).toString();
		this._term = YP.makeCopy(arg1, new Variable.CopyStore());
	}
}

PrologException.prototype.toString = function() {
    return this._message;
}

PrologException.TypeErrorInfo = function(Type, Culprit, Message) {
    this._Type = Type;
    this._Culprit = Culprit;
    this._Message = Message;
}

// Return the TypeErrorInfo for this exception, or null if _term does not match
//   error(type_error(Type, Culprit), Message).
PrologException.prototype.getTypeErrorInfo = function() {
    if (!(this._term instanceof Functor2 && this._term._name._name == "error"))
        return null;
    var errorTerm = this._term._arg1;
    if (!(errorTerm instanceof Functor2 && errorTerm._name._name == "type_error"))
        return null;
    if (!(errorTerm._arg1 instanceof Atom))
        return null;
    return new PrologException.TypeErrorInfo(errorTerm._arg1, errorTerm._arg2, this._term._arg2);
}

PrologException.ExistenceErrorInfo = function(Type, Culprit, Message) {
    this._Type = Type;
    this._Culprit = Culprit;
    this._Message = Message;
}

// If _Type is procedure and _Culprit is name/artity, return the name.  Otherwise return null.
PrologException.ExistenceErrorInfo.prototype.getProcedureName = function() {
    if (!(this._Type._name == "procedure" && 
          this._Culprit instanceof Functor2 && this._Culprit._name == Atom.SLASH))
        return null;
    return this._Culprit._arg1;
}

// If _Type is procedure and _Culprit is name/arity and arity is an integer, return the arity.  
// Otherwise return -1.
PrologException.ExistenceErrorInfo.prototype.getProcedureArity = function() {
    if (!(this._Type._name == "procedure" &&
          this._Culprit instanceof Functor2 && this._Culprit._name == Atom.SLASH))
        return -1;
    if (typeof(this._Culprit._arg2) != "number")
        return -1;
    return this._Culprit._arg2;
}

// Return the ExistenceErrorInfo for this exception, or null if _term does not match
//   error(existence_error(Type, Culprit), Message).  If the returned ExistenceErrorInfo _Culprit is
//   procedure, you can use its getProcedureName and getProcedureArity.
PrologException.prototype.getExistenceErrorInfo = function() {
    if (!(this._term instanceof Functor2 && this._term._name._name == "error"))
        return null;
    var errorTerm = this._term._arg1;
    if (!(errorTerm instanceof Functor2 && errorTerm._name._name == "existence_error"))
        return null;
    if (!(errorTerm._arg1 instanceof Atom))
        return null;
    return new PrologException.ExistenceErrorInfo(errorTerm._arg1, errorTerm._arg2, this._term._arg2);
}
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

// An IndexedAnswers holds answers to a query based on the values of index arguments.
function IndexedAnswers(arity) {
    this._arity = arity;
    // addAnswer adds the answer here and indexes it later.
    this._allAnswers = [];
    // The key has the arity of answers with non-null values for each indexed arg.  The value
    //   is a list of the matching answers.  The signature is implicit in the pattern on non-null index args.
    this._indexedAnswers = new Object();
    // Keeps track of whether we have started adding entries to _indexedAnswers for the signature.
    this._gotAnswersForSignature = new Object();
}

IndexedAnswers.MAX_INDEX_ARGS = 31;
                                                                    
// Append the answer to the list and update the indexes, if any.
// Elements of answer must be ground, since arguments with unbound variables make this
// into a dynamic rule which we don't index.
IndexedAnswers.prototype.addAnswer = function(answer) {
    this.addOrPrependAnswer(answer, false);
}               

// Prepend the answer to the list and clear the indexes so that they must be re-computed
// on the next call to match.  (Only addAnswer will maintain the indexes while adding answers.)
// Elements of answer must be ground, since arguments with unbound variables make this
// into a dynamic rule which we don't index.
IndexedAnswers.prototype.prependAnswer = function(answer) {
    this.addOrPrependAnswer(answer, true);
}               
   
// Do the work of addAnswer or prependAnswer.
IndexedAnswers.prototype.addOrPrependAnswer = function(answer, prepend) {
    if (answer.length != this._arity)
        return;

    // Store a copy of the answer array.
    var copyStore = new Variable.CopyStore();
    var answerCopy = [YP.makeCopy(value, copyStore)
                        for each (value in answer)];
    if (copyStore.getNUniqueVariables() > 0)
        throw "Elements of answer must be ground, but found " + copyStore.getNUniqueVariables() +
              " unbound variables";

    if (prepend) {
        this._allAnswers.unshift(answerCopy);
        this.clearIndexes();
    }
    else {
        this._allAnswers.push(answerCopy);
        // If match has already indexed answers for a signature, we need to add
        //   this to the existing indexed answers.
        for(var signature in this._gotAnswersForSignature)
            this.indexAnswerForSignature(answerCopy, signature);
    }
}

IndexedAnswers.prototype.indexAnswerForSignature = function(answer, signature) {
    // First find out which of the answer values can be used as an index.
    var indexValues = [IndexedAnswers.getIndexValue(YP.getValue(value))
                        for each (value in answer)];
    // We limit the number of indexed args in a 32-bit signature.
    for (var i = IndexedAnswers.MAX_INDEX_ARGS; i < indexValues.length; ++i)
	    indexValues[i] = null;

    // We need an entry in indexArgs from indexValues for each 1 bit in signature.
    var indexArgs = [];
    for (var i = 0; i < indexValues.length; ++i) {
        if ((signature & (1 << i)) == 0)
            indexArgs.push(null);
        else {
            if (indexValues[i] == null)
                // The signature wants an index value here, but we don't have one so
                //   we can't add it as an answer for this signature.
                return;
            else
                indexArgs.push(indexValues[i]);
        }
    }

    this.add(indexArgs, answer);
}

// Assume indexArgs is an array.  Return the array of answers from _indexedAnswers
//   for the indexArgs, or undefined in not found.
IndexedAnswers.prototype.get = function(indexArgs) {
    // The key lookup converts indexArgs to a string and may clash with a different
	//   indexArgs that converts to the same string, we we actually store an array
	//   of objects that have {_key: indexArgs, _value: answers} and we have to
	//   look through this array to make sure we got the right indexArgs.
    var keyValueArray = this._indexedAnswers[indexArgs];
	if (keyValueArray === undefined)
	    return undefined;
	for each (var keyValue in keyValueArray) {
	    if (IndexedAnswers.arrayEquals(keyValue._key, indexArgs))
		    return keyValue._value;
    }
	
	return undefined;
}

// Add answer to _indexedAnswers for indexArgs, creating the entry if needed.
IndexedAnswers.prototype.add = function(indexArgs, answer) {
	var answers = this.get(indexArgs);
	if (answers === undefined) {
   	    answers = [];
        var keyValueArray = this._indexedAnswers[indexArgs];
		if (keyValueArray === undefined) {
		  keyValueArray = []
		  this._indexedAnswers[indexArgs] = keyValueArray;
		}
        keyValueArray.push({_key: indexArgs, _value: answers});
	}

    answers.push(answer);
}

IndexedAnswers.prototype.match = function(arguments) {
    if (arguments.length != this._arity)
        return;

    // Set up indexArgs, up to arg position MAX_INDEX_ARGS.  The signature has a 1 bit for
    //   each non-null index arg.
    var indexArgs = [];
    var gotAllIndexArgs = true;
    var signature = 0;
    for (var i = 0; i < arguments.length; ++i) {
        var indexValue = null;
        if (i < IndexedAnswers.MAX_INDEX_ARGS) {
            // We limit the number of args in a 32-bit signature.
            indexValue = IndexedAnswers.getIndexValue(YP.getValue(arguments[i]));
            if (indexValue != null)
                signature += (1 << i);
        }
        if (indexValue == null)
            gotAllIndexArgs = false;
        indexArgs.push(indexValue);
    }

	var answers;
    if (signature == 0)
        // No index args, so we have to match from _allAnswers.
        answers = this._allAnswers;
    else {
        if (this._gotAnswersForSignature[signature] === undefined) {
            // We need to create the entry in _indexedAnswers.
            for each (var answer in this._allAnswers)
                this.indexAnswerForSignature(answer, signature);
            // Mark that we did this signature.
            this._gotAnswersForSignature[signature] = null;
        }
	    answers = this.get(indexArgs);
    	if (answers === undefined)
            return;
    }

    if (gotAllIndexArgs) {
        // All the arguments were already bound, so we don't need to do bindings.
        yield false;
        return;
    }

    // Find matches in answers.
    var iterators = [];
    for each (var answer in answers) {
        var gotMatch = true;
        var nIterators = 0;
        // Try to bind all the arguments.
        for (var i = 0; i < arguments.length; ++i) {
            if (indexArgs[i] != null)
                // We already matched this argument by looking up _indexedAnswers.
                continue;

            var iterator = Iterator(YP.unify(arguments[i], answer[i]));
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
}

IndexedAnswers.prototype.clause = function(Head, Body) {
    Head = YP.getValue(Head);
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");
    var arguments = YP.getFunctorArgs(Head);

    // We always match Head from _allAnswers, and the Body is Atom.TRUE.
    for each (var l1 in YP.unify(Body, Atom.TRUE)) {
        // The caller can assert another answer into this same predicate during yield, so we have to
        //   make a copy of the answers.
        for each (var answer in [x for each (x in this._allAnswers)]) {
            for each (var l2 in YP.unifyArrays(arguments, answer))
                yield false;
        }
    }
}

IndexedAnswers.prototype.retract = function(Head, Body) {
    Head = YP.getValue(Head);
    if (Head instanceof Variable)
        throw new PrologException("instantiation_error", "Head is an unbound variable");
    var arguments = YP.getFunctorArgs(Head);

    // We always match Head from _allAnswers, and the Body is Atom.TRUE.
    for each (var l1 in YP.unify(Body, Atom.TRUE)) {
        // The caller can assert another answer into this same predicate during yield, so we have to
        //   make a copy of the answers.
        for each (var answer in [x for each (x in this._allAnswers)]) {
            for each (var l2 in YP.unifyArrays(arguments, answer)) {
                // Remove answer.
                this._allAnswers.splice(this._allAnswers.indexOf(answer), 1);
                this.clearIndexes();
                yield false;
            }
        }
    }
}

// After retracting or prepending an answer in _allAnswers, the indexes are invalid, so clear them.
IndexedAnswers.prototype.clearIndexes = function()
{
    this._indexedAnswers = new Object();
    this._gotAnswersForSignature = new Object();
}

// Assume a1 and a2 are arrays.  Return true if both have the same
//   length and all elements are equal.
IndexedAnswers.arrayEquals = function(a1, a2) {
    if (a1.length != a2.length)
        return false;

    for (var i = 0; i < a1.length; ++i) {
        if (a1[i] != a2[i])
            return false;
	}
    return true;
}

// If we keep an index on value, return the value, or null if we don't index it.
IndexedAnswers.getIndexValue = function(value) {
    if (value instanceof Atom || typeof(value) == "string" /* || typeof(value) == "number" */)
        return value;
    else
        return null;
}

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

// A FindallAnswers holds answers for findall.
function FindallAnswers(Template) {
    this._template = Template;
    this._bagArray = [];
}

FindallAnswers.prototype.add = function() {
	this._bagArray.push(YP.makeCopy(this._template, new Variable.CopyStore()));
}

FindallAnswers.prototype.resultArray = function() {
	return this._bagArray;
}

// Unify Bag with the result. This frees the internal answers, so you can only call this once.
FindallAnswers.prototype.result = function(Bag) {
    var result = ListPair.make(this._bagArray);
    // Try to free the memory.
    this._bagArray = null;
    return YP.unify(Bag, result);
}

// This is a simplified findall when the goal is a single call.
FindallAnswers.findall = function(Template, goal, Bag) {
    var findallAnswers = new FindallAnswers(Template);
    for each (var l1 in goal)
        findallAnswers.add();
	return findallAnswers.result(Bag);	
}

// Like findall, except return an array of the results.
FindallAnswers.findallArray = function(Template, goal) {
    var findallAnswers = new FindallAnswers(Template);
    for each (var l1 in goal)
        findallAnswers.add();
	return findallAnswers.resultArray();	
}
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

// A BagofAnswers holds answers for bagof and setof.

// To get the free variables, split off any existential qualifiers from Goal such as the X in 
// "X ^ f(Y)", get the set of unbound variables in Goal that are not qualifiers, then remove
// the unbound variables that are qualifiers as well as the unbound variables in Template.
function BagofAnswers(Template, Goal) {
    this._template = Template;

    // First get the set of variables that are not free variables.
    var variableSet = [];
    YP.addUniqueVariables(Template, variableSet);
    var UnqualifiedGoal = YP.getValue(Goal);
    while (UnqualifiedGoal instanceof Functor2 && UnqualifiedGoal._name == Atom.HAT) {
        YP.addUniqueVariables(UnqualifiedGoal._arg1, variableSet);
        UnqualifiedGoal = YP.getValue(UnqualifiedGoal._arg2);
    }

    // Remember how many non-free variables there are so we can find the unique free variables 
    //   that are added.
    var nNonFreeVariables = variableSet.length;
    YP.addUniqueVariables(UnqualifiedGoal, variableSet);
    var nFreeVariables = variableSet.length - nNonFreeVariables;
    if (nFreeVariables == 0) {
        // There were no free variables added, so we won't waste time with _bagForFreeVariables.
        this._freeVariables = null;
        this._findallBagArray = [];
    }
    else {
        // Copy the free variables.
        this._freeVariables = [];
        for (var i = 0; i < nFreeVariables; ++i)
            this._freeVariables[i] = variableSet[i + nNonFreeVariables];

        this._bagForFreeVariables = [];
    }
}

BagofAnswers.prototype.add = function() {
    if (this._freeVariables == null)
        // The goal has bound the values in _template but we don't bother with _freeVariables.
        this._findallBagArray.push(YP.makeCopy(this._template, new Variable.CopyStore()));
    else
    {
        // The goal has bound the values in _template and _freeVariables.
		// Copy the _freeVariables using YP.getValues so that we preserve them after unbinding.
        var freeVariableValues = [YP.getValue(value) for each (value in this._freeVariables)];
		
        // Find the entry in _bagForFreeVariables for this set of _freeVariables values.
		// _bagForFreeVariables is an array of objects like { _freeVariables: [], _bagArray: [] }
		// Debug: If Javascript develops support for a generalized Dictionary object, use it.
        var bagArray = null;
		for each (var valuesAndBag in this._bagForFreeVariables) {
		  if (BagofAnswers.arraysTermEqual(valuesAndBag._freeVariables, freeVariableValues)) {
		      bagArray = valuesAndBag._bagArray;
			  break;
		  }
		}
		if (bagArray == null) {
		    // Didn't find the freeVariables, so create a new entry.
            bagArray = [];
			this._bagForFreeVariables.push({ _freeVariables: freeVariableValues, _bagArray: bagArray });
        }

        // Now copy the template and add to the bag for the freeVariables values.
        bagArray.push(YP.makeCopy(this._template, new Variable.CopyStore()));
    }
}

// For each result, unify the _freeVariables and unify bagArrayVariable with the associated bag.
// bagArrayVariable is unified with the array of matches for template that 
// corresponds to the bindings for freeVariables.  Be very careful: this does not unify with a Prolog list.
BagofAnswers.prototype.resultArray = function(bagArrayVariable) {
    if (this._freeVariables == null) {
        // No unbound free variables, so we only filled one bag.  If empty, bagof fails.
        if (this._findallBagArray.length > 0) {
            for each (var l1 in bagArrayVariable.unify(this._findallBagArray))
                yield false;
        }
    }
    else {
		for each (var valuesAndBag in this._bagForFreeVariables) {
            for each (var l1 in YP.unifyArrays(this._freeVariables, valuesAndBag._freeVariables)) {
                for each (var l2 in bagArrayVariable.unify(valuesAndBag._bagArray))
                    yield false;
            }
            // Debug: Should we free memory of the answers already returned?
        }
    }
}

// For each result, unify the _freeVariables and unify Bag with the associated bag.
BagofAnswers.prototype.result = function(Bag) {
    var bagArrayVariable = new Variable();
    for each (var l1 in this.resultArray(bagArrayVariable)) {
        for each (var l2 in YP.unify(Bag, ListPair.make(bagArrayVariable.getValue())))
            yield false;
    }
}

// For each result, unify the _freeVariables and unify Bag with the associated bag which is sorted
// with duplicates removed, as in setof.
BagofAnswers.prototype.resultSet = function(Bag) {
    var bagArrayVariable = new Variable();
    for each (var l1 in this.resultArray(bagArrayVariable)) {
        var bagArray = bagArrayVariable.getValue();
        YP.sortArray(bagArray);
        for each (var l2 in YP.unify(Bag, ListPair.makeWithoutRepeatedTerms(bagArray)))
            yield false;
    }
}

// Return true if all members of the arrays are YP.termEqual.
BagofAnswers.arraysTermEqual = function(array1, array2) {
    if (array1.length != array2.length)
        return false;
    for (var i = 0; i < array1.length; ++i) {
        if (!YP.termEqual(array1[i], array2[i]))
            return false;
    }
    return true;
}

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

function Parser() {
}

Parser.read_term2 = function(Term, Options) {
    var Answer = new Variable();
    var Variables = new Variable();
    for each (var l1 in Parser.read_termOptions(Options, Variables)) {
        for each (var l2 in portable_read3(Answer, Variables, new Variable())) {
            for each (var l3 in remove_pos(Answer, Term))
                yield false;
        }
    }
}

Parser.read_term3 = function(Input, Term, Options) {
    var SaveInput = new Variable();
    var Answer = new Variable();
    var Variables = new Variable();
    for each (var l1 in Parser.read_termOptions(Options, Variables)) {
        for each (var l2 in YP.current_input(SaveInput)) {
            try {
                YP.see(Input);
                for each (var l3 in portable_read3(Answer, Variables, new Variable())) {
                    for each (var l4 in remove_pos(Answer, Term))
                        yield false;
                }
            }
            finally {
                YP.see(SaveInput);
            }
        }
    }
}

// For read_term, check if Options has variable_names(Variables).
// Otherwise, ignore Options.
Parser.read_termOptions = function(Options, Variables) {
    Options = YP.getValue(Options);
    if (Options instanceof Variable)
        throw new PrologException(Atom.a("instantiation_error"), "Options is an unbound variable");
    // First try to match Options = [variable_names(Variables)]
    for each (var l1 in YP.unify(Options, ListPair.make(new Functor1("variable_names", Variables)))) {
        yield false;
        return;
    }
    // Default: Ignore Options.
    yield false;
}

Parser.read1 = function(Term) {
    return Parser.read_term2(Term, Atom.NIL);
}

Parser.read2 = function(Input, Term) {
    return Parser.read_term3(Input, Term, Atom.NIL);
}

function formatError(Output, Format, Arguments) {
    // Debug: Simple implementation for now.
    YP.write(Format);
    YP.write(Arguments);
    YP.nl();
    yield false;
}

// Debug: Hand-modify this central predicate to do tail recursion.
function read_tokens(arg1, arg2, arg3) {
    var repeat = true;
    while (repeat) {
        repeat = false;

        cutIf9:
        cutIf8:
        cutIf7:
        cutIf6:
        cutIf5:
        cutIf4:
        cutIf3:
        cutIf2:
        cutIf1:
        {
            var C1 = arg1;
            var Dict = arg2;
            var Tokens = arg3;
            var C2 = new Variable();
            if (YP.lessThanOrEqual(C1, new ListPair(32, Atom.NIL))) {
                if (YP.greaterThanOrEqual(C1, 0)) {
                    for each (var l4 in YP.get_code(C2)) {
/*
                        for each (var l5 in read_tokens(C2, Dict, Tokens)) {
                            yield false;
                        }
*/
                        arg1 = YP.getValue(C2);
                        arg2 = YP.getValue(Dict);
                        arg3 = YP.getValue(Tokens);
                        repeat = true;
                    }
                }
                break cutIf1;
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(97, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(122, Atom.NIL))) {
                    for each (var l4 in read_identifier(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf2;
                }
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(65, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(90, Atom.NIL))) {
                    for each (var l4 in read_variable(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf3;
                }
            }
            if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
                if (YP.lessThanOrEqual(C1, new ListPair(57, Atom.NIL))) {
                    for each (var l4 in read_number(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf4;
                }
            }
            if (YP.lessThan(C1, 127)) {
                for each (var l3 in read_special(C1, Dict, Tokens)) {
                    yield false;
                }
                break cutIf5;
            }
            if (YP.lessThanOrEqual(C1, 160)) {
                for each (var l3 in YP.get_code(C2)) {
/*
                    for each (var l4 in read_tokens(C2, Dict, Tokens)) {
                        yield false;
                    }
*/
                    arg1 = YP.getValue(C2);
                    arg2 = YP.getValue(Dict);
                    arg3 = YP.getValue(Tokens);
                    repeat = true;
                }
                break cutIf6;
            }
            if (YP.greaterThanOrEqual(C1, 223)) {
                if (YP.notEqual(C1, 247)) {
                    for each (var l4 in read_identifier(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf7;
                }
            }
            if (YP.greaterThanOrEqual(C1, 192)) {
                if (YP.notEqual(C1, 215)) {
                    for each (var l4 in read_variable(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf8;
                }
            }
            if (YP.notEqual(C1, 170)) {
                if (YP.notEqual(C1, 186)) {
                    for each (var l4 in read_symbol(C1, Dict, Tokens)) {
                        yield false;
                    }
                    break cutIf9;
                }
            }
            for each (var l2 in read_identifier(C1, Dict, Tokens)) {
                yield false;
            }
        }
    }
}

// Compiler output follows.

function getDeclaringClass() { return null; }

function parseInput(TermList) {
  {
    var TermAndVariables = new Variable();
    var findallAnswers1 = new FindallAnswers(TermAndVariables);
    for each (var l2 in parseInputHelper(TermAndVariables)) {
      findallAnswers1.add();
    }
    for each (var l2 in findallAnswers1.result(TermList)) {
      yield false;
    }
  }
}

function parseInputHelper(arg1) {
  {
    var Term = new Variable();
    var Variables = new Variable();
    var Answer = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("f", Term, Variables))) {
      for each (var l3 in YP.repeat()) {
        for each (var l4 in portable_read3(Answer, Variables, x4)) {
          for each (var l5 in remove_pos(Answer, Term)) {
            cutIf1:
            {
              if (YP.termEqual(Term, Atom.a("end_of_file"))) {
                return;
                break cutIf1;
              }
              yield false;
            }
          }
        }
      }
    }
  }
}

function clear_errors() {
  {
    yield false;
  }
}

function remove_pos(arg1, arg2) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, X)) {
      for each (var l3 in YP.unify(arg2, X)) {
        if (YP.var(X)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var X = arg2;
    var _Pos = new Variable();
    var _Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("$VAR", _Pos, _Name, X))) {
      if (YP.var(X)) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var H = new Variable();
    var T = new Variable();
    var NH = new Variable();
    var NT = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      for each (var l3 in YP.unify(arg2, new ListPair(NH, NT))) {
        for each (var l4 in remove_pos(H, NH)) {
          for each (var l5 in remove_pos(T, NT)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var A = new Variable();
    var B = new Variable();
    var NA = new Variable();
    var NB = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in YP.unify(arg2, new Functor2(",", NA, NB))) {
        for each (var l4 in remove_pos(A, NA)) {
          for each (var l5 in remove_pos(B, NB)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Atom_1 = new Variable();
    var _F = new Variable();
    for each (var l2 in YP.unify(arg1, Atom_1)) {
      for each (var l3 in YP.unify(arg2, Atom_1)) {
        for each (var l4 in YP.functor(Atom_1, _F, 0)) {
          yield false;
        }
      }
    }
  }
  {
    var Term = arg1;
    var NewTerm = arg2;
    var Func = new Variable();
    var _Pos = new Variable();
    var Args = new Variable();
    var NArgs = new Variable();
    if (YP.nonvar(Term)) {
      for each (var l3 in YP.univ(Term, new ListPair(Func, new ListPair(_Pos, Args)))) {
        for each (var l4 in remove_pos(Args, NArgs)) {
          for each (var l5 in YP.univ(NewTerm, new ListPair(Func, NArgs))) {
            yield false;
          }
        }
      }
    }
  }
}

function portable_read_position(Term, PosTerm, Syntax) {
  {
    for each (var l2 in portable_read(PosTerm, Syntax)) {
      for each (var l3 in remove_pos(PosTerm, Term)) {
        yield false;
      }
    }
  }
}

function portable_read(Answer, Syntax) {
  {
    var Tokens = new Variable();
    var ParseTokens = new Variable();
    for each (var l2 in read_tokens1(Tokens)) {
      for each (var l3 in remove_comments(Tokens, ParseTokens, Syntax)) {
        for each (var l4 in parse2(ParseTokens, Answer)) {
          yield false;
        }
      }
    }
  }
}

function portable_read3(Answer, Variables, Syntax) {
  {
    var Tokens = new Variable();
    var ParseTokens = new Variable();
    for each (var l2 in read_tokens2(Tokens, Variables)) {
      for each (var l3 in remove_comments(Tokens, ParseTokens, Syntax)) {
        for each (var l4 in parse2(ParseTokens, Answer)) {
          yield false;
        }
      }
    }
  }
}

function remove_comments(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield false;
        }
      }
    }
  }
  {
    var Ys = arg2;
    var S = new Variable();
    var E = new Variable();
    var Xs = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("comment", S, E), Xs))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("comment", S, E), Zs))) {
        for each (var l4 in remove_comments(Xs, Ys, Zs)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Pos = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    var Pos2 = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("/", Atom.a("["), Pos), Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("["), Ys))) {
        for each (var l4 in YP.unify(arg3, new ListPair(new Functor2("list", Pos, Pos2), Zs))) {
          for each (var l5 in YP.unify(Pos2, YP.add(Pos, 1))) {
            for each (var l6 in remove_comments(Xs, Ys, Zs)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    var Pos2 = new Variable();
    var Zs = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("/", Atom.a("]"), Pos), Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("]"), Ys))) {
        for each (var l4 in YP.unify(arg3, new ListPair(new Functor2("list", Pos, Pos2), Zs))) {
          for each (var l5 in YP.unify(Pos2, YP.add(Pos, 1))) {
            for each (var l6 in remove_comments(Xs, Ys, Zs)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Zs = arg3;
    var Token = new Variable();
    var Xs = new Variable();
    var Ys = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, Xs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Token, Ys))) {
        for each (var l4 in remove_comments(Xs, Ys, Zs)) {
          yield false;
        }
      }
    }
  }
}

function expect(Token, arg2, arg3) {
  {
    var Rest = arg3;
    for each (var l2 in YP.unify(arg2, new ListPair(Token, Rest))) {
      yield true;
      return;
    }
  }
  {
    var S0 = arg2;
    var x3 = arg3;
    for each (var l2 in syntax_error(ListPair.make([Token, Atom.a("or"), Atom.a("operator"), Atom.a("expected")]), S0)) {
      yield false;
    }
  }
}

function parse2(Tokens, Answer) {
  {
    var Term = new Variable();
    var LeftOver = new Variable();
    for each (var l2 in clear_errors()) {
      for each (var l3 in parse(Tokens, 1200, Term, LeftOver)) {
        for each (var l4 in all_read(LeftOver)) {
          for each (var l5 in YP.unify(Answer, Term)) {
            yield false;
          }
          return;
        }
      }
      for each (var l3 in syntax_error1(Tokens)) {
        yield false;
      }
    }
  }
}

function all_read(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
  {
    var Token = new Variable();
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S))) {
      for each (var l3 in syntax_error(ListPair.make([Atom.a("operator"), Atom.a("expected"), Atom.a("after"), Atom.a("expression")]), new ListPair(Token, S))) {
        yield false;
      }
    }
  }
}

function parse(arg1, arg2, arg3, arg4) {
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in syntax_error(new ListPair(Atom.a("expression"), new ListPair(Atom.a("expected"), Atom.NIL)), Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Precedence = arg2;
    var Term = arg3;
    var LeftOver = arg4;
    var Token = new Variable();
    var RestTokens = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, RestTokens))) {
      for each (var l3 in parse5(Token, RestTokens, Precedence, Term, LeftOver)) {
        yield false;
      }
    }
  }
}

function parse5(arg1, arg2, arg3, arg4, arg5) {
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in cannot_start(Atom.a("}"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in cannot_start(Atom.a("]"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in cannot_start(Atom.a(")"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in cannot_start(Atom.a(","), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in cannot_start(Atom.a("|"), S0)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Codes = new Variable();
    var Term = new Variable();
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", Codes))) {
      cutIf1:
      {
        for each (var l4 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("atom"))) {
          for each (var l5 in YP.atom_codes(Term, Codes)) {
            for each (var l6 in exprtl0(S0, Term, Precedence, Answer, S)) {
              yield false;
            }
          }
          break cutIf1;
        }
        cutIf2:
        {
          for each (var l5 in YP.current_prolog_flag(Atom.a("double_quotes"), Atom.a("chars"))) {
            for each (var l6 in YP.atom_codes(A, Codes)) {
              for each (var l7 in YP.atom_chars(A, Term)) {
                for each (var l8 in exprtl0(S0, Term, Precedence, Answer, S)) {
                  yield false;
                }
              }
            }
            break cutIf2;
          }
          for each (var l5 in YP.unify(Term, Codes)) {
            for each (var l6 in exprtl0(S0, Term, Precedence, Answer, S)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Number = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", Number))) {
      for each (var l3 in exprtl0(S0, Number, Precedence, Answer, S)) {
        yield false;
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("]"), S1))) {
        for each (var l4 in read_atom(new Functor2("/", Atom.NIL, 0), S1, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in parse(S1, 999, Arg1, S2)) {
        for each (var l4 in read_list(S2, RestArgs, S3)) {
          for each (var l5 in exprtl0(S3, new ListPair(Arg1, RestArgs), Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a(")"), S2, S3)) {
          for each (var l5 in exprtl0(S3, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a(")"), S2, S3)) {
          for each (var l5 in exprtl0(S3, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var _Pos = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("{"), _Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("}"), S1))) {
        for each (var l4 in read_atom(Atom.a("{}"), S1, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Pos = new Variable();
    var Term = new Variable();
    var S2 = new Variable();
    var S3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("{"), Pos))) {
      for each (var l3 in parse(S1, 1200, Term, S2)) {
        for each (var l4 in expect(Atom.a("}"), S2, S3)) {
          for each (var l5 in exprtl0(S3, new Functor2("{}", Pos, Term), Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Variable_1 = new Variable();
    var Name = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", Variable_1, Name, Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("("), S1))) {
        for each (var l4 in parse(S1, 999, Arg1, S2)) {
          for each (var l5 in read_args(S2, RestArgs, S3)) {
            for each (var l6 in YP.univ(Term, new ListPair(Atom.a("call"), new ListPair(new Functor3("$VAR", Pos, Name, Variable_1), new ListPair(Arg1, RestArgs))))) {
              for each (var l7 in exprtl0(S3, Term, Precedence, Answer, S)) {
                yield false;
              }
            }
            return;
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Variable_1 = new Variable();
    var Name = new Variable();
    var Pos = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", Variable_1, Name, Pos))) {
      for each (var l3 in exprtl0(S0, new Functor3("$VAR", Pos, Name, Variable_1), Precedence, Answer, S)) {
        yield false;
      }
    }
  }
  {
    var S0 = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Atom_1 = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", Atom_1, P))) {
      for each (var l3 in read_atom(new Functor2("/", Atom_1, P), S0, Precedence, Answer, S)) {
        yield false;
      }
    }
  }
}

function read_atom(arg1, arg2, Precedence, Answer, S) {
  {
    var _Pos = new Variable();
    var Number = new Variable();
    var S1 = new Variable();
    var Negative = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom.a("-"), _Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("number", Number), S1))) {
        for each (var l4 in YP.unify(Negative, YP.negate(Number))) {
          for each (var l5 in exprtl0(S1, Negative, Precedence, Answer, S)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Functor_1 = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var Arg1 = new Variable();
    var S2 = new Variable();
    var RestArgs = new Variable();
    var S3 = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Functor_1, Pos))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Atom.a("("), S1))) {
        for each (var l4 in parse(S1, 999, Arg1, S2)) {
          for each (var l5 in read_args(S2, RestArgs, S3)) {
            for each (var l6 in YP.univ(Term, new ListPair(Functor_1, new ListPair(Pos, new ListPair(Arg1, RestArgs))))) {
              for each (var l7 in exprtl0(S3, Term, Precedence, Answer, S)) {
                yield false;
              }
            }
            return;
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Op = new Variable();
    var Pos = new Variable();
    var Oprec = new Variable();
    var Aprec = new Variable();
    var Flag = new Variable();
    var Term = new Variable();
    var Arg = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Op, Pos))) {
      for each (var l3 in prefixop(Op, Oprec, Aprec)) {
        for each (var l4 in possible_right_operand(S0, Flag)) {
          cutIf1:
          {
            if (YP.lessThan(Flag, 0)) {
              for each (var l7 in YP.univ(Term, new ListPair(Op, new ListPair(Pos, Atom.NIL)))) {
                for each (var l8 in exprtl0(S0, Term, Precedence, Answer, S)) {
                  yield false;
                }
              }
              break cutIf1;
            }
            cutIf2:
            {
              if (YP.greaterThan(Oprec, Precedence)) {
                for each (var l8 in syntax_error(ListPair.make([Atom.a("prefix"), Atom.a("operator"), Op, Atom.a("in"), Atom.a("context"), Atom.a("with"), Atom.a("precedence"), Precedence]), S0)) {
                  yield false;
                }
                break cutIf2;
              }
              cutIf3:
              {
                if (YP.greaterThan(Flag, 0)) {
                  for each (var l9 in parse(S0, Aprec, Arg, S1)) {
                    for each (var l10 in YP.univ(Term, ListPair.make([Op, Pos, Arg]))) {
                      for each (var l11 in exprtl(S1, Oprec, Term, Precedence, Answer, S)) {
                        yield false;
                      }
                    }
                    return;
                  }
                  break cutIf3;
                }
                for each (var l8 in peepop(S0, S1)) {
                  for each (var l9 in prefix_is_atom(S1, Oprec)) {
                    for each (var l10 in exprtl(S1, Oprec, new Functor2("/", Op, Pos), Precedence, Answer, S)) {
                      yield false;
                    }
                  }
                }
                for each (var l8 in parse(S0, Aprec, Arg, S1)) {
                  for each (var l9 in YP.univ(Term, ListPair.make([Op, Pos, Arg]))) {
                    for each (var l10 in exprtl(S1, Oprec, Term, Precedence, Answer, S)) {
                      yield false;
                    }
                  }
                  return;
                }
              }
            }
          }
        }
        return;
      }
    }
  }
  {
    var S0 = arg2;
    var Atom_1 = new Variable();
    var Pos = new Variable();
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", Atom_1, Pos))) {
      for each (var l3 in YP.univ(Term, new ListPair(Atom_1, new ListPair(Pos, Atom.NIL)))) {
        for each (var l4 in exprtl0(S0, Term, Precedence, Answer, S)) {
          yield false;
        }
      }
    }
  }
}

function cannot_start(Token, S0) {
  {
    for each (var l2 in syntax_error(ListPair.make([Token, Atom.a("cannot"), Atom.a("start"), Atom.a("an"), Atom.a("expression")]), S0)) {
      yield false;
    }
  }
}

function read_args(arg1, arg2, arg3) {
  {
    var S = arg3;
    var S1 = new Variable();
    var Term = new Variable();
    var Rest = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a(","), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(Term, Rest))) {
        for each (var l4 in parse(S1, 999, Term, S2)) {
          for each (var l5 in read_args(S2, Rest, S)) {
            yield false;
          }
          return;
        }
        return;
      }
    }
  }
  {
    var S = arg3;
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a(")"), S))) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var S = arg1;
    var x2 = arg2;
    var x3 = arg3;
    for each (var l2 in syntax_error(ListPair.make([Atom.a(", or )"), Atom.a("expected"), Atom.a("in"), Atom.a("arguments")]), S)) {
      yield false;
    }
  }
}

function read_list(arg1, arg2, arg3) {
  {
    var x1 = arg2;
    var x2 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), Atom.NIL)) {
        yield false;
      }
    }
  }
  {
    var Rest = arg2;
    var S = arg3;
    var Token = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S1))) {
      for each (var l3 in read_list4(Token, S1, Rest, S)) {
        yield false;
      }
    }
  }
}

function read_list4(arg1, arg2, arg3, arg4) {
  {
    var S1 = arg2;
    var S = arg4;
    var Term = new Variable();
    var Rest = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Term, Rest))) {
        for each (var l4 in parse(S1, 999, Term, S2)) {
          for each (var l5 in read_list(S2, Rest, S)) {
            yield false;
          }
          return;
        }
        return;
      }
    }
  }
  {
    var S1 = arg2;
    var Rest = arg3;
    var S = arg4;
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in parse(S1, 999, Rest, S2)) {
        for each (var l4 in expect(Atom.a("]"), S2, S)) {
          yield false;
        }
        return;
      }
      return;
    }
  }
  {
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, S1)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          for each (var l5 in YP.unify(arg4, S1)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Token = arg1;
    var S1 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    for each (var l2 in syntax_error(ListPair.make([Atom.a(", | or ]"), Atom.a("expected"), Atom.a("in"), Atom.a("list")]), new ListPair(Token, S1))) {
      yield false;
    }
  }
}

function possible_right_operand(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var Flag = arg2;
    var H = new Variable();
    var T = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(H, T))) {
      for each (var l3 in possible_right_operand3(H, Flag, T)) {
        yield false;
      }
    }
  }
}

function possible_right_operand3(arg1, arg2, arg3) {
  {
    var x4 = arg3;
    var x1 = new Variable();
    var x2 = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", x1, x2, x3))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg3;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", x1))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg3;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", x1))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, new ListPair(Atom.a("]"), x1))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, new ListPair(Atom.a("}"), x1))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    var x3 = arg3;
    var x1 = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", x1, x2))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        yield false;
      }
    }
  }
}

function peepop(arg1, arg2) {
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), new ListPair(Atom.a("("), S1)))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor2("atom", F, Pos), new ListPair(Atom.a("("), S1)))) {
        yield true;
        return;
      }
    }
  }
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var L = new Variable();
    var P = new Variable();
    var R = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L, P, R]), S1))) {
        for each (var l4 in infixop(F, L, P, R)) {
          yield false;
        }
      }
    }
  }
  {
    var F = new Variable();
    var Pos = new Variable();
    var S1 = new Variable();
    var L = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", F, Pos), S1))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L, P), S1))) {
        for each (var l4 in postfixop(F, L, P)) {
          yield false;
        }
      }
    }
  }
  {
    var S0 = new Variable();
    for each (var l2 in YP.unify(arg1, S0)) {
      for each (var l3 in YP.unify(arg2, S0)) {
        yield false;
      }
    }
  }
}

function prefix_is_atom(arg1, arg2) {
  {
    var Precedence = arg2;
    var Token = new Variable();
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, x2))) {
      for each (var l3 in prefix_is_atom(Token, Precedence)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    var x1 = new Variable();
    var L = new Variable();
    var x3 = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor(Atom.a("infixop", Atom.a("")), [x1, L, x3, x4]))) {
      if (YP.greaterThanOrEqual(L, P)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    var x1 = new Variable();
    var L = new Variable();
    var x3 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3(Atom.a("postfixop", Atom.a("")), x1, L, x3))) {
      if (YP.greaterThanOrEqual(L, P)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      yield false;
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      yield false;
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      yield false;
    }
  }
  {
    var P = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      if (YP.greaterThanOrEqual(1100, P)) {
        yield false;
      }
    }
  }
  {
    var P = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      if (YP.greaterThanOrEqual(1000, P)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
}

function exprtl0(arg1, arg2, arg3, arg4, arg5) {
  {
    var x2 = arg3;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var Token = new Variable();
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, S1))) {
      for each (var l3 in exprtl0_6(Token, Term, Precedence, Answer, S, S1)) {
        yield false;
      }
    }
  }
}

function exprtl0_6(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("}"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a("}"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("]"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a("]"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg3;
    var S1 = arg6;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(")"))) {
      for each (var l3 in YP.unify(arg2, Term)) {
        for each (var l4 in YP.unify(arg4, Term)) {
          for each (var l5 in YP.unify(arg5, new ListPair(Atom.a(")"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      cutIf1:
      {
        if (YP.greaterThanOrEqual(Precedence, 1000)) {
          for each (var l5 in parse(S1, 1000, Next, S2)) {
            for each (var l6 in exprtl(S2, 1000, new Functor2(",", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
            return;
          }
          break cutIf1;
        }
        for each (var l4 in YP.unify(Answer, Term)) {
          for each (var l5 in YP.unify(S, new ListPair(Atom.a(","), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      cutIf2:
      {
        if (YP.greaterThanOrEqual(Precedence, 1100)) {
          for each (var l5 in parse(S1, 1100, Next, S2)) {
            for each (var l6 in exprtl(S2, 1100, new Functor2(";", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
            return;
          }
          break cutIf2;
        }
        for each (var l4 in YP.unify(Answer, Term)) {
          for each (var l5 in YP.unify(S, new ListPair(Atom.a("|"), S1))) {
            yield false;
          }
        }
      }
    }
  }
  {
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var S1 = arg6;
    var S = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("string", S))) {
      for each (var l3 in cannot_follow(Atom.a("chars"), new Functor1("string", S), S1)) {
        yield false;
      }
    }
  }
  {
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var S1 = arg6;
    var N = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("number", N))) {
      for each (var l3 in cannot_follow(Atom.a("number"), new Functor1("number", N), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in YP.unify(arg6, new ListPair(Atom.a("}"), S1))) {
        for each (var l4 in exprtl0_atom(Atom.a("{}"), Term, Precedence, Answer, S, S1)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("{"))) {
      for each (var l3 in cannot_follow(Atom.a("brace"), Atom.a("{"), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in YP.unify(arg6, new ListPair(Atom.a("]"), S1))) {
        for each (var l4 in exprtl0_atom(Atom.NIL, Term, Precedence, Answer, S, S1)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("["))) {
      for each (var l3 in cannot_follow(Atom.a("bracket"), Atom.a("["), S1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a("("))) {
      for each (var l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1)) {
        yield false;
      }
    }
  }
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var x4 = arg5;
    var S1 = arg6;
    for each (var l2 in YP.unify(arg1, Atom.a(" ("))) {
      for each (var l3 in cannot_follow(Atom.a("parenthesis"), Atom.a("("), S1)) {
        yield false;
      }
    }
  }
  {
    var x4 = arg2;
    var x5 = arg3;
    var x6 = arg4;
    var x7 = arg5;
    var S1 = arg6;
    var A = new Variable();
    var B = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("var", A, B, P))) {
      for each (var l3 in cannot_follow(Atom.a("variable"), new Functor3("var", A, B, P), S1)) {
        yield false;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var S1 = arg6;
    var F = new Variable();
    var P = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("atom", F, P))) {
      for each (var l3 in exprtl0_atom(new Functor2("/", F, P), Term, Precedence, Answer, S, S1)) {
        yield false;
      }
    }
  }
}

function exprtl0_atom(arg1, arg2, arg3, arg4, arg5, S1) {
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L1 = new Variable();
    var O1 = new Variable();
    var R1 = new Variable();
    var L2 = new Variable();
    var O2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in ambigop(F, Precedence, L1, O1, R1, L2, O2)) {
        for each (var l4 in prefix_is_atom(S1, Precedence)) {
          for each (var l5 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
            yield false;
          }
          return;
        }
        for each (var l4 in exprtl(new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        for each (var l4 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L1 = new Variable();
    var O1 = new Variable();
    var R1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in infixop(F, L1, O1, R1)) {
        for each (var l4 in exprtl(new ListPair(new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L1, O1, R1]), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var Term = arg2;
    var Precedence = arg3;
    var Answer = arg4;
    var S = arg5;
    var F = new Variable();
    var Pos = new Variable();
    var L2 = new Variable();
    var O2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("/", F, Pos))) {
      for each (var l3 in postfixop(F, L2, O2)) {
        for each (var l4 in exprtl(new ListPair(new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L2, O2), S1), 0, Term, Precedence, Answer, S)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    var X = arg1;
    var x2 = arg2;
    var x3 = arg3;
    var x4 = arg4;
    var x5 = arg5;
    var x7 = new Variable();
    for each (var l2 in syntax_error(ListPair.make([new Functor2("-", Atom.a("non"), Atom.a("operator")), X, Atom.a("follows"), Atom.a("expression")]), new ListPair(new Functor2("atom", X, x7), S1))) {
      yield false;
    }
    return;
  }
}

function cannot_follow(Type, Token, Tokens) {
  {
    for each (var l2 in syntax_error(ListPair.make([Type, Atom.a("follows"), Atom.a("expression")]), new ListPair(Token, Tokens))) {
      yield false;
    }
  }
}

function exprtl(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x1 = arg2;
    var x3 = arg4;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Term)) {
        for each (var l4 in YP.unify(arg5, Term)) {
          for each (var l5 in YP.unify(arg6, Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var Token = new Variable();
    var Tokens = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Token, Tokens))) {
      for each (var l3 in exprtl_7(Token, C, Term, Precedence, Answer, S, Tokens)) {
        yield false;
      }
    }
  }
}

function exprtl_7(arg1, arg2, arg3, arg4, arg5, arg6, arg7) {
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var F = new Variable();
    var Pos = new Variable();
    var L = new Variable();
    var O = new Variable();
    var R = new Variable();
    var Other = new Variable();
    var S2 = new Variable();
    var Expr = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor(Atom.a("infixop", Atom.a("")), [new Functor2("/", F, Pos), L, O, R]))) {
      if (YP.greaterThanOrEqual(Precedence, O)) {
        if (YP.lessThanOrEqual(C, L)) {
          for each (var l5 in parse(S1, R, Other, S2)) {
            for each (var l6 in YP.univ(Expr, ListPair.make([F, Pos, Term, Other]))) {
              for each (var l7 in exprtl(S2, O, Expr, Precedence, Answer, S)) {
                yield false;
              }
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var F = new Variable();
    var Pos = new Variable();
    var L = new Variable();
    var O = new Variable();
    var Expr = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3(Atom.a("postfixop", Atom.a("")), new Functor2("/", F, Pos), L, O))) {
      if (YP.greaterThanOrEqual(Precedence, O)) {
        if (YP.lessThanOrEqual(C, L)) {
          for each (var l5 in YP.univ(Expr, ListPair.make([F, Pos, Term]))) {
            for each (var l6 in peepop(S1, S2)) {
              for each (var l7 in exprtl(S2, O, Expr, Precedence, Answer, S)) {
                yield false;
              }
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a(","))) {
      if (YP.greaterThanOrEqual(Precedence, 1000)) {
        if (YP.lessThan(C, 1000)) {
          for each (var l5 in parse(S1, 1000, Next, S2)) {
            for each (var l6 in exprtl(S2, 1000, new Functor2(",", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var C = arg2;
    var Term = arg3;
    var Precedence = arg4;
    var Answer = arg5;
    var S = arg6;
    var S1 = arg7;
    var Next = new Variable();
    var S2 = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.a("|"))) {
      if (YP.greaterThanOrEqual(Precedence, 1100)) {
        if (YP.lessThan(C, 1100)) {
          for each (var l5 in parse(S1, 1100, Next, S2)) {
            for each (var l6 in exprtl(S2, 1100, new Functor2(";", Term, Next), Precedence, Answer, S)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var Token = arg1;
    var x2 = arg2;
    var x4 = arg4;
    var Tokens = arg7;
    var Term = new Variable();
    for each (var l2 in YP.unify(arg3, Term)) {
      for each (var l3 in YP.unify(arg5, Term)) {
        for each (var l4 in YP.unify(arg6, new ListPair(Token, Tokens))) {
          yield false;
        }
      }
    }
  }
}

function syntax_error(_Message, _List) {
  {
    return;
  }
  {
    for each (var l2 in YP.fail()) {
      yield false;
    }
  }
}

function syntax_error1(_List) {
  {
    return;
  }
  {
    for each (var l2 in YP.fail()) {
      yield false;
    }
  }
}

function prefixop(F, O, Q) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("fx"), F)) {
        for each (var l4 in YP.unify(Q, YP.subtract(O, 1))) {
          yield false;
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("fy"), F)) {
          for each (var l5 in YP.unify(Q, O)) {
            yield false;
          }
          break cutIf2;
        }
      }
    }
  }
}

function postfixop(F, P, O) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("xf"), F)) {
        for each (var l4 in YP.unify(P, YP.subtract(O, 1))) {
          yield false;
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("yf"), F)) {
          for each (var l5 in YP.unify(P, O)) {
            yield false;
          }
          break cutIf2;
        }
      }
    }
  }
}

function infixop(F, P, O, Q) {
  {
    cutIf1:
    {
      for each (var l3 in YP.current_op(O, Atom.a("xfy"), F)) {
        for each (var l4 in YP.unify(P, YP.subtract(O, 1))) {
          for each (var l5 in YP.unify(Q, O)) {
            yield false;
          }
        }
        break cutIf1;
      }
      cutIf2:
      {
        for each (var l4 in YP.current_op(O, Atom.a("xfx"), F)) {
          for each (var l5 in YP.unify(P, YP.subtract(O, 1))) {
            for each (var l6 in YP.unify(Q, P)) {
              yield false;
            }
          }
          break cutIf2;
        }
        cutIf3:
        {
          for each (var l5 in YP.current_op(O, Atom.a("yfx"), F)) {
            for each (var l6 in YP.unify(Q, YP.subtract(O, 1))) {
              for each (var l7 in YP.unify(P, O)) {
                yield false;
              }
            }
            break cutIf3;
          }
        }
      }
    }
  }
}

function ambigop(F, Precedence, L1, O1, R1, L2, O2) {
  {
    for each (var l2 in postfixop(F, L2, O2)) {
      if (YP.lessThanOrEqual(O2, Precedence)) {
        for each (var l4 in infixop(F, L1, O1, R1)) {
          if (YP.lessThanOrEqual(O1, Precedence)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_tokens1(arg1) {
  {
    var TokenList = arg1;
    var C1 = new Variable();
    var _X = new Variable();
    var ListOfTokens = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      for each (var l3 in read_tokens(C1, _X, ListOfTokens)) {
        for each (var l4 in YP.unify(TokenList, ListOfTokens)) {
          yield false;
        }
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL))) {
      yield false;
    }
  }
}

function read_tokens2(arg1, arg2) {
  {
    var TokenList = arg1;
    var Dictionary = arg2;
    var C1 = new Variable();
    var Dict = new Variable();
    var ListOfTokens = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      for each (var l3 in read_tokens(C1, Dict, ListOfTokens)) {
        for each (var l4 in terminate_list(Dict)) {
          for each (var l5 in YP.unify(Dictionary, Dict)) {
            for each (var l6 in YP.unify(TokenList, ListOfTokens)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("atom", Atom.a("end_of_file"), 0), Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield false;
      }
    }
  }
}

function terminate_list(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield false;
    }
  }
  {
    var x1 = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, Tail))) {
      for each (var l3 in terminate_list(Tail)) {
        yield false;
      }
    }
  }
}

function read_special(arg1, Dict, arg3) {
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 95)) {
      for each (var l3 in read_variable(95, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 247)) {
      for each (var l3 in read_symbol(247, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 215)) {
      for each (var l3 in read_symbol(215, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var StartPos = new Variable();
    var EndPos = new Variable();
    var Tokens = new Variable();
    var Ch = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 37)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("comment", StartPos, EndPos), Tokens))) {
        for each (var l4 in get_current_position(StartPos)) {
          for each (var l5 in YP.repeat()) {
            for each (var l6 in YP.get_code(Ch)) {
              if (YP.lessThan(Ch, new ListPair(32, Atom.NIL))) {
                if (YP.notEqual(Ch, 9)) {
                  if (YP.termNotEqual(Ch, -1)) {
                    for each (var l10 in get_current_position(EndPos)) {
                      for each (var l11 in YP.get_code(NextCh)) {
                        for each (var l12 in read_tokens(NextCh, Dict, Tokens)) {
                          yield false;
                        }
                      }
                    }
                  }
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var T = arg3;
    var C2 = new Variable();
    var StartPos = new Variable();
    var EndPos = new Variable();
    var Tokens = new Variable();
    var StartPos1 = new Variable();
    var NextCh = new Variable();
    var Chars = new Variable();
    for each (var l2 in YP.unify(arg1, 47)) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.equal(C2, new ListPair(42, Atom.NIL))) {
            for each (var l6 in YP.unify(T, new ListPair(new Functor2("comment", StartPos, EndPos), Tokens))) {
              for each (var l7 in get_current_position(StartPos1)) {
                for each (var l8 in YP.unify(StartPos, YP.subtract(StartPos1, 1))) {
                  for each (var l9 in read_solidus(32, NextCh)) {
                    for each (var l10 in get_current_position(EndPos)) {
                      for each (var l11 in read_tokens(NextCh, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
          for each (var l5 in YP.unify(T, Tokens)) {
            for each (var l6 in rest_symbol(C2, Chars, NextCh)) {
              for each (var l7 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(47, Chars))) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 33)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom.a("!"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 40)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(" ("), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 41)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(")"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 44)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a(","), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 59)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom.a(";"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 91)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("["), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_tokens(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 93)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("]"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_after_atom(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Pos = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 123)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("/", Atom.a("{"), Pos), Tokens))) {
        for each (var l4 in get_current_position(Pos)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_tokens(NextCh, Dict, Tokens)) {
              yield false;
            }
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 124)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("|"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 125)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("}"), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_after_atom(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = arg3;
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 46)) {
      for each (var l3 in YP.get_code(NextCh)) {
        for each (var l4 in read_fullstop(NextCh, Dict, Tokens)) {
          yield false;
        }
      }
    }
  }
  {
    var Chars = new Variable();
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 34)) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("string", Chars), Tokens))) {
        for each (var l4 in read_string(Chars, 34, NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
  {
    var Tokens = arg3;
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 39)) {
      for each (var l3 in read_string(Chars, 39, NextCh)) {
        for each (var l4 in read_after_atom4(NextCh, Dict, Tokens, Chars)) {
          yield false;
        }
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 35)) {
      for each (var l3 in read_symbol(35, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 36)) {
      for each (var l3 in read_symbol(36, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 38)) {
      for each (var l3 in read_symbol(38, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 42)) {
      for each (var l3 in read_symbol(42, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 43)) {
      for each (var l3 in read_symbol(43, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 45)) {
      for each (var l3 in read_symbol(45, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 58)) {
      for each (var l3 in read_symbol(58, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 60)) {
      for each (var l3 in read_symbol(60, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 61)) {
      for each (var l3 in read_symbol(61, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 62)) {
      for each (var l3 in read_symbol(62, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 63)) {
      for each (var l3 in read_symbol(63, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 64)) {
      for each (var l3 in read_symbol(64, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 92)) {
      for each (var l3 in read_symbol(92, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 94)) {
      for each (var l3 in read_symbol(94, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 96)) {
      for each (var l3 in read_symbol(96, Dict, Tokens)) {
        yield false;
      }
    }
  }
  {
    var Tokens = arg3;
    for each (var l2 in YP.unify(arg1, 126)) {
      for each (var l3 in read_symbol(126, Dict, Tokens)) {
        yield false;
      }
    }
  }
}

function read_symbol(C1, Dict, Tokens) {
  {
    var C2 = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.get_code(C2)) {
      for each (var l3 in rest_symbol(C2, Chars, NextCh)) {
        for each (var l4 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(C1, Chars))) {
          yield false;
        }
      }
    }
  }
}

function rest_symbol(arg1, arg2, arg3) {
  {
    var C2 = arg1;
    var LastCh = arg3;
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C2, Chars))) {
      cutIf1:
      {
        if (YP.greaterThan(C2, 160)) {
          if (YP.lessThan(C2, 192)) {
            if (YP.notEqual(C2, 186)) {
              if (YP.notEqual(C2, 170)) {
                for each (var l8 in YP.get_code(NextCh)) {
                  for each (var l9 in rest_symbol(NextCh, Chars, LastCh)) {
                    yield false;
                  }
                }
                return;
              }
            }
          }
          break cutIf1;
        }
        for each (var l4 in symbol_char(C2)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in rest_symbol(NextCh, Chars, LastCh)) {
              yield false;
            }
          }
          return;
        }
      }
    }
  }
  {
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg1, C2)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, C2)) {
          yield false;
        }
      }
    }
  }
}

function symbol_char(arg1) {
  {
    for each (var l2 in YP.unify(arg1, 35)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 36)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 38)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 42)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 43)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 45)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 46)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 47)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 58)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 60)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 61)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 62)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 63)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 64)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 92)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 94)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 96)) {
      yield false;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 126)) {
      yield false;
    }
  }
}

function get_current_position(Pos) {
  {
    for each (var l2 in YP.unify(Pos, 0)) {
      yield false;
    }
  }
}

function read_after_atom4(Ch, Dict, arg3, Chars) {
  {
    var Atom_1 = new Variable();
    var Pos = new Variable();
    var Tokens = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor2("atom", Atom_1, Pos), Tokens))) {
      for each (var l3 in YP.unify(Pos, 0)) {
        for each (var l4 in YP.atom_codes(Atom_1, Chars)) {
          for each (var l5 in read_after_atom(Ch, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_after_atom(arg1, Dict, arg3) {
  {
    var Tokens = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, 40)) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("("), Tokens))) {
        for each (var l4 in YP.get_code(NextCh)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
        return;
      }
    }
  }
  {
    var Ch = arg1;
    var Tokens = arg3;
    for each (var l2 in read_tokens(Ch, Dict, Tokens)) {
      yield false;
    }
  }
}

function read_string(Chars, Quote, NextCh) {
  {
    var Ch = new Variable();
    var Char = new Variable();
    var Next = new Variable();
    for each (var l2 in YP.get_code(Ch)) {
      for each (var l3 in read_char(Ch, Quote, Char, Next)) {
        for each (var l4 in rest_string5(Char, Next, Chars, Quote, NextCh)) {
          yield false;
        }
      }
    }
  }
}

function rest_string5(arg1, arg2, arg3, arg4, arg5) {
  {
    var _X = arg4;
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg1, -1)) {
      for each (var l3 in YP.unify(arg2, NextCh)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          for each (var l5 in YP.unify(arg5, NextCh)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Char = arg1;
    var Next = arg2;
    var Quote = arg4;
    var NextCh = arg5;
    var Chars = new Variable();
    var Char2 = new Variable();
    var Next2 = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(Char, Chars))) {
      for each (var l3 in read_char(Next, Quote, Char2, Next2)) {
        for each (var l4 in rest_string5(Char2, Next2, Chars, Quote, NextCh)) {
          yield false;
        }
      }
    }
  }
}

function escape_char(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, 110)) {
      for each (var l3 in YP.unify(arg2, 10)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 78)) {
      for each (var l3 in YP.unify(arg2, 10)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 116)) {
      for each (var l3 in YP.unify(arg2, 9)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 84)) {
      for each (var l3 in YP.unify(arg2, 9)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 114)) {
      for each (var l3 in YP.unify(arg2, 13)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 82)) {
      for each (var l3 in YP.unify(arg2, 13)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 118)) {
      for each (var l3 in YP.unify(arg2, 11)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 86)) {
      for each (var l3 in YP.unify(arg2, 11)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 98)) {
      for each (var l3 in YP.unify(arg2, 8)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 66)) {
      for each (var l3 in YP.unify(arg2, 8)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 102)) {
      for each (var l3 in YP.unify(arg2, 12)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 70)) {
      for each (var l3 in YP.unify(arg2, 12)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 101)) {
      for each (var l3 in YP.unify(arg2, 27)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 69)) {
      for each (var l3 in YP.unify(arg2, 27)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 100)) {
      for each (var l3 in YP.unify(arg2, 127)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 68)) {
      for each (var l3 in YP.unify(arg2, 127)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 115)) {
      for each (var l3 in YP.unify(arg2, 32)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 83)) {
      for each (var l3 in YP.unify(arg2, 32)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 122)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, 90)) {
      for each (var l3 in YP.unify(arg2, -1)) {
        yield false;
      }
    }
  }
}

function read_variable(C1, Dict, arg3) {
  {
    var Var = new Variable();
    var Name = new Variable();
    var StartPos = new Variable();
    var Tokens = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor3("var", Var, Name, StartPos), Tokens))) {
      for each (var l3 in get_current_position(StartPos)) {
        for each (var l4 in read_name(C1, Chars, NextCh)) {
          for each (var l5 in YP.atom_codes(Name, Chars)) {
            cutIf1:
            {
              if (YP.termEqual(Name, Atom.a("_"))) {
                for each (var l8 in read_after_atom(NextCh, Dict, Tokens)) {
                  yield false;
                }
                break cutIf1;
              }
              for each (var l7 in read_lookup(Dict, Name, Var)) {
                for each (var l8 in read_after_atom(NextCh, Dict, Tokens)) {
                  yield false;
                }
              }
            }
          }
        }
      }
    }
  }
}

function read_lookup(arg1, Name, Var) {
  {
    var N = new Variable();
    var V = new Variable();
    var L = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", N, V), L))) {
      cutIf1:
      {
        for each (var l4 in YP.unify(N, Name)) {
          for each (var l5 in YP.unify(V, Var)) {
            yield false;
          }
          break cutIf1;
        }
        for each (var l4 in read_lookup(L, Name, Var)) {
          yield false;
        }
      }
    }
  }
}

function read_solidus(Ch, LastCh) {
  {
    var NextCh = new Variable();
    cutIf1:
    {
      if (YP.equal(Ch, 42)) {
        for each (var l4 in YP.get_code(NextCh)) {
          cutIf2:
          {
            if (YP.equal(NextCh, 47)) {
              for each (var l7 in YP.get_code(LastCh)) {
                yield false;
              }
              break cutIf2;
            }
            for each (var l6 in read_solidus(NextCh, LastCh)) {
              yield false;
            }
          }
        }
        break cutIf1;
      }
      cutIf3:
      {
        if (YP.notEqual(Ch, -1)) {
          for each (var l5 in YP.get_code(NextCh)) {
            for each (var l6 in read_solidus(NextCh, LastCh)) {
              yield false;
            }
          }
          break cutIf3;
        }
        for each (var l4 in YP.unify(LastCh, Ch)) {
          for each (var l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in /*comment~n"), Atom.NIL)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_identifier(C1, Dict, Tokens) {
  {
    var Chars = new Variable();
    var NextCh = new Variable();
    for each (var l2 in read_name(C1, Chars, NextCh)) {
      for each (var l3 in read_after_atom4(NextCh, Dict, Tokens, Chars)) {
        yield false;
      }
    }
  }
}

function read_name(C1, arg2, LastCh) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(97, Atom.NIL))) {
            cutIf2:
            {
              if (YP.lessThanOrEqual(C2, new ListPair(122, Atom.NIL))) {
                for each (var l8 in read_name(C2, Chars, LastCh)) {
                  yield false;
                }
                break cutIf2;
              }
              cutIf3:
              {
                if (YP.lessThan(C2, 192)) {
                  if (YP.notEqual(YP.bitwiseOr(C2, 16), 186)) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf3;
                  }
                }
                cutIf4:
                {
                  if (YP.equal(YP.bitwiseOr(C2, 32), 247)) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf4;
                  }
                  for each (var l9 in read_name(C2, Chars, LastCh)) {
                    yield false;
                  }
                }
              }
            }
            break cutIf1;
          }
          cutIf5:
          {
            if (YP.greaterThanOrEqual(C2, new ListPair(65, Atom.NIL))) {
              cutIf6:
              {
                if (YP.greaterThan(C2, new ListPair(90, Atom.NIL))) {
                  if (YP.notEqual(C2, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in YP.unify(Chars, Atom.NIL)) {
                      for each (var l11 in YP.unify(LastCh, C2)) {
                        yield false;
                      }
                    }
                    break cutIf6;
                  }
                }
                for each (var l8 in read_name(C2, Chars, LastCh)) {
                  yield false;
                }
              }
              break cutIf5;
            }
            cutIf7:
            {
              if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
                if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
                  for each (var l9 in read_name(C2, Chars, LastCh)) {
                    yield false;
                  }
                  break cutIf7;
                }
              }
              for each (var l7 in YP.unify(Chars, Atom.NIL)) {
                for each (var l8 in YP.unify(LastCh, C2)) {
                  yield false;
                }
              }
            }
          }
        }
      }
    }
  }
}

function read_fullstop(Ch, Dict, Tokens) {
  {
    var Number = new Variable();
    var Tokens1 = new Variable();
    var Chars = new Variable();
    var NextCh = new Variable();
    cutIf1:
    {
      if (YP.lessThanOrEqual(Ch, new ListPair(57, Atom.NIL))) {
        if (YP.greaterThanOrEqual(Ch, new ListPair(48, Atom.NIL))) {
          for each (var l5 in YP.unify(Tokens, new ListPair(new Functor1("number", Number), Tokens1))) {
            for each (var l6 in read_float(Number, Dict, Tokens1, new ListPair(48, Atom.NIL), Ch)) {
              yield false;
            }
          }
          break cutIf1;
        }
      }
      cutIf2:
      {
        if (YP.greaterThan(Ch, new ListPair(32, Atom.NIL))) {
          for each (var l5 in rest_symbol(Ch, Chars, NextCh)) {
            for each (var l6 in read_after_atom4(NextCh, Dict, Tokens, new ListPair(46, Chars))) {
              yield false;
            }
          }
          break cutIf2;
        }
        cutIf3:
        {
          if (YP.greaterThanOrEqual(Ch, 0)) {
            for each (var l6 in YP.unify(Tokens, Atom.NIL)) {
              yield false;
            }
            break cutIf3;
          }
          for each (var l5 in formatError(Atom.a("user_error"), Atom.a("~N** end of file just after full stop~n"), Atom.NIL)) {
          }
        }
      }
    }
  }
}

function read_float(Number, Dict, Tokens, Digits, Digit) {
  {
    var Chars = new Variable();
    var Rest = new Variable();
    var NextCh = new Variable();
    for each (var l2 in prepend(Digits, Chars, Rest)) {
      for each (var l3 in read_float4(Digit, Rest, NextCh, Chars)) {
        for each (var l4 in YP.number_codes(Number, Chars)) {
          for each (var l5 in read_tokens(NextCh, Dict, Tokens)) {
            yield false;
          }
        }
      }
    }
  }
}

function prepend(arg1, arg2, arg3) {
  {
    var X = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, new ListPair(46, X))) {
        yield false;
      }
    }
  }
  {
    var Y = arg3;
    var C = new Variable();
    var Cs = new Variable();
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(C, Cs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(C, X))) {
        for each (var l4 in prepend(Cs, X, Y)) {
          yield false;
        }
      }
    }
  }
}

function read_float4(C1, arg2, NextCh, Total) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    var C3 = new Variable();
    var C4 = new Variable();
    var More = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
            if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
              for each (var l7 in read_float4(C2, Chars, NextCh, Total)) {
                yield false;
              }
              break cutIf1;
            }
          }
          cutIf2:
          {
            if (YP.equal(YP.bitwiseOr(C2, 32), new ListPair(101, Atom.NIL))) {
              for each (var l7 in YP.get_code(C3)) {
                cutIf3:
                {
                  if (YP.equal(C3, new ListPair(45, Atom.NIL))) {
                    for each (var l10 in YP.get_code(C4)) {
                      for each (var l11 in YP.unify(Chars, new ListPair(C2, new ListPair(45, More)))) {
                        cutIf4:
                        {
                          if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                            if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                              for each (var l15 in read_exponent(C4, More, NextCh)) {
                                yield false;
                              }
                              break cutIf4;
                            }
                          }
                          for each (var l13 in YP.unify(More, Atom.NIL)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                            }
                          }
                          for each (var l13 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                            for each (var l14 in YP.unify(NextCh, C4)) {
                              yield false;
                            }
                          }
                        }
                      }
                    }
                    break cutIf3;
                  }
                  cutIf5:
                  {
                    if (YP.equal(C3, new ListPair(43, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C4)) {
                        for each (var l12 in YP.unify(Chars, new ListPair(C2, More))) {
                          cutIf6:
                          {
                            if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                              if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                                for each (var l16 in read_exponent(C4, More, NextCh)) {
                                  yield false;
                                }
                                break cutIf6;
                              }
                            }
                            for each (var l14 in YP.unify(More, Atom.NIL)) {
                              for each (var l15 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                              }
                            }
                            for each (var l14 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                              for each (var l15 in YP.unify(NextCh, C4)) {
                                yield false;
                              }
                            }
                          }
                        }
                      }
                      break cutIf5;
                    }
                    for each (var l10 in YP.unify(C4, C3)) {
                      for each (var l11 in YP.unify(Chars, new ListPair(C2, More))) {
                        cutIf7:
                        {
                          if (YP.greaterThanOrEqual(C4, new ListPair(48, Atom.NIL))) {
                            if (YP.lessThanOrEqual(C4, new ListPair(57, Atom.NIL))) {
                              for each (var l15 in read_exponent(C4, More, NextCh)) {
                                yield false;
                              }
                              break cutIf7;
                            }
                          }
                          for each (var l13 in YP.unify(More, Atom.NIL)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** Missing exponent in ~s~n"), new ListPair(Total, Atom.NIL))) {
                            }
                          }
                          for each (var l13 in YP.unify(More, new ListPair(48, Atom.NIL))) {
                            for each (var l14 in YP.unify(NextCh, C4)) {
                              yield false;
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
              break cutIf2;
            }
            for each (var l6 in YP.unify(Chars, Atom.NIL)) {
              for each (var l7 in YP.unify(NextCh, C2)) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
}

function read_exponent(C1, arg2, NextCh) {
  {
    var Chars = new Variable();
    var C2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(C1, Chars))) {
      for each (var l3 in YP.get_code(C2)) {
        cutIf1:
        {
          if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
            if (YP.lessThanOrEqual(C2, new ListPair(57, Atom.NIL))) {
              for each (var l7 in read_exponent(C2, Chars, NextCh)) {
                yield false;
              }
              break cutIf1;
            }
          }
          for each (var l5 in YP.unify(Chars, Atom.NIL)) {
            for each (var l6 in YP.unify(NextCh, C2)) {
              yield false;
            }
          }
        }
      }
    }
  }
}

function read_number(C1, Dict, arg3) {
  {
    var Number = new Variable();
    var Tokens = new Variable();
    var C2 = new Variable();
    var N = new Variable();
    var C = new Variable();
    var C3 = new Variable();
    var Digits = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor1("number", Number), Tokens))) {
      for each (var l3 in read_number4(C1, C2, 0, N)) {
        cutIf1:
        {
          if (YP.equal(C2, 39)) {
            cutIf2:
            {
              if (YP.greaterThanOrEqual(N, 2)) {
                if (YP.lessThanOrEqual(N, 36)) {
                  for each (var l9 in read_based(N, 0, Number, C)) {
                    for each (var l10 in read_tokens(C, Dict, Tokens)) {
                      yield false;
                    }
                  }
                  break cutIf2;
                }
              }
              cutIf3:
              {
                if (YP.equal(N, 0)) {
                  for each (var l9 in YP.get_code(C3)) {
                    for each (var l10 in read_char(C3, -1, Number, C)) {
                      for each (var l11 in read_tokens(C, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                  break cutIf3;
                }
                for each (var l8 in formatError(Atom.a("user_error"), Atom.a("~N** ~d' read as ~d '~n"), new ListPair(N, new ListPair(N, Atom.NIL)))) {
                  for each (var l9 in YP.unify(Number, N)) {
                    for each (var l10 in YP.unify(C, C2)) {
                      for each (var l11 in read_tokens(C, Dict, Tokens)) {
                        yield false;
                      }
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
          cutIf4:
          {
            if (YP.equal(C2, 46)) {
              for each (var l7 in YP.get_code(C3)) {
                cutIf5:
                {
                  if (YP.greaterThanOrEqual(C3, new ListPair(48, Atom.NIL))) {
                    if (YP.lessThanOrEqual(C3, new ListPair(57, Atom.NIL))) {
                      for each (var l11 in YP.number_codes(N, Digits)) {
                        for each (var l12 in read_float(Number, Dict, Tokens, Digits, C3)) {
                          yield false;
                        }
                      }
                      break cutIf5;
                    }
                  }
                  for each (var l9 in YP.unify(Number, N)) {
                    for each (var l10 in read_fullstop(C3, Dict, Tokens)) {
                      yield false;
                    }
                  }
                }
              }
              break cutIf4;
            }
            for each (var l6 in YP.unify(Number, N)) {
              for each (var l7 in read_tokens(C2, Dict, Tokens)) {
                yield false;
              }
            }
          }
        }
      }
    }
  }
}

function read_number4(C0, C, N0, N) {
  {
    var N1 = new Variable();
    var C1 = new Variable();
    cutIf1:
    {
      if (YP.greaterThanOrEqual(C0, new ListPair(48, Atom.NIL))) {
        if (YP.lessThanOrEqual(C0, new ListPair(57, Atom.NIL))) {
          for each (var l5 in YP.unify(N1, YP.add(YP.subtract(YP.multiply(N0, 10), new ListPair(48, Atom.NIL)), C0))) {
            for each (var l6 in YP.get_code(C1)) {
              for each (var l7 in read_number4(C1, C, N1, N)) {
                yield false;
              }
            }
          }
          break cutIf1;
        }
      }
      cutIf2:
      {
        if (YP.equal(C0, 95)) {
          for each (var l5 in YP.get_code(C1)) {
            for each (var l6 in read_number4(C1, C, N0, N)) {
              yield false;
            }
          }
          break cutIf2;
        }
        for each (var l4 in YP.unify(C, C0)) {
          for each (var l5 in YP.unify(N, N0)) {
            yield false;
          }
        }
      }
    }
  }
}

function read_based(Base, N0, N, C) {
  {
    var C1 = new Variable();
    var Digit = new Variable();
    var N1 = new Variable();
    for each (var l2 in YP.get_code(C1)) {
      cutIf1:
      {
        if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
          if (YP.lessThanOrEqual(C1, new ListPair(57, Atom.NIL))) {
            for each (var l6 in YP.unify(Digit, YP.subtract(C1, new ListPair(48, Atom.NIL)))) {
              cutIf2:
              {
                if (YP.lessThan(Digit, Base)) {
                  for each (var l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                    for each (var l10 in read_based(Base, N1, N, C)) {
                      yield false;
                    }
                  }
                  break cutIf2;
                }
                cutIf3:
                {
                  if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in read_based(Base, N0, N, C)) {
                      yield false;
                    }
                    break cutIf3;
                  }
                  for each (var l9 in YP.unify(N, N0)) {
                    for each (var l10 in YP.unify(C, C1)) {
                      yield false;
                    }
                  }
                }
              }
            }
            break cutIf1;
          }
        }
        cutIf4:
        {
          if (YP.greaterThanOrEqual(C1, new ListPair(65, Atom.NIL))) {
            if (YP.lessThanOrEqual(C1, new ListPair(90, Atom.NIL))) {
              for each (var l7 in YP.unify(Digit, YP.subtract(C1, YP.subtract(new ListPair(65, Atom.NIL), 10)))) {
                cutIf5:
                {
                  if (YP.lessThan(Digit, Base)) {
                    for each (var l10 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                      for each (var l11 in read_based(Base, N1, N, C)) {
                        yield false;
                      }
                    }
                    break cutIf5;
                  }
                  cutIf6:
                  {
                    if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                      for each (var l11 in read_based(Base, N0, N, C)) {
                        yield false;
                      }
                      break cutIf6;
                    }
                    for each (var l10 in YP.unify(N, N0)) {
                      for each (var l11 in YP.unify(C, C1)) {
                        yield false;
                      }
                    }
                  }
                }
              }
              break cutIf4;
            }
          }
          cutIf7:
          {
            if (YP.greaterThanOrEqual(C1, new ListPair(97, Atom.NIL))) {
              if (YP.lessThanOrEqual(C1, new ListPair(122, Atom.NIL))) {
                for each (var l8 in YP.unify(Digit, YP.subtract(C1, YP.subtract(new ListPair(97, Atom.NIL), 10)))) {
                  cutIf8:
                  {
                    if (YP.lessThan(Digit, Base)) {
                      for each (var l11 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                        for each (var l12 in read_based(Base, N1, N, C)) {
                          yield false;
                        }
                      }
                      break cutIf8;
                    }
                    cutIf9:
                    {
                      if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                        for each (var l12 in read_based(Base, N0, N, C)) {
                          yield false;
                        }
                        break cutIf9;
                      }
                      for each (var l11 in YP.unify(N, N0)) {
                        for each (var l12 in YP.unify(C, C1)) {
                          yield false;
                        }
                      }
                    }
                  }
                }
                break cutIf7;
              }
            }
            for each (var l6 in YP.unify(Digit, 99)) {
              cutIf10:
              {
                if (YP.lessThan(Digit, Base)) {
                  for each (var l9 in YP.unify(N1, YP.add(YP.multiply(N0, Base), Digit))) {
                    for each (var l10 in read_based(Base, N1, N, C)) {
                      yield false;
                    }
                  }
                  break cutIf10;
                }
                cutIf11:
                {
                  if (YP.equal(C1, new ListPair(95, Atom.NIL))) {
                    for each (var l10 in read_based(Base, N0, N, C)) {
                      yield false;
                    }
                    break cutIf11;
                  }
                  for each (var l9 in YP.unify(N, N0)) {
                    for each (var l10 in YP.unify(C, C1)) {
                      yield false;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function read_char(Char, Quote, Result, Next) {
  {
    var C1 = new Variable();
    var C2 = new Variable();
    var C3 = new Variable();
    var Ch = new Variable();
    cutIf1:
    {
      if (YP.equal(Char, 92)) {
        for each (var l4 in YP.get_code(C1)) {
          cutIf2:
          {
            if (YP.lessThan(C1, 0)) {
              for each (var l7 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~cquoted~c~n"), new ListPair(Quote, new ListPair(Quote, Atom.NIL)))) {
                for each (var l8 in YP.unify(Result, -1)) {
                  for each (var l9 in YP.unify(Next, C1)) {
                    yield false;
                  }
                }
              }
              break cutIf2;
            }
            cutIf3:
            {
              if (YP.lessThanOrEqual(C1, new ListPair(32, Atom.NIL))) {
                for each (var l8 in YP.get_code(C2)) {
                  for each (var l9 in read_char(C2, Quote, Result, Next)) {
                    yield false;
                  }
                }
                break cutIf3;
              }
              cutIf4:
              {
                if (YP.equal(YP.bitwiseOr(C1, 32), new ListPair(99, Atom.NIL))) {
                  for each (var l9 in YP.get_code(C2)) {
                    for each (var l10 in read_char(C2, Quote, Result, Next)) {
                      yield false;
                    }
                  }
                  break cutIf4;
                }
                cutIf5:
                {
                  if (YP.lessThanOrEqual(C1, new ListPair(55, Atom.NIL))) {
                    if (YP.greaterThanOrEqual(C1, new ListPair(48, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C2)) {
                        cutIf6:
                        {
                          if (YP.lessThanOrEqual(C2, new ListPair(55, Atom.NIL))) {
                            if (YP.greaterThanOrEqual(C2, new ListPair(48, Atom.NIL))) {
                              for each (var l15 in YP.get_code(C3)) {
                                cutIf7:
                                {
                                  if (YP.lessThanOrEqual(C3, new ListPair(55, Atom.NIL))) {
                                    if (YP.greaterThanOrEqual(C3, new ListPair(48, Atom.NIL))) {
                                      for each (var l19 in YP.get_code(Next)) {
                                        for each (var l20 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(YP.add(YP.multiply(C1, 8), C2), 8), C3), YP.multiply(73, new ListPair(48, Atom.NIL))))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf7;
                                    }
                                  }
                                  for each (var l17 in YP.unify(Next, C3)) {
                                    for each (var l18 in YP.unify(Result, YP.subtract(YP.add(YP.multiply(C1, 8), C2), YP.multiply(9, new ListPair(48, Atom.NIL))))) {
                                      yield false;
                                    }
                                  }
                                }
                              }
                              break cutIf6;
                            }
                          }
                          for each (var l13 in YP.unify(Next, C2)) {
                            for each (var l14 in YP.unify(Result, YP.subtract(C1, new ListPair(48, Atom.NIL)))) {
                              yield false;
                            }
                          }
                        }
                      }
                      break cutIf5;
                    }
                  }
                  cutIf8:
                  {
                    if (YP.equal(C1, new ListPair(94, Atom.NIL))) {
                      for each (var l11 in YP.get_code(C2)) {
                        cutIf9:
                        {
                          if (YP.lessThan(C2, 0)) {
                            for each (var l14 in formatError(Atom.a("user_error"), Atom.a("~N** end of file in ~c..~c^..~c~n"), ListPair.make([Quote, 92, Quote]))) {
                              for each (var l15 in YP.unify(Result, -1)) {
                                for each (var l16 in YP.unify(Next, C2)) {
                                  yield false;
                                }
                              }
                            }
                            break cutIf9;
                          }
                          cutIf10:
                          {
                            if (YP.equal(C2, new ListPair(63, Atom.NIL))) {
                              for each (var l15 in YP.unify(Result, 127)) {
                                for each (var l16 in YP.get_code(Next)) {
                                  yield false;
                                }
                              }
                              break cutIf10;
                            }
                            for each (var l14 in YP.unify(Result, YP.bitwiseAnd(C2, 31))) {
                              for each (var l15 in YP.get_code(Next)) {
                                yield false;
                              }
                            }
                          }
                        }
                      }
                      break cutIf8;
                    }
                    cutIf11:
                    {
                      for each (var l11 in escape_char(C1, Result)) {
                        for each (var l12 in YP.get_code(Next)) {
                          yield false;
                        }
                        break cutIf11;
                      }
                      for each (var l11 in YP.unify(Result, C1)) {
                        for each (var l12 in YP.get_code(Next)) {
                          yield false;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        break cutIf1;
      }
      cutIf12:
      {
        if (YP.equal(Char, Quote)) {
          for each (var l5 in YP.get_code(Ch)) {
            cutIf13:
            {
              if (YP.equal(Ch, Quote)) {
                for each (var l8 in YP.unify(Result, Quote)) {
                  for each (var l9 in YP.get_code(Next)) {
                    yield false;
                  }
                }
                break cutIf13;
              }
              for each (var l7 in YP.unify(Result, -1)) {
                for each (var l8 in YP.unify(Next, Ch)) {
                  yield false;
                }
              }
            }
          }
          break cutIf12;
        }
        cutIf14:
        {
          if (YP.lessThan(Char, new ListPair(32, Atom.NIL))) {
            if (YP.notEqual(Char, 9)) {
              if (YP.notEqual(Char, 10)) {
                if (YP.notEqual(Char, 13)) {
                  for each (var l9 in YP.unify(Result, -1)) {
                    for each (var l10 in YP.unify(Next, Char)) {
                      for each (var l11 in formatError(Atom.a("user_error"), Atom.a("~N** Strange character ~d ends ~ctoken~c~n"), ListPair.make([Char, Quote, Quote]))) {
                        yield false;
                      }
                    }
                  }
                  break cutIf14;
                }
              }
            }
          }
          for each (var l5 in YP.unify(Result, Char)) {
            for each (var l6 in YP.get_code(Next)) {
              yield false;
            }
          }
        }
      }
    }
  }
}


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

function CompilerState() {
	this._pred = new IndexedAnswers(4);
	this._moduleForNameArity = new Object();
}

// Make these static functions that explicitly take the State so Prolog can call it.

// Make a new CompilerState and bind it to State.
CompilerState.make = function(State) {
    return YP.unify(State, new CompilerState());
}

CompilerState.assertPred = function(State, Pred, Determinism) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Pred);
    var functorArgs = YP.getFunctorArgs(Pred);
    // Debug: Should check if it's already asserted and is the same.
    State._pred.addAnswer([functorName, functorArgs.length, Pred, YP.getValue(Determinism)]);
}

CompilerState.assertModuleForNameArity = function(State, Name, Arity, Module) {
    State = YP.getValue(State);
    Name = YP.getValue(Name);
    Arity = YP.getValue(Arity);
    Module = YP.getValue(Module);
    // If the Module Atom comes from the parser, it always has null _declaringClass.
    if (Module instanceof Atom && Module._module == null && Name instanceof Atom && typeof(Arity) == "number")
        // Replace a previous entry if it exists.
        State._moduleForNameArity[Name + "/" + Arity] = Module;
}

CompilerState.startFunction = function(State, Head) {
    State = YP.getValue(State);
    State._gensymCounter = 0;
    State._useFinalCutCode = false;
    State._finalCutCode = new Variable();
    State._codeUsesYield = false;
    if (CompilerState.isDetNoneOut(State, Head))
        State._determinism = Atom.a("detNoneOut");
    else if (CompilerState.isSemidetNoneOut(State, Head))
        State._determinism = Atom.a("semidetNoneOut");
    else
        State._determinism = Atom.a("nondet");
}

CompilerState.setCodeUsesYield = function(State) {
    State = YP.getValue(State);
    State._codeUsesYield = true;
}

CompilerState.codeUsesYield = function(State) {
    State = YP.getValue(State);
    return State._codeUsesYield;
}

CompilerState.determinismEquals = function(State, Term) {
    State = YP.getValue(State);
    return YP.termEqual(State._determinism, Term);
}

// Set _variableNames to a new list of (Name = Variable) for each unique variable in rule.
// If the variable is in variableNameSuggestions, use it, otherwise use x1, x2, etc.
CompilerState.newVariableNames = function(State, Rule, VariableNameSuggestions) {
    State = YP.getValue(State);
    var variablesSet = [];
    YP.addUniqueVariables(Rule, variablesSet);

    State._variableNames = [];
    var xCounter = 0;
    for each (var variable in variablesSet)
        State._variableNames.push
            (new Functor2(Atom.a("="), CompilerState.makeVariableName(variable, VariableNameSuggestions, ++xCounter), 
			 variable));
}

CompilerState.makeVariableName = function(variable, variableNameSuggestions, xCounter) {
    // Debug: should require named variables to start with _ or capital. Should
    //   check for duplicates and clashes with keywords.
    for (var element = YP.getValue(variableNameSuggestions);
         element instanceof Functor2 && element._name == Atom.DOT;
         element = YP.getValue(element._arg2)) {
        var suggestionPair = YP.getValue(element._arg1);
        if (sameVariable(variable, suggestionPair._arg2)) {
            var suggestion = YP.getValue(suggestionPair._arg1);
            if (suggestion == Atom.a("Atom"))
                suggestion = Atom.a("Atom_1");
            if (suggestion == Atom.a("Variable"))
                suggestion = Atom.a("Variable_1");
            if (suggestion == Atom.a("Functor"))
                suggestion = Atom.a("Functor_1");
            return suggestion;
        }
    }

    return Atom.a("x" + xCounter);
}

// Unify Result with the name assigned by CompilerState.newVariableNames in State._variableNames
//   for variable.
CompilerState.getVariableName = function(State, variable, Result) {
    State = YP.getValue(State);
    for each (var variableInfo in State._variableNames) {
        if (variableInfo instanceof Functor2 && variableInfo._name == Atom.a("=")) {
            if (sameVariable(variable, variableInfo._arg2))
                return YP.unify(Result, variableInfo._arg1);
        }
    }

    // We set up names for all unique variables, so this should never happen.
    throw new PrologException(Atom.a("Can't find entry in _variableNames"));
}

CompilerState.variableNamesList = function(State, VariableNamesList) {
    State = YP.getValue(State);
    return YP.unify(VariableNamesList, ListPair.make(State._variableNames));
}

CompilerState.gensym = function(State, Base, Symbol) {
    State = YP.getValue(State);
    return YP.unify(Symbol, Atom.a(Base.toString() + ++State._gensymCounter));
}

CompilerState.isDetNoneOut = function(State, Term) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Term);
    var functorArgs = YP.getFunctorArgs(Term);

    var pred = new Variable();
    for each (var l1 in State._pred.match([functorName, functorArgs.length, pred, Atom.a("det")])) {
        if (CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue()))) {
            return true;
        }
    }

    return false;
}

CompilerState.isSemidetNoneOut = function(State, Term) {
    State = YP.getValue(State);
    var functorName = YP.getFunctorName(Term);
    var functorArgs = YP.getFunctorArgs(Term);

    var pred = new Variable();
    for each (var l1 in State._pred.match([functorName, functorArgs.length, pred, Atom.a("semidet")])) {
        if (CompilerState.isNoneOut(YP.getFunctorArgs(pred.getValue()))) {
            return true;
        }
    }

    return false;
}

// Return false if any of args is out, otherwise true.
// args is an array of ::(Type,Mode) where Mode is in or out.
CompilerState.isNoneOut = function(args) {
    for each (var arg in args) {
        if (arg instanceof Functor2 && arg._name == Atom.a("::") && arg._arg2 == Atom.a("out"))
            return false;
    }
    return true;
}

CompilerState.nameArityHasModule = function(State, Name, Arity, Module) {
    State = YP.getValue(State);
    Name = YP.getValue(Name);
    Arity = YP.getValue(Arity);
    Module = YP.getValue(Module);
    if (Name instanceof Atom && typeof(Arity) == "number") {
        var FoundModule = State._moduleForNameArity[Name + "/" + Arity];
        if (!FoundModule === undefined)
            return false;
        return FoundModule == Module;
    }
    return false;
}

function Compiler() {
}

// Use makeFunctionPseudoCode, convertFunctionJavascript and compileAnonymousFunction
// to return an anonymous YP.IClause for the Head and Body of a rule clause.
// Head is a prolog term such as new Functor2("test1", X, Y).
// Note that the name of the head is ignored.
// Body is a prolog term such as 
// new Functor2(",", new Functor1(Atom.a("test2", Atom.a("")), X), 
//              new Functor2("=", Y, X)).
// This may not be null.  (For a head-only clause, set the Body to Atom.a("true"). 
// (This has no declaringClass because it is assumed that predicates with default module Atom.a("")
//  are in the global scope.)
// Returns a new object on which you can call match(args) where
// args length is the arity of the Head.
Compiler.compileAnonymousClause = function(Head, Body) {
    var args = YP.getFunctorArgs(Head);
    // compileAnonymousFunction wants "function".
    var Rule = new Functor2(Atom.RULE, Functor.make("function", args), Body);
    var RuleList = ListPair.make(new Functor2(Atom.F, Rule, Atom.NIL));

    var functionCode = new YP.StringWriter();
    var SaveOutputStream = new Variable();
    for each (var l1 in YP.current_output(SaveOutputStream)) {
        try {
            YP.tell(functionCode);
            var PseudoCode = new Variable();
            for each (var l2 in makeFunctionPseudoCode(RuleList, PseudoCode)) {
                if (YP.termEqual(PseudoCode, Atom.a("getDeclaringClass")))
                    // Ignore getDeclaringClass since we have access to the one passed in.
                    continue;

                convertFunctionJavascript(PseudoCode);
            }
            YP.told();
        }
        finally {
            // Restore after calling tell.
            YP.tell(SaveOutputStream.getValue());
        }
    }

    return Compiler.compileAnonymousFunction(functionCode.toString(), args.length);
}
           
// Use eval to compile the functionCode and return a YP.ClauseHeadAndBody object which implements
//   match(args) which is called with an array of the arguments to match the clause.
// functionCode is the code for the iterator, such as
// "function() { yield false; }"
// nArgs is the number of args in the function.
Compiler.compileAnonymousFunction = function(functionCode, nArgs) {
    var matchCode = new YP.StringWriter();
    matchCode.write("(function(args) { return this._function("); 
    if (nArgs >= 1)
        matchCode.write("args[0]");
    for (var i = 1; i < nArgs; ++i)
        matchCode.write(", args[" + i + "]");
    matchCode.write("); })");
  
    var clause = new YP.ClauseHeadAndBody();
    // Put inside parentheses to make a syntactically valid expression.
    clause._function = eval("(" + functionCode + ")");
    clause.match = eval(matchCode.toString());
    return clause; 
}

// If the functor with name and args can be called directly as determined by
//   functorCallFunctionName, then call it and return its iterator.  If the predicate is
//   dynamic and undefined, or if static and the method cannot be found, return
//   the result of YP.unknownPredicate.
// declaringClass is used to resolve references to the default 
// module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
//   null, this looks in the global scope.  If not found, this throws a 
//   PrologException for existence_error.
// This returns null if the functor has a special form than needs to be compiled 
//   (including ,/2 and ;/2).
Compiler.getSimpleIterator = function(name, args, declaringClass) {
    var state = new CompilerState();
    var FunctionName = new Variable();
    for each (var l1 in functorCallFunctionName(state, name, args.length, FunctionName)) {
        var functionNameAtom = FunctionName.getValue();
        if (functionNameAtom == Atom.NIL)
            // name is for a dynamic predicate.
            return YP.matchDynamic(name, args);

        var methodName = functionNameAtom._name;
        // Set the default for the method to call.
        var methodClass = declaringClass;

        var checkMode = false;
        if (methodName.substr(0,2) == "YP.") {
            // Assume we only check mode in calls to standard Prolog predicates in YP.
            checkMode = true;

            // Use the method in class YP.
            methodName = methodName.substr(3);
            methodClass = YP;
        }
        if (methodName.indexOf(".") >= 0)
            // We don't support calling inner classes, etc.
            return null;

        var func = null;
	    try {
            if (methodClass == null)
                // Look in the global scope.
	            func = eval(methodName);
	        else
	            func = methodClass[methodName];
	             
	        if (func === undefined || typeof(func) != "function")
	            func = null;
	    } catch (e) {
	        func = null;
        }

        if (func == null)
            throw new PrologException
                (new Functor2
                 (Atom.a("existence_error"), Atom.a("procedure"),
                  new Functor2(Atom.a("/"), name, args.length)),
                 "Cannot find predicate function " + methodName + " for " + name + "/" + args.length + 
                 " in " + (methodClass == null ? "the global scope" : methodClass.toString()));
        
        if (checkMode) {
            assertYPPred(state);
            var functor = Functor.make(name, args);
            if (CompilerState.isDetNoneOut(state, functor)) {
                func.apply(null, args);
                return YP.succeed();
            }
            if (CompilerState.isSemidetNoneOut(state, functor)) {
                if (func.apply(null, args))
                    return YP.succeed();
                else
                    return YP.fail();
            }
        }
        return Iterator(func.apply(null, args));
    }

    return null;
}

// Return true if there is a dynamic or static predicate with name and arity.
// This returns false for built-in predicates.
// declaringClass: used to resolve references to the default 
// module Atom.a(""). If a declaringClass is needed to resolve the reference but it is
//   null, return false.
Compiler.isCurrentPredicate = function(name, arity, declaringClass) {
    var state = new CompilerState();
    var FunctionName = new Variable();
    for each (var l1 in functorCallFunctionName(state, name, arity, FunctionName)) {
        var functionNameAtom = FunctionName.getValue();
        if (functionNameAtom == Atom.NIL)
            // name is for a dynamic predicate.
            return YP.isDynamicCurrentPredicate(name, arity);

        var methodName = functionNameAtom._name;

        if (methodName.substr(0,2) == "YP.")
            // current_predicate/1 should fail for built-ins.
            return false;
        if (methodName.indexOf(".") >= 0)
            // We don't support calling inner classes, etc.
            return false;
            
        try {
            // Look in the global scope.
	  	    var func = eval(methodName);
	  	    // Note that Javascript doesn't let us check the arity, but make sure it's a function.
	  	    return typeof(func) == "function";
	  	} catch (e) {
	  	    // eval didn't find it.
	  	    return false;
	  	}
    }

    return false;
}

// Compiler output follows.

function getDeclaringClass() { return null; }

function repeatWrite(arg1, N) {
  {
    var _Value = arg1;
    if (YP.termEqual(N, 0)) {
      return;
    }
  }
  {
    var Value = arg1;
    var NextN = new Variable();
    YP.write(Value);
    for each (var l2 in YP.unify(NextN, YP.subtract(N, 1))) {
      repeatWrite(Value, NextN);
      return;
    }
  }
}

function sameVariable(Variable1, Variable2) {
  {
    if (YP.var(Variable1)) {
      if (YP.var(Variable2)) {
        if (YP.termEqual(Variable1, Variable2)) {
          return true;
        }
      }
    }
  }
  return false;
}

function makeFunctionPseudoCode(RuleList, FunctionCode) {
  {
    var State = new Variable();
    for each (var l2 in CompilerState.make(State)) {
      assertYPPred(State);
      processCompilerDirectives(RuleList, State);
      for each (var l3 in YP.unify(FunctionCode, Atom.a("getDeclaringClass"))) {
        yield false;
      }
      for each (var l3 in makeFunctionPseudoCode3(RuleList, State, FunctionCode)) {
        yield false;
      }
    }
  }
}

function assertYPPred(State) {
  {
    CompilerState.assertPred(State, Atom.a("nl"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("write", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("put_code", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("see", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, Atom.a("seen"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("tell", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, Atom.a("told"), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("throw", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("abolish", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("retractall", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor2("set_prolog_flag", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("det"));
    CompilerState.assertPred(State, new Functor1("var", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("nonvar", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("atom", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("integer", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("float", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("number", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("atomic", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("compound", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor1("ground", new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("==", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("\\==", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@<", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@=<", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@>", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    CompilerState.assertPred(State, new Functor2("@>=", new Functor2("::", Atom.a("univ"), Atom.a("in")), new Functor2("::", Atom.a("univ"), Atom.a("in"))), Atom.a("semidet"));
    return;
  }
}

function processCompilerDirectives(arg1, arg2) {
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var State = arg2;
    var Pred = new Variable();
    var Determinism = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", new Functor1("pred", new Functor2("is", Pred, Determinism))), x3), RestRules))) {
      CompilerState.assertPred(State, Pred, Determinism);
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
  {
    var State = arg2;
    var Module = new Variable();
    var PredicateList = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", new Functor2("import", Module, PredicateList)), x3), RestRules))) {
      for each (var l3 in importPredicateList(State, Module, PredicateList)) {
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var x1 = new Variable();
    var x2 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor1(":-", x1), x2), RestRules))) {
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
  {
    var State = arg2;
    var Head = new Variable();
    var _Body = new Variable();
    var x3 = new Variable();
    var RestRules = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", new Functor2(":-", Head, _Body), x3), RestRules))) {
      for each (var l3 in YP.functor(Head, Name, Arity)) {
        CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""));
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var Fact = new Variable();
    var x2 = new Variable();
    var RestRules = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", Fact, x2), RestRules))) {
      for each (var l3 in YP.functor(Fact, Name, Arity)) {
        CompilerState.assertModuleForNameArity(State, Name, Arity, Atom.a(""));
        processCompilerDirectives(RestRules, State);
        return;
      }
    }
  }
  {
    var State = arg2;
    var x1 = new Variable();
    var RestRules = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, RestRules))) {
      processCompilerDirectives(RestRules, State);
      return;
    }
  }
}

function importPredicateList(arg1, arg2, arg3) {
  {
    var _State = arg1;
    var _Module = arg2;
    for each (var l2 in YP.unify(arg3, Atom.NIL)) {
      yield true;
      return;
    }
  }
  {
    var State = arg1;
    var Module = arg2;
    var Name = new Variable();
    var Arity = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(new Functor2("/", Name, Arity), Rest))) {
      CompilerState.assertModuleForNameArity(State, Name, Arity, Module);
      for each (var l3 in importPredicateList(State, Module, Rest)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg1;
    var Module = arg2;
    var x3 = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg3, new ListPair(x3, Rest))) {
      for each (var l3 in importPredicateList(State, Module, Rest)) {
        yield true;
        return;
      }
    }
  }
}

function makeFunctionPseudoCode3(RuleList, State, FunctionCode) {
  {
    var SamePredicateRuleList = new Variable();
    var RestRules = new Variable();
    for each (var l2 in samePredicateRuleList(RuleList, SamePredicateRuleList, RestRules)) {
      if (YP.termNotEqual(SamePredicateRuleList, Atom.NIL)) {
        for each (var l4 in compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode)) {
          yield false;
        }
        for each (var l4 in makeFunctionPseudoCode3(RestRules, State, FunctionCode)) {
          yield false;
        }
      }
    }
  }
}

function compileSamePredicateFunction(SamePredicateRuleList, State, FunctionCode) {
  {
    var FirstRule = new Variable();
    var x5 = new Variable();
    var x6 = new Variable();
    var x7 = new Variable();
    var Head = new Variable();
    var x9 = new Variable();
    var ArgAssignments = new Variable();
    var Calls = new Variable();
    var Rule = new Variable();
    var VariableNameSuggestions = new Variable();
    var ClauseBag = new Variable();
    var Name = new Variable();
    var ArgsList = new Variable();
    var FunctionArgNames = new Variable();
    var MergedArgName = new Variable();
    var ArgName = new Variable();
    var MergedArgNames = new Variable();
    var FunctionArgs = new Variable();
    var BodyCode = new Variable();
    var ReturnType = new Variable();
    var BodyWithReturn = new Variable();
    for each (var l2 in YP.unify(new ListPair(new Functor2("f", FirstRule, x5), x6), SamePredicateRuleList)) {
      cutIf1:
      {
        for each (var l4 in YP.unify(FirstRule, new Functor1(":-", x7))) {
          break cutIf1;
        }
        cutIf2:
        {
          for each (var l5 in YP.unify(new Functor2(":-", Head, x9), FirstRule)) {
            CompilerState.startFunction(State, Head);
            var findallAnswers3 = new FindallAnswers(new Functor2("f", ArgAssignments, Calls));
            for each (var l6 in member(new Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList)) {
              for each (var l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls)) {
                findallAnswers3.add();
              }
            }
            for each (var l6 in findallAnswers3.result(ClauseBag)) {
              for each (var l7 in YP.univ(Head, new ListPair(Name, ArgsList))) {
                for each (var l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames)) {
                  var findallAnswers4 = new FindallAnswers(MergedArgName);
                  for each (var l9 in member(ArgName, FunctionArgNames)) {
                    cutIf5:
                    {
                      for each (var l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName)) {
                        findallAnswers4.add();
                        break cutIf5;
                      }
                      for each (var l11 in YP.unify(MergedArgName, ArgName)) {
                        findallAnswers4.add();
                      }
                    }
                  }
                  for each (var l9 in findallAnswers4.result(MergedArgNames)) {
                    for each (var l10 in maplist_arg(MergedArgNames, FunctionArgs)) {
                      for each (var l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode)) {
                        cutIf6:
                        {
                          if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                            for each (var l14 in YP.unify(ReturnType, Atom.a("void"))) {
                              cutIf7:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf7;
                                }
                                cutIf8:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf8;
                                  }
                                  cutIf9:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf9;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            break cutIf6;
                          }
                          cutIf10:
                          {
                            if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                              for each (var l15 in YP.unify(ReturnType, Atom.a("bool"))) {
                                cutIf11:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                    for each (var l18 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf11;
                                  }
                                  cutIf12:
                                  {
                                    if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf12;
                                    }
                                    cutIf13:
                                    {
                                      if (CompilerState.codeUsesYield(State)) {
                                        for each (var l20 in YP.unify(BodyWithReturn, BodyCode)) {
                                          for each (var l21 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                            yield false;
                                          }
                                        }
                                        break cutIf13;
                                      }
                                      for each (var l19 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              break cutIf10;
                            }
                            for each (var l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>"))) {
                              cutIf14:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf14;
                                }
                                cutIf15:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf15;
                                  }
                                  cutIf16:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf16;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            break cutIf2;
          }
          for each (var l5 in YP.unify(Head, FirstRule)) {
            CompilerState.startFunction(State, Head);
            var findallAnswers17 = new FindallAnswers(new Functor2("f", ArgAssignments, Calls));
            for each (var l6 in member(new Functor2("f", Rule, VariableNameSuggestions), SamePredicateRuleList)) {
              for each (var l7 in compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls)) {
                findallAnswers17.add();
              }
            }
            for each (var l6 in findallAnswers17.result(ClauseBag)) {
              for each (var l7 in YP.univ(Head, new ListPair(Name, ArgsList))) {
                for each (var l8 in getFunctionArgNames(ArgsList, 1, FunctionArgNames)) {
                  var findallAnswers18 = new FindallAnswers(MergedArgName);
                  for each (var l9 in member(ArgName, FunctionArgNames)) {
                    cutIf19:
                    {
                      for each (var l11 in argAssignedAll(ArgName, ClauseBag, MergedArgName)) {
                        findallAnswers18.add();
                        break cutIf19;
                      }
                      for each (var l11 in YP.unify(MergedArgName, ArgName)) {
                        findallAnswers18.add();
                      }
                    }
                  }
                  for each (var l9 in findallAnswers18.result(MergedArgNames)) {
                    for each (var l10 in maplist_arg(MergedArgNames, FunctionArgs)) {
                      for each (var l11 in maplist_compileClause(ClauseBag, MergedArgNames, BodyCode)) {
                        cutIf20:
                        {
                          if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                            for each (var l14 in YP.unify(ReturnType, Atom.a("void"))) {
                              cutIf21:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf21;
                                }
                                cutIf22:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf22;
                                  }
                                  cutIf23:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf23;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            break cutIf20;
                          }
                          cutIf24:
                          {
                            if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                              for each (var l15 in YP.unify(ReturnType, Atom.a("bool"))) {
                                cutIf25:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                    for each (var l18 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf25;
                                  }
                                  cutIf26:
                                  {
                                    if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf26;
                                    }
                                    cutIf27:
                                    {
                                      if (CompilerState.codeUsesYield(State)) {
                                        for each (var l20 in YP.unify(BodyWithReturn, BodyCode)) {
                                          for each (var l21 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                            yield false;
                                          }
                                        }
                                        break cutIf27;
                                      }
                                      for each (var l19 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                              break cutIf24;
                            }
                            for each (var l14 in YP.unify(ReturnType, Atom.a("IEnumerable<bool>"))) {
                              cutIf28:
                              {
                                if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
                                  for each (var l17 in append(BodyCode, new ListPair(Atom.a("returnfalse"), Atom.NIL), BodyWithReturn)) {
                                    for each (var l18 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                      yield false;
                                    }
                                  }
                                  break cutIf28;
                                }
                                cutIf29:
                                {
                                  if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
                                    for each (var l18 in YP.unify(BodyWithReturn, BodyCode)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                    break cutIf29;
                                  }
                                  cutIf30:
                                  {
                                    if (CompilerState.codeUsesYield(State)) {
                                      for each (var l19 in YP.unify(BodyWithReturn, BodyCode)) {
                                        for each (var l20 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                          yield false;
                                        }
                                      }
                                      break cutIf30;
                                    }
                                    for each (var l18 in append(BodyCode, new ListPair(new Functor1("blockScope", new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.fail"), Atom.NIL), new ListPair(Atom.a("yieldfalse"), Atom.NIL)), Atom.NIL)), Atom.NIL), BodyWithReturn)) {
                                      for each (var l19 in YP.unify(FunctionCode, new Functor("function", [ReturnType, Name, FunctionArgs, BodyWithReturn]))) {
                                        yield false;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function samePredicateRuleList(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var First = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Atom.NIL))) {
      for each (var l3 in YP.unify(arg2, new ListPair(First, Atom.NIL))) {
        for each (var l4 in YP.unify(arg3, Atom.NIL)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var SamePredicateRuleList = arg2;
    var RestRules = arg3;
    var First = new Variable();
    var Rest = new Variable();
    var FirstRule = new Variable();
    var x6 = new Variable();
    var SecondRule = new Variable();
    var x8 = new Variable();
    var x9 = new Variable();
    var FirstHead = new Variable();
    var x11 = new Variable();
    var SecondHead = new Variable();
    var x13 = new Variable();
    var Name = new Variable();
    var Arity = new Variable();
    var RestSamePredicates = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(new Functor2("f", FirstRule, x6), First)) {
        for each (var l4 in YP.unify(new ListPair(new Functor2("f", SecondRule, x8), x9), Rest)) {
          cutIf1:
          {
            for each (var l6 in YP.unify(new Functor2(":-", FirstHead, x11), FirstRule)) {
              cutIf2:
              {
                for each (var l8 in YP.unify(new Functor2(":-", SecondHead, x13), SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf3:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf3;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                  break cutIf2;
                }
                for each (var l8 in YP.unify(SecondHead, SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf4:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf4;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                }
              }
              break cutIf1;
            }
            for each (var l6 in YP.unify(FirstHead, FirstRule)) {
              cutIf5:
              {
                for each (var l8 in YP.unify(new Functor2(":-", SecondHead, x13), SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf6:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf6;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                  break cutIf5;
                }
                for each (var l8 in YP.unify(SecondHead, SecondRule)) {
                  for each (var l9 in YP.functor(FirstHead, Name, Arity)) {
                    cutIf7:
                    {
                      for each (var l11 in YP.functor(SecondHead, Name, Arity)) {
                        for each (var l12 in samePredicateRuleList(Rest, RestSamePredicates, RestRules)) {
                          for each (var l13 in YP.unify(SamePredicateRuleList, new ListPair(First, RestSamePredicates))) {
                            yield true;
                            return;
                          }
                        }
                        break cutIf7;
                      }
                      for each (var l11 in YP.unify(SamePredicateRuleList, new ListPair(First, Atom.NIL))) {
                        for each (var l12 in YP.unify(RestRules, Rest)) {
                          yield true;
                          return;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function maplist_compileClause(arg1, arg2, arg3) {
  {
    var _MergedArgNames = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var MergedArgNames = arg2;
    var ArgAssignments = new Variable();
    var Calls = new Variable();
    var Rest = new Variable();
    var ClauseCode = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", ArgAssignments, Calls), Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("blockScope", ClauseCode), RestResults))) {
        for each (var l4 in prependArgAssignments(ArgAssignments, Calls, MergedArgNames, ClauseCode)) {
          for each (var l5 in maplist_compileClause(Rest, MergedArgNames, RestResults)) {
            yield true;
            return;
          }
        }
      }
    }
  }
}

function prependArgAssignments(arg1, arg2, arg3, arg4) {
  {
    var _MergedArgNames = arg3;
    var In = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, In)) {
        for each (var l4 in YP.unify(arg4, In)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var In = arg2;
    var MergedArgNames = arg3;
    var ClauseCode = arg4;
    var VariableName = new Variable();
    var ArgName = new Variable();
    var RestArgAssignments = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("f", VariableName, ArgName), RestArgAssignments))) {
      cutIf1:
      {
        for each (var l4 in member(VariableName, MergedArgNames)) {
          for each (var l5 in prependArgAssignments(RestArgAssignments, In, MergedArgNames, ClauseCode)) {
            yield true;
            return;
          }
          break cutIf1;
        }
        for each (var l4 in prependArgAssignments(RestArgAssignments, new ListPair(new Functor3("declare", Atom.a("object"), VariableName, new Functor1("var", ArgName)), In), MergedArgNames, ClauseCode)) {
          yield true;
          return;
        }
      }
    }
  }
}

function argAssignedAll(arg1, arg2, VariableName) {
  {
    var _ArgName = arg1;
    for each (var l2 in YP.unify(arg2, Atom.NIL)) {
      if (YP.nonvar(VariableName)) {
        yield true;
        return;
      }
    }
  }
  {
    var ArgName = arg1;
    var ArgAssignments = new Variable();
    var _Calls = new Variable();
    var RestClauseBag = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(new Functor2("f", ArgAssignments, _Calls), RestClauseBag))) {
      for each (var l3 in member(new Functor2("f", VariableName, ArgName), ArgAssignments)) {
        for each (var l4 in argAssignedAll(ArgName, RestClauseBag, VariableName)) {
          yield false;
        }
      }
    }
  }
}

function maplist_arg(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var First = new Variable();
    var Rest = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg2, new ListPair(new Functor1("arg", First), RestResults))) {
        for each (var l4 in maplist_arg(Rest, RestResults)) {
          yield true;
          return;
        }
      }
    }
  }
}

function getFunctionArgNames(arg1, arg2, arg3) {
  {
    var _StartArgNumber = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var StartArgNumber = arg2;
    var x1 = new Variable();
    var Rest = new Variable();
    var ArgName = new Variable();
    var RestFunctionArgs = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var NextArgNumber = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(ArgName, RestFunctionArgs))) {
        for each (var l4 in YP.number_codes(StartArgNumber, NumberCodes)) {
          for each (var l5 in YP.atom_codes(NumberAtom, NumberCodes)) {
            for each (var l6 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
              for each (var l7 in YP.unify(NextArgNumber, YP.add(StartArgNumber, 1))) {
                for each (var l8 in getFunctionArgNames(Rest, NextArgNumber, RestFunctionArgs)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileBodyWithHeadBindings(Rule, VariableNameSuggestions, State, ArgAssignments, Calls) {
  {
    var Head = new Variable();
    var Body = new Variable();
    var x8 = new Variable();
    var HeadArgs = new Variable();
    var CompiledHeadArgs = new Variable();
    var BodyCode = new Variable();
    var VariableNamesList = new Variable();
    var ArgUnifications = new Variable();
    for each (var l2 in YP.unify(new Functor2(":-", Head, Body), Rule)) {
      CompilerState.newVariableNames(State, Rule, VariableNameSuggestions);
      for each (var l3 in YP.univ(Head, new ListPair(x8, HeadArgs))) {
        for each (var l4 in maplist_compileTerm(HeadArgs, State, CompiledHeadArgs)) {
          for each (var l5 in compileRuleBody(Body, State, BodyCode)) {
            for each (var l6 in CompilerState.variableNamesList(State, VariableNamesList)) {
              for each (var l7 in compileArgUnifications(HeadArgs, CompiledHeadArgs, 1, HeadArgs, BodyCode, ArgUnifications)) {
                for each (var l8 in compileDeclarations(VariableNamesList, HeadArgs, Atom.NIL, ArgAssignments, ArgUnifications, Calls)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    for each (var l2 in compileBodyWithHeadBindings(new Functor2(":-", Rule, Atom.a("true")), VariableNameSuggestions, State, ArgAssignments, Calls)) {
      yield true;
      return;
    }
  }
}

function compileArgUnifications(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var x1 = arg2;
    var x2 = arg3;
    var x3 = arg4;
    var BodyCode = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg5, BodyCode)) {
        for each (var l4 in YP.unify(arg6, BodyCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var Index = arg3;
    var AllHeadArgs = arg4;
    var BodyCode = arg5;
    var ArgUnifications = arg6;
    var HeadArg = new Variable();
    var RestHeadArgs = new Variable();
    var x3 = new Variable();
    var RestCompiledHeadArgs = new Variable();
    var _ArgIndex1 = new Variable();
    var NextIndex = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(HeadArg, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(x3, RestCompiledHeadArgs))) {
        for each (var l4 in getVariableArgIndex1(HeadArg, AllHeadArgs, _ArgIndex1)) {
          for each (var l5 in YP.unify(NextIndex, YP.add(Index, 1))) {
            for each (var l6 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, ArgUnifications)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Index = arg3;
    var AllHeadArgs = arg4;
    var BodyCode = arg5;
    var _HeadArg = new Variable();
    var RestHeadArgs = new Variable();
    var CompiledHeadArg = new Variable();
    var RestCompiledHeadArgs = new Variable();
    var ArgName = new Variable();
    var RestArgUnifications = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var NextIndex = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(_HeadArg, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg2, new ListPair(CompiledHeadArg, RestCompiledHeadArgs))) {
        for each (var l4 in YP.unify(arg6, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.unify"), new ListPair(new Functor1("var", ArgName), new ListPair(CompiledHeadArg, Atom.NIL))), RestArgUnifications), Atom.NIL))) {
          for each (var l5 in YP.number_codes(Index, NumberCodes)) {
            for each (var l6 in YP.atom_codes(NumberAtom, NumberCodes)) {
              for each (var l7 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
                for each (var l8 in YP.unify(NextIndex, YP.add(Index, 1))) {
                  for each (var l9 in compileArgUnifications(RestHeadArgs, RestCompiledHeadArgs, NextIndex, AllHeadArgs, BodyCode, RestArgUnifications)) {
                    yield true;
                    return;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileDeclarations(arg1, arg2, arg3, arg4, arg5, arg6) {
  {
    var _HeadArgs = arg2;
    var ArgAssignmentsIn = new Variable();
    var DeclarationsIn = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, ArgAssignmentsIn)) {
        for each (var l4 in YP.unify(arg4, ArgAssignmentsIn)) {
          for each (var l5 in YP.unify(arg5, DeclarationsIn)) {
            for each (var l6 in YP.unify(arg6, DeclarationsIn)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var HeadArgs = arg2;
    var ArgAssignmentsIn = arg3;
    var ArgAssignmentsOut = arg4;
    var DeclarationsIn = arg5;
    var DeclarationsOut = arg6;
    var VariableName = new Variable();
    var Var = new Variable();
    var RestVariableNames = new Variable();
    var ArgIndex1 = new Variable();
    var NumberCodes = new Variable();
    var NumberAtom = new Variable();
    var ArgName = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", VariableName, Var), RestVariableNames))) {
      for each (var l3 in getVariableArgIndex1(Var, HeadArgs, ArgIndex1)) {
        for each (var l4 in YP.number_codes(ArgIndex1, NumberCodes)) {
          for each (var l5 in YP.atom_codes(NumberAtom, NumberCodes)) {
            for each (var l6 in YP.atom_concat(Atom.a("arg"), NumberAtom, ArgName)) {
              for each (var l7 in compileDeclarations(RestVariableNames, HeadArgs, new ListPair(new Functor2("f", VariableName, ArgName), ArgAssignmentsIn), ArgAssignmentsOut, DeclarationsIn, DeclarationsOut)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var HeadArgs = arg2;
    var ArgAssignmentsIn = arg3;
    var ArgAssignmentsOut = arg4;
    var DeclarationsIn = arg5;
    var VariableName = new Variable();
    var _Var = new Variable();
    var RestVariableNames = new Variable();
    var DeclarationsOut = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("=", VariableName, _Var), RestVariableNames))) {
      for each (var l3 in YP.unify(arg6, new ListPair(new Functor3("declare", Atom.a("Variable"), VariableName, new Functor2("new", Atom.a("Variable"), Atom.NIL)), DeclarationsOut))) {
        for each (var l4 in compileDeclarations(RestVariableNames, HeadArgs, ArgAssignmentsIn, ArgAssignmentsOut, DeclarationsIn, DeclarationsOut)) {
          yield true;
          return;
        }
      }
    }
  }
}

function getVariableArgIndex1(Var, arg2, arg3) {
  {
    var FirstHeadArgs = new Variable();
    var RestHeadArgs = new Variable();
    var x4 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(FirstHeadArgs, RestHeadArgs))) {
      for each (var l3 in YP.unify(arg3, 1)) {
        if (sameVariable(Var, FirstHeadArgs)) {
          cutIf1:
          {
            for each (var l6 in getVariableArgIndex1(Var, RestHeadArgs, x4)) {
              break cutIf1;
            }
            yield false;
          }
          return;
        }
      }
    }
  }
  {
    var Index = arg3;
    var x2 = new Variable();
    var RestHeadArgs = new Variable();
    var RestIndex = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(x2, RestHeadArgs))) {
      for each (var l3 in getVariableArgIndex1(Var, RestHeadArgs, RestIndex)) {
        for each (var l4 in YP.unify(Index, YP.add(1, RestIndex))) {
          yield true;
          return;
        }
      }
    }
  }
}

function compileRuleBody(arg1, arg2, arg3) {
  {
    var A = arg1;
    var State = arg2;
    var PseudoCode = arg3;
    if (YP.var(A)) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("call", A), Atom.a("true")), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      if (YP.var(A)) {
        for each (var l4 in compileRuleBody(new Functor2(",", new Functor1("call", A), B), State, PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in compileFunctorCall(A, State, ACode)) {
        if (CompilerState.isDetNoneOut(State, A)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            for each (var l6 in YP.unify(PseudoCode, new ListPair(ACode, BCode))) {
              yield true;
              return;
            }
          }
        }
        if (CompilerState.isSemidetNoneOut(State, A)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            for each (var l6 in YP.unify(PseudoCode, new ListPair(new Functor2("if", ACode, BCode), Atom.NIL))) {
              yield true;
              return;
            }
          }
        }
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          for each (var l5 in YP.unify(PseudoCode, new ListPair(new Functor2("foreach", ACode, BCode), Atom.NIL))) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(";", new Functor2("->", A, T), B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(";", new Functor2("->", A, new Functor2(",", T, C)), new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(";", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(";", new Functor2(",", A, C), new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("\\+", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("if", new Functor1("not", ACode), BCode), Atom.NIL))) {
        if (CompilerState.isSemidetNoneOut(State, A)) {
          for each (var l5 in compileFunctorCall(A, State, ACode)) {
            for each (var l6 in compileRuleBody(B, State, BCode)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("\\+", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, Atom.a("fail")), Atom.a("true")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("once", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, Atom.a("true")), Atom.a("fail")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("->", A, T), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor2(";", new Functor2("->", A, T), Atom.a("fail")), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("\\=", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("\\+", new Functor2("=", A, B)), C), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var ACode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("!"), A))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in append(ACode, new ListPair(Atom.a("yieldbreak"), Atom.NIL), PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Name = new Variable();
    var A = new Variable();
    var ACode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("$CUTIF", Name), A))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in append(ACode, new ListPair(new Functor1("breakBlock", Name), Atom.NIL), PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg2;
    var x1 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("fail"), x1))) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", Atom.a("true"), A))) {
      for each (var l3 in compileRuleBody(A, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var Term = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var TermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2("is", A, Term), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.unify"), new ListPair(ACode, new ListPair(TermCode, Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ACode)) {
          for each (var l5 in compileExpression(Term, State, TermCode)) {
            for each (var l6 in compileRuleBody(B, State, BCode)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var ACode = new Variable();
    var B = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("$DET_NONE_OUT", ACode), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(ACode, BCode))) {
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var FunctionName = new Variable();
    var X1Code = new Variable();
    var X2Code = new Variable();
    var BCode = new Variable();
    var Name = new Variable();
    var X1 = new Variable();
    var X2 = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", A, B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("if", new Functor2("call", FunctionName, new ListPair(X1Code, new ListPair(X2Code, Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in YP.univ(A, ListPair.make([Name, X1, X2]))) {
          for each (var l5 in binaryExpressionConditional(Name, FunctionName)) {
            for each (var l6 in compileExpression(X1, State, X1Code)) {
              for each (var l7 in compileExpression(X2, State, X2Code)) {
                for each (var l8 in compileRuleBody(B, State, BCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    var TemplateCode = new Variable();
    var FindallAnswers = new Variable();
    var GoalAndAddCode = new Variable();
    var BagCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("findall", Template, Goal, Bag), B))) {
      for each (var l3 in compileTerm(Template, State, TemplateCode)) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("findallAnswers"), FindallAnswers)) {
          for each (var l5 in compileRuleBody(new Functor2(",", Goal, new Functor2(",", new Functor1("$DET_NONE_OUT", new Functor3("callMember", new Functor1("var", FindallAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode)) {
            for each (var l6 in compileTerm(Bag, State, BagCode)) {
              for each (var l7 in compileRuleBody(B, State, BCode)) {
                for each (var l8 in append(new ListPair(new Functor3("declare", Atom.a("FindallAnswers"), FindallAnswers, new Functor2("new", Atom.a("FindallAnswers"), new ListPair(TemplateCode, Atom.NIL))), GoalAndAddCode), new ListPair(new Functor2("foreach", new Functor3("callMember", new Functor1("var", FindallAnswers), Atom.a("result"), new ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("bagof", Template, Goal, Bag), B))) {
      for each (var l3 in compileBagof(Atom.a("result"), Template, Goal, Bag, B, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var Template = new Variable();
    var Goal = new Variable();
    var Bag = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("setof", Template, Goal, Bag), B))) {
      for each (var l3 in compileBagof(Atom.a("resultSet"), Template, Goal, Bag, B, State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("call", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.getIterator"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("current_predicate", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("foreach", new Functor2("call", Atom.a("YP.current_predicate"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode), Atom.NIL))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("asserta", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("call", Atom.a("YP.asserta"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var B = new Variable();
    var ATermCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("assertz", A), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("call", Atom.a("YP.assertz"), new ListPair(ATermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL))), BCode))) {
        for each (var l4 in compileTerm(A, State, ATermCode)) {
          for each (var l5 in compileRuleBody(B, State, BCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor1("assert", A), B))) {
      for each (var l3 in compileRuleBody(new Functor2(",", new Functor1("assertz", A), B), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var Goal = new Variable();
    var Catcher = new Variable();
    var Handler = new Variable();
    var B = new Variable();
    var CatchGoal = new Variable();
    var GoalTermCode = new Variable();
    var BCode = new Variable();
    var CatcherTermCode = new Variable();
    var HandlerAndBCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor3("catch", Goal, Catcher, Handler), B))) {
      for each (var l3 in YP.unify(arg3, ListPair.make([new Functor3("declare", Atom.a("YP.Catch"), CatchGoal, new Functor2("new", Atom.a("YP.Catch"), new ListPair(GoalTermCode, new ListPair(new Functor2("call", Atom.a("getDeclaringClass"), Atom.NIL), Atom.NIL)))), new Functor2("foreach", new Functor1("var", CatchGoal), BCode), new Functor2("foreach", new Functor3("callMember", new Functor1("var", CatchGoal), Atom.a("unifyExceptionOrThrow"), new ListPair(CatcherTermCode, Atom.NIL)), HandlerAndBCode)]))) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("catchGoal"), CatchGoal)) {
          for each (var l5 in compileTerm(Goal, State, GoalTermCode)) {
            for each (var l6 in compileTerm(Catcher, State, CatcherTermCode)) {
              for each (var l7 in compileRuleBody(B, State, BCode)) {
                for each (var l8 in compileRuleBody(new Functor2(",", Handler, B), State, HandlerAndBCode)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var C = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(",", new Functor2(",", A, B), C))) {
      for each (var l3 in compileRuleBody(new Functor2(",", A, new Functor2(",", B, C)), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", A, B))) {
      if (YP.var(A)) {
        for each (var l4 in compileRuleBody(new Functor2(";", new Functor1("call", A), B), State, PseudoCode)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var A = new Variable();
    var T = new Variable();
    var B = new Variable();
    var CutIfLabel = new Variable();
    var Code = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", new Functor2("->", A, T), B))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor2("breakableBlock", CutIfLabel, Code), Atom.NIL))) {
        for each (var l4 in CompilerState.gensym(State, Atom.a("cutIf"), CutIfLabel)) {
          for each (var l5 in compileRuleBody(new Functor2(";", new Functor2(",", A, new Functor2(",", new Functor1("$CUTIF", CutIfLabel), T)), B), State, Code)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var _B = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", Atom.a("!"), _B))) {
      for each (var l3 in compileRuleBody(Atom.a("!"), State, PseudoCode)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var PseudoCode = arg3;
    var A = new Variable();
    var B = new Variable();
    var ACode = new Variable();
    var BCode = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2(";", A, B))) {
      for each (var l3 in compileRuleBody(A, State, ACode)) {
        for each (var l4 in compileRuleBody(B, State, BCode)) {
          for each (var l5 in append(ACode, BCode, PseudoCode)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("return"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("returntrue"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("!"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("yieldtrue"), new ListPair(Atom.a("yieldbreak"), Atom.NIL)))) {
        CompilerState.setCodeUsesYield(State);
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg2;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("$CUTIF", Name))) {
      for each (var l3 in YP.unify(arg3, new ListPair(new Functor1("breakBlock", Name), Atom.NIL))) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("return"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("detNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("returntrue"), Atom.NIL))) {
        if (CompilerState.determinismEquals(State, Atom.a("semidetNoneOut"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.a("true"))) {
      for each (var l3 in YP.unify(arg3, new ListPair(Atom.a("yieldfalse"), Atom.NIL))) {
        CompilerState.setCodeUsesYield(State);
        yield true;
        return;
      }
    }
  }
  {
    var A = arg1;
    var State = arg2;
    var PseudoCode = arg3;
    for each (var l2 in compileRuleBody(new Functor2(",", A, Atom.a("true")), State, PseudoCode)) {
      yield true;
      return;
    }
  }
}

function compileBagof(ResultMethod, Template, Goal, Bag, B, State, PseudoCode) {
  {
    var TemplateCode = new Variable();
    var GoalTermCode = new Variable();
    var UnqualifiedGoal = new Variable();
    var BagofAnswers = new Variable();
    var GoalAndAddCode = new Variable();
    var BagCode = new Variable();
    var BCode = new Variable();
    for each (var l2 in compileTerm(Template, State, TemplateCode)) {
      for each (var l3 in compileTerm(Goal, State, GoalTermCode)) {
        for each (var l4 in unqualifiedGoal(Goal, UnqualifiedGoal)) {
          for each (var l5 in CompilerState.gensym(State, Atom.a("bagofAnswers"), BagofAnswers)) {
            for each (var l6 in compileRuleBody(new Functor2(",", UnqualifiedGoal, new Functor2(",", new Functor1("$DET_NONE_OUT", new Functor3("callMember", new Functor1("var", BagofAnswers), Atom.a("add"), Atom.NIL)), Atom.a("fail"))), State, GoalAndAddCode)) {
              for each (var l7 in compileTerm(Bag, State, BagCode)) {
                for each (var l8 in compileRuleBody(B, State, BCode)) {
                  for each (var l9 in append(new ListPair(new Functor3("declare", Atom.a("BagofAnswers"), BagofAnswers, new Functor2("new", Atom.a("BagofAnswers"), new ListPair(TemplateCode, new ListPair(GoalTermCode, Atom.NIL)))), GoalAndAddCode), new ListPair(new Functor2("foreach", new Functor3("callMember", new Functor1("var", BagofAnswers), ResultMethod, new ListPair(BagCode, Atom.NIL)), BCode), Atom.NIL), PseudoCode)) {
                    yield true;
                    return;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function unqualifiedGoal(arg1, arg2) {
  {
    var Goal = arg1;
    for each (var l2 in YP.unify(arg2, new Functor1("call", Goal))) {
      if (YP.var(Goal)) {
        yield true;
        return;
      }
    }
  }
  {
    var UnqualifiedGoal = arg2;
    var x1 = new Variable();
    var Goal = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("^", x1, Goal))) {
      for each (var l3 in unqualifiedGoal(Goal, UnqualifiedGoal)) {
        yield true;
        return;
      }
    }
  }
  {
    var UnqualifiedGoal = new Variable();
    for each (var l2 in YP.unify(arg1, UnqualifiedGoal)) {
      for each (var l3 in YP.unify(arg2, UnqualifiedGoal)) {
        yield true;
        return;
      }
    }
  }
}

function binaryExpressionConditional(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=:="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.equal"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=\\="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.notEqual"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.greaterThan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.lessThan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">="))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.greaterThanOrEqual"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.lessThanOrEqual"))) {
        yield true;
        return;
      }
    }
  }
}

function compileFunctorCall(Functor_1, State, PseudoCode) {
  {
    var FunctorName = new Variable();
    var FunctorArgs = new Variable();
    var x6 = new Variable();
    var Arity = new Variable();
    var FunctionName = new Variable();
    var CompiledArgs = new Variable();
    for each (var l2 in YP.univ(Functor_1, new ListPair(FunctorName, FunctorArgs))) {
      for each (var l3 in YP.functor(Functor_1, x6, Arity)) {
        for each (var l4 in functorCallFunctionName(State, FunctorName, Arity, FunctionName)) {
          for each (var l5 in maplist_compileTerm(FunctorArgs, State, CompiledArgs)) {
            cutIf1:
            {
              if (YP.termEqual(FunctionName, Atom.NIL)) {
                for each (var l8 in YP.unify(PseudoCode, new Functor2("call", Atom.a("YP.matchDynamic"), new ListPair(new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", FunctorName), Atom.NIL)), new ListPair(new Functor1("objectArray", CompiledArgs), Atom.NIL))))) {
                  yield true;
                  return;
                }
                break cutIf1;
              }
              for each (var l7 in YP.unify(PseudoCode, new Functor3("functorCall", FunctionName, FunctorArgs, CompiledArgs))) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
}

function functorCallFunctionName(arg1, arg2, arg3, arg4) {
  {
    var _State = arg1;
    var Name = arg2;
    var Arity = arg3;
    var x4 = arg4;
    if (functorCallIsSpecialForm(Name, Arity)) {
      return;
    }
  }
  {
    var x1 = arg1;
    var Name = arg2;
    var Arity = arg3;
    var FunctionName = arg4;
    for each (var l2 in functorCallYPFunctionName(Name, Arity, FunctionName)) {
      yield true;
      return;
    }
  }
  {
    var State = arg1;
    var Arity = arg3;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg2, Name)) {
      for each (var l3 in YP.unify(arg4, Name)) {
        if (CompilerState.nameArityHasModule(State, Name, Arity, Atom.a(""))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg1;
    var _Arity = arg3;
    var Name = new Variable();
    for each (var l2 in YP.unify(arg2, Name)) {
      for each (var l3 in YP.unify(arg4, Name)) {
        for each (var l4 in Atom.module(Name, Atom.a(""))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var _Arity = arg3;
    for each (var l2 in YP.unify(arg4, Atom.NIL)) {
      for each (var l3 in Atom.module(Name, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var Arity = arg3;
    var x4 = arg4;
    var Module = new Variable();
    var Message = new Variable();
    for each (var l2 in Atom.module(Name, Module)) {
      for each (var l3 in YP.atom_concat(Atom.a("Not supporting calls to external module: "), Module, Message)) {
        YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("callable"), new Functor2("/", Name, Arity)), Message));
        yield true;
        return;
      }
    }
  }
  {
    var _State = arg1;
    var Name = arg2;
    var _Arity = arg3;
    var x4 = arg4;
    YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("callable"), Name), Atom.a("Term is not callable")));
    yield true;
    return;
  }
}

function functorCallIsSpecialForm(Name, Arity) {
  {
    var x3 = new Variable();
    if (YP.termEqual(Arity, 0)) {
      if (YP.termEqual(Name, Atom.a("!"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("fail"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("true"))) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 1)) {
      if (YP.termEqual(Name, Atom.a("\\+"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("once"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("$CUTIF"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("$DET_NONE_OUT"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("call"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("current_predicate"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("asserta"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("assertz"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("assert"))) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 2)) {
      if (YP.termEqual(Name, Atom.a(";"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a(","))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("->"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("\\="))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("is"))) {
        return true;
      }
      for each (var l3 in binaryExpressionConditional(Name, x3)) {
        return true;
      }
    }
    if (YP.termEqual(Arity, 3)) {
      if (YP.termEqual(Name, Atom.a("findall"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("bagof"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("setof"))) {
        return true;
      }
      if (YP.termEqual(Name, Atom.a("catch"))) {
        return true;
      }
    }
  }
  return false;
}

function functorCallYPFunctionName(arg1, arg2, arg3) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.unify"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=.."))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.univ"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("var"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.var"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nonvar"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.nonvar"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("arg"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.arg"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("functor"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.functor"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("repeat"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.repeat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("get_code"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.get_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_op"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_op"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_length"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_length"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_concat"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_concat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sub_atom"))) {
      for each (var l3 in YP.unify(arg2, 5)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.sub_atom"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_chars"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_chars"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom_codes"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom_codes"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("char_code"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.char_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_chars"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number_chars"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number_codes"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number_codes"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("copy_term"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.copy_term"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sort"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.sort"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("nl"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.nl"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("write"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.write"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("put_code"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.put_code"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("see"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.see"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("seen"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.seen"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("tell"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.tell"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("told"))) {
      for each (var l3 in YP.unify(arg2, 0)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.told"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("clause"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.clause"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("retract"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.retract"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abolish"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.abolish"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("retractall"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.retractall"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atom"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atom"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("integer"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.integer"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.isFloat"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("number"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.number"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atomic"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.atomic"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("compound"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.compound"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("ground"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.ground"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("=="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\=="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termNotEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@<"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termLessThan"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@=<"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termLessThanOrEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@>"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termGreaterThan"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("@>="))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.termGreaterThanOrEqual"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("throw"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.throwException"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_prolog_flag"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("set_prolog_flag"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.set_prolog_flag"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_input"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_input"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("current_output"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("YP.current_output"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read_term"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read_term2"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read_term"))) {
      for each (var l3 in YP.unify(arg2, 3)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read_term3"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read"))) {
      for each (var l3 in YP.unify(arg2, 1)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read1"))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("read"))) {
      for each (var l3 in YP.unify(arg2, 2)) {
        for each (var l4 in YP.unify(arg3, Atom.a("Parser.read2"))) {
          yield true;
          return;
        }
      }
    }
  }
}

function compileTerm(arg1, arg2, arg3) {
  {
    var Term = arg1;
    var State = arg2;
    var VariableName = new Variable();
    for each (var l2 in YP.unify(arg3, new Functor1("var", VariableName))) {
      if (YP.var(Term)) {
        for each (var l4 in CompilerState.getVariableName(State, Term, VariableName)) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, new Functor1("var", Atom.a("Atom.NIL")))) {
        yield true;
        return;
      }
    }
  }
  {
    var Term = arg1;
    var State = arg2;
    var Code = arg3;
    var ModuleCode = new Variable();
    if (YP.atom(Term)) {
      cutIf1:
      {
        for each (var l4 in compileAtomModule(Term, 0, State, ModuleCode)) {
          for each (var l5 in YP.unify(Code, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Term), new ListPair(ModuleCode, Atom.NIL))))) {
            yield true;
            return;
          }
          break cutIf1;
        }
        for each (var l4 in YP.unify(Code, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Term), Atom.NIL)))) {
          yield true;
          return;
        }
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var CompiledList = new Variable();
    var x5 = new Variable();
    var Rest2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new Functor2("call", Atom.a("ListPair.make"), new ListPair(new Functor1("objectArray", CompiledList), Atom.NIL)))) {
        if (YP.nonvar(Rest)) {
          for each (var l5 in YP.unify(Rest, new ListPair(x5, Rest2))) {
            if (YP.termNotEqual(Rest2, Atom.NIL)) {
              for each (var l7 in maplist_compileTerm(new ListPair(First, Rest), State, CompiledList)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var Arg1 = new Variable();
    var Arg2 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new Functor2("new", Atom.a("ListPair"), new ListPair(Arg1, new ListPair(Arg2, Atom.NIL))))) {
        for each (var l4 in compileTerm(First, State, Arg1)) {
          for each (var l5 in compileTerm(Rest, State, Arg2)) {
            yield true;
            return;
          }
        }
      }
    }
  }
  {
    var Term = arg1;
    var State = arg2;
    var Result = arg3;
    var Name = new Variable();
    var TermArgs = new Variable();
    var x6 = new Variable();
    var Arity = new Variable();
    var ModuleCode = new Variable();
    var NameCode = new Variable();
    var X1 = new Variable();
    var Arg1 = new Variable();
    var X2 = new Variable();
    var Arg2 = new Variable();
    var X3 = new Variable();
    var Arg3 = new Variable();
    var Args = new Variable();
    for each (var l2 in YP.univ(Term, new ListPair(Name, TermArgs))) {
      cutIf2:
      {
        if (YP.termEqual(TermArgs, Atom.NIL)) {
          for each (var l5 in YP.unify(Result, new Functor1("object", Name))) {
            yield true;
            return;
          }
          break cutIf2;
        }
        for each (var l4 in YP.functor(Term, x6, Arity)) {
          cutIf3:
          {
            for each (var l6 in compileAtomModule(Name, Arity, State, ModuleCode)) {
              for each (var l7 in YP.unify(NameCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Name), new ListPair(ModuleCode, Atom.NIL))))) {
                cutIf4:
                {
                  for each (var l9 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in YP.unify(Result, new Functor2("new", Atom.a("Functor1"), new ListPair(NameCode, new ListPair(Arg1, Atom.NIL))))) {
                        yield true;
                        return;
                      }
                    }
                    break cutIf4;
                  }
                  cutIf5:
                  {
                    for each (var l10 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                      for each (var l11 in compileTerm(X1, State, Arg1)) {
                        for each (var l12 in compileTerm(X2, State, Arg2)) {
                          for each (var l13 in YP.unify(Result, new Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2])))) {
                            yield true;
                            return;
                          }
                        }
                      }
                      break cutIf5;
                    }
                    for each (var l10 in YP.unify(TermArgs, ListPair.make([X1, X2, X3]))) {
                      for each (var l11 in compileTerm(X1, State, Arg1)) {
                        for each (var l12 in compileTerm(X2, State, Arg2)) {
                          for each (var l13 in compileTerm(X3, State, Arg3)) {
                            for each (var l14 in YP.unify(Result, new Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3])))) {
                              yield true;
                              return;
                            }
                          }
                        }
                      }
                    }
                    for each (var l10 in maplist_compileTerm(TermArgs, State, Args)) {
                      for each (var l11 in YP.unify(Result, new Functor2("new", Atom.a("Functor"), new ListPair(NameCode, new ListPair(new Functor1("objectArray", Args), Atom.NIL))))) {
                        yield true;
                        return;
                      }
                    }
                  }
                }
              }
              break cutIf3;
            }
            for each (var l6 in YP.unify(NameCode, new Functor1("object", Name))) {
              cutIf6:
              {
                for each (var l8 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
                  for each (var l9 in compileTerm(X1, State, Arg1)) {
                    for each (var l10 in YP.unify(Result, new Functor2("new", Atom.a("Functor1"), new ListPair(NameCode, new ListPair(Arg1, Atom.NIL))))) {
                      yield true;
                      return;
                    }
                  }
                  break cutIf6;
                }
                cutIf7:
                {
                  for each (var l9 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in compileTerm(X2, State, Arg2)) {
                        for each (var l12 in YP.unify(Result, new Functor2("new", Atom.a("Functor2"), ListPair.make([NameCode, Arg1, Arg2])))) {
                          yield true;
                          return;
                        }
                      }
                    }
                    break cutIf7;
                  }
                  for each (var l9 in YP.unify(TermArgs, ListPair.make([X1, X2, X3]))) {
                    for each (var l10 in compileTerm(X1, State, Arg1)) {
                      for each (var l11 in compileTerm(X2, State, Arg2)) {
                        for each (var l12 in compileTerm(X3, State, Arg3)) {
                          for each (var l13 in YP.unify(Result, new Functor2("new", Atom.a("Functor3"), ListPair.make([NameCode, Arg1, Arg2, Arg3])))) {
                            yield true;
                            return;
                          }
                        }
                      }
                    }
                  }
                  for each (var l9 in maplist_compileTerm(TermArgs, State, Args)) {
                    for each (var l10 in YP.unify(Result, new Functor2("new", Atom.a("Functor"), new ListPair(NameCode, new ListPair(new Functor1("objectArray", Args), Atom.NIL))))) {
                      yield true;
                      return;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

function compileAtomModule(Name, arg2, arg3, ModuleCode) {
  {
    var Arity = arg2;
    var State = arg3;
    if (CompilerState.nameArityHasModule(State, Name, Arity, Atom.a(""))) {
      for each (var l3 in YP.unify(ModuleCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Atom.a("")), Atom.NIL)))) {
        yield true;
        return;
      }
    }
  }
  {
    var _Arity = arg2;
    var _State = arg3;
    var Module = new Variable();
    for each (var l2 in Atom.module(Name, Module)) {
      if (YP.termNotEqual(Module, Atom.NIL)) {
        for each (var l4 in YP.unify(ModuleCode, new Functor2("call", Atom.a("Atom.a"), new ListPair(new Functor1("object", Module), Atom.NIL)))) {
          yield true;
          return;
        }
      }
    }
  }
}

function maplist_compileTerm(arg1, arg2, arg3) {
  {
    var _State = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg3, Atom.NIL)) {
        yield true;
        return;
      }
    }
  }
  {
    var State = arg2;
    var First = new Variable();
    var Rest = new Variable();
    var FirstResult = new Variable();
    var RestResults = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(First, Rest))) {
      for each (var l3 in YP.unify(arg3, new ListPair(FirstResult, RestResults))) {
        if (YP.nonvar(Rest)) {
          for each (var l5 in compileTerm(First, State, FirstResult)) {
            for each (var l6 in maplist_compileTerm(Rest, State, RestResults)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
}

function compileExpression(Term, State, Result) {
  {
    var Name = new Variable();
    var TermArgs = new Variable();
    var X1 = new Variable();
    var FunctionName = new Variable();
    var Arg1 = new Variable();
    var x9 = new Variable();
    var X2 = new Variable();
    var Arg2 = new Variable();
    var x12 = new Variable();
    var Arity = new Variable();
    if (YP.nonvar(Term)) {
      for each (var l3 in YP.univ(Term, new ListPair(Name, TermArgs))) {
        if (YP.atom(Name)) {
          cutIf1:
          {
            for each (var l6 in YP.unify(TermArgs, new ListPair(X1, Atom.NIL))) {
              for each (var l7 in unaryFunction(Name, FunctionName)) {
                for each (var l8 in compileExpression(X1, State, Arg1)) {
                  for each (var l9 in YP.unify(Result, new Functor2("call", FunctionName, new ListPair(Arg1, Atom.NIL)))) {
                    yield true;
                    return;
                  }
                }
                break cutIf1;
              }
            }
            cutIf2:
            {
              for each (var l7 in YP.unify(Term, new ListPair(x9, Atom.NIL))) {
                for each (var l8 in compileTerm(Term, State, Result)) {
                  yield true;
                  return;
                }
                break cutIf2;
              }
              cutIf3:
              {
                for each (var l8 in YP.unify(TermArgs, new ListPair(X1, new ListPair(X2, Atom.NIL)))) {
                  for each (var l9 in binaryFunction(Name, FunctionName)) {
                    for each (var l10 in compileExpression(X1, State, Arg1)) {
                      for each (var l11 in compileExpression(X2, State, Arg2)) {
                        for each (var l12 in YP.unify(Result, new Functor2("call", FunctionName, new ListPair(Arg1, new ListPair(Arg2, Atom.NIL))))) {
                          yield true;
                          return;
                        }
                      }
                    }
                    break cutIf3;
                  }
                }
                for each (var l8 in YP.functor(Term, x12, Arity)) {
                  YP.throwException(new Functor2("error", new Functor2("type_error", Atom.a("evaluable"), new Functor2("/", Name, Arity)), Atom.a("Not an expression function")));
                  yield false;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    for each (var l2 in compileTerm(Term, State, Result)) {
      yield true;
      return;
    }
  }
}

function unaryFunction(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("-"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.negate"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("abs"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.abs"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sign"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sign"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("float"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.toFloat"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("floor"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.floor"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("truncate"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.truncate"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("round"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.round"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("ceiling"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.ceiling"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sin"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sin"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("cos"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.cos"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("atan"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.atan"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("exp"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.exp"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("log"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.log"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("sqrt"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.sqrt"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseComplement"))) {
        yield true;
        return;
      }
    }
  }
}

function binaryFunction(arg1, arg2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("+"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.add"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("-"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.subtract"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("*"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.multiply"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("/"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.divide"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("//"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.intDivide"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("mod"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.mod"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("**"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.pow"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a(">>"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseShiftRight"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("<<"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseShiftLeft"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("/\\"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseAnd"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("\\/"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.bitwiseOr"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("min"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.min"))) {
        yield true;
        return;
      }
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("max"))) {
      for each (var l3 in YP.unify(arg2, Atom.a("YP.max"))) {
        yield true;
        return;
      }
    }
  }
}

function convertFunctionCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("public class YPInnerClass {}"));
      YP.nl();
      YP.write(Atom.a("public static Type getDeclaringClass() { return typeof(YPInnerClass).DeclaringType; }"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var ReturnType = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    var Level = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [ReturnType, Name, ArgList, Body]))) {
      YP.write(Atom.a("public static "));
      YP.write(ReturnType);
      YP.write(Atom.a(" "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(Level, 1)) {
        convertStatementListCSharp(Body, Level);
        YP.write(Atom.a("}"));
        YP.nl();
        YP.nl();
        return;
      }
    }
  }
}

function convertStatementListCSharp(arg1, x1, x2) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield true;
      return;
    }
  }
}

function convertStatementListCSharp(arg1, Level) {
  {
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NewStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      for each (var l3 in append(Body, new ListPair(new Functor1("label", Name), RestStatements), NewStatements)) {
        convertStatementListCSharp(NewStatements, Level);
        return;
      }
    }
  }
  {
    var Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", Type, Name, Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Type);
      YP.write(Atom.a(" "));
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield return true;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield return false;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("yield break;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return true;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("return false;"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("label", Name), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a(":"));
      YP.nl();
      cutIf1:
      {
        if (YP.termEqual(RestStatements, Atom.NIL)) {
          convertIndentationCSharp(Level);
          YP.write(Atom.a("{}"));
          YP.nl();
          convertStatementListCSharp(RestStatements, Level);
          return;
          break cutIf1;
        }
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("goto "));
      YP.write(Name);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      convertStatementListCSharp(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level);
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
  {
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("if ("));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("foreach (bool l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListCSharp(Body, NextLevel);
        convertIndentationCSharp(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListCSharp(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationCSharp(Level);
      YP.write(Atom.a("throw "));
      convertExpressionCSharp(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListCSharp(RestStatements, Level);
      return;
    }
  }
}

function convertIndentationCSharp(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionCSharp(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListCSharp(Tail);
          return;
          break cutIf1;
        }
        convertArgListCSharp(Tail);
        return;
      }
    }
  }
}

function convertExpressionCSharp(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(Atom.a("object "));
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionCSharp(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Atom.a("new "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("null"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("!("));
      convertExpressionCSharp(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionCSharp(X);
      YP.write(Atom.a(") && ("));
      convertExpressionCSharp(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("new object[] {"));
      convertArgListCSharp(ArgList);
      YP.write(Atom.a("}"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesCSharp(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesCSharp(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesCSharp(RestCodes);
        return;
      }
    }
  }
}

function convertFunctionJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("function getDeclaringClass() { return null; }"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var x1 = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [x1, Name, ArgList, Body]))) {
      YP.write(Atom.a("function "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(") {"));
      YP.nl();
      convertStatementListJavascript(Body, 1);
      YP.write(Atom.a("}"));
      YP.nl();
      YP.nl();
      return;
    }
  }
}

function convertStatementListJavascript(arg1, arg2) {
  {
    var x1 = arg2;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a(":"));
      YP.nl();
      convertIndentationJavascript(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var _Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", _Type, Name, Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("var "));
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("yield true;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("yield false;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return true;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("return false;"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("break "));
      YP.write(Name);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      convertStatementListJavascript(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(");"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
  {
    var Level = arg2;
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("{"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("if ("));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("for each (var l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(") {"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        convertStatementListJavascript(Body, NextLevel);
        convertIndentationJavascript(Level);
        YP.write(Atom.a("}"));
        YP.nl();
        convertStatementListJavascript(RestStatements, Level);
        return;
      }
    }
  }
  {
    var Level = arg2;
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationJavascript(Level);
      YP.write(Atom.a("throw "));
      convertExpressionJavascript(Expression);
      YP.write(Atom.a(";"));
      YP.nl();
      convertStatementListJavascript(RestStatements, Level);
      return;
    }
  }
}

function convertIndentationJavascript(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionJavascript(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListJavascript(Tail);
          return;
          break cutIf1;
        }
        convertArgListJavascript(Tail);
        return;
      }
    }
  }
}

function convertExpressionJavascript(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionJavascript(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Atom.a("new "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("null"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("!("));
      convertExpressionJavascript(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionJavascript(X);
      YP.write(Atom.a(") && ("));
      convertExpressionJavascript(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("["));
      convertArgListJavascript(ArgList);
      YP.write(Atom.a("]"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesJavascript(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesJavascript(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesJavascript(RestCodes);
        return;
      }
    }
  }
}

function convertFunctionPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.a("getDeclaringClass"))) {
      YP.write(Atom.a("def getDeclaringClass():"));
      YP.nl();
      YP.write(Atom.a("  return globals()"));
      YP.nl();
      YP.nl();
      return;
    }
  }
  {
    var x1 = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var Body = new Variable();
    var Level = new Variable();
    var HasBreakableBlock = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor("function", [x1, Name, ArgList, Body]))) {
      YP.write(Atom.a("def "));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a("):"));
      YP.nl();
      for each (var l3 in YP.unify(Level, 1)) {
        cutIf1:
        {
          if (hasBreakableBlockPython(Body)) {
            for each (var l6 in YP.unify(HasBreakableBlock, 1)) {
              cutIf2:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("doBreak = False"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                    YP.nl();
                    return;
                  }
                  break cutIf2;
                }
                for each (var l8 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                  YP.nl();
                  return;
                }
              }
            }
            break cutIf1;
          }
          for each (var l5 in YP.unify(HasBreakableBlock, 0)) {
            cutIf3:
            {
              if (YP.termEqual(HasBreakableBlock, 1)) {
                convertIndentationPython(Level);
                YP.write(Atom.a("doBreak = False"));
                YP.nl();
                for each (var l8 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                  YP.nl();
                  return;
                }
                break cutIf3;
              }
              for each (var l7 in convertStatementListPython(Body, Level, HasBreakableBlock)) {
                YP.nl();
                return;
              }
            }
          }
        }
      }
    }
  }
}

function hasBreakableBlockPython(arg1) {
  {
    var _Name = new Variable();
    var _Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", _Name, _Body), _RestStatements))) {
      return true;
    }
  }
  {
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var _Expression = new Variable();
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", _Expression, Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var _Expression = new Variable();
    var Body = new Variable();
    var _RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", _Expression, Body), _RestStatements))) {
      if (hasBreakableBlockPython(Body)) {
        return true;
      }
    }
  }
  {
    var x1 = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(x1, RestStatements))) {
      if (hasBreakableBlockPython(RestStatements)) {
        return true;
      }
    }
  }
  return false;
}

function convertStatementListPython(arg1, arg2, arg3) {
  {
    var x1 = arg2;
    var x2 = arg3;
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      yield true;
      return;
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("breakableBlock", Name, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = False"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        cutIf1:
        {
          if (YP.termEqual(Body, Atom.NIL)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("if "));
            YP.write(Name);
            YP.write(Atom.a(":"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("doBreak = False"));
            YP.nl();
            convertIndentationPython(Level);
            YP.write(Atom.a("if doBreak:"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("break"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
            break cutIf1;
          }
          convertIndentationPython(Level);
          YP.write(Atom.a("for _ in [1]:"));
          YP.nl();
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("if "));
            YP.write(Name);
            YP.write(Atom.a(":"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("doBreak = False"));
            YP.nl();
            convertIndentationPython(Level);
            YP.write(Atom.a("if doBreak:"));
            YP.nl();
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("break"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var _Type = new Variable();
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("declare", _Type, Name, Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("assign", Name, Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldtrue"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("yield True"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldfalse"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("yield False"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("yieldbreak"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("return"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returntrue"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return True"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Atom.a("returnfalse"), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("return False"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("breakBlock", Name), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a(" = True"));
      YP.nl();
      convertIndentationPython(Level);
      YP.write(Atom.a("doBreak = True"));
      YP.nl();
      convertIndentationPython(Level);
      YP.write(Atom.a("break"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("call", Name, ArgList), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("functorCall", Name, _FunctorArgs, ArgList), RestStatements))) {
      for each (var l3 in convertStatementListPython(new ListPair(new Functor2("call", Name, ArgList), RestStatements), Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor3("callMember", new Functor1("var", Obj), Name, ArgList), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("blockScope", Body), RestStatements))) {
      cutIf2:
      {
        if (YP.termEqual(HasBreakableBlock, 1)) {
          if (YP.termNotEqual(Body, Atom.NIL)) {
            convertIndentationPython(Level);
            YP.write(Atom.a("for _ in [1]:"));
            YP.nl();
            for each (var l6 in YP.unify(NextLevel, YP.add(Level, 1))) {
              for each (var l7 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
                cutIf3:
                {
                  if (YP.termEqual(HasBreakableBlock, 1)) {
                    cutIf4:
                    {
                      if (YP.greaterThan(Level, 1)) {
                        convertIndentationPython(Level);
                        YP.write(Atom.a("if doBreak:"));
                        YP.nl();
                        convertIndentationPython(NextLevel);
                        YP.write(Atom.a("break"));
                        YP.nl();
                        for each (var l12 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                          yield true;
                          return;
                        }
                        break cutIf4;
                      }
                      for each (var l11 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                        yield true;
                        return;
                      }
                    }
                    break cutIf3;
                  }
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                }
              }
            }
            break cutIf2;
          }
        }
        for each (var l4 in YP.unify(NextLevel, Level)) {
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            cutIf5:
            {
              if (YP.termEqual(HasBreakableBlock, 1)) {
                cutIf6:
                {
                  if (YP.greaterThan(Level, 1)) {
                    convertIndentationPython(Level);
                    YP.write(Atom.a("if doBreak:"));
                    YP.nl();
                    convertIndentationPython(NextLevel);
                    YP.write(Atom.a("break"));
                    YP.nl();
                    for each (var l10 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                      yield true;
                      return;
                    }
                    break cutIf6;
                  }
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                }
                break cutIf5;
              }
              for each (var l7 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                yield true;
                return;
              }
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("if", Expression, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("if "));
      convertExpressionPython(Expression);
      YP.write(Atom.a(":"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        cutIf7:
        {
          if (YP.termEqual(Body, Atom.NIL)) {
            convertIndentationPython(NextLevel);
            YP.write(Atom.a("pass"));
            YP.nl();
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
            break cutIf7;
          }
          for each (var l5 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
            for each (var l6 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var Body = new Variable();
    var RestStatements = new Variable();
    var NextLevel = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor2("foreach", Expression, Body), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("for l"));
      YP.write(Level);
      YP.write(Atom.a(" in "));
      convertExpressionPython(Expression);
      YP.write(Atom.a(":"));
      YP.nl();
      for each (var l3 in YP.unify(NextLevel, YP.add(Level, 1))) {
        for each (var l4 in YP.unify(NextLevel, YP.add(Level, 1))) {
          cutIf8:
          {
            if (YP.termEqual(Body, Atom.NIL)) {
              convertIndentationPython(NextLevel);
              YP.write(Atom.a("pass"));
              YP.nl();
              cutIf9:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("if doBreak:"));
                  YP.nl();
                  convertIndentationPython(NextLevel);
                  YP.write(Atom.a("break"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                  break cutIf9;
                }
                for each (var l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                  yield true;
                  return;
                }
              }
              break cutIf8;
            }
            for each (var l6 in convertStatementListPython(Body, NextLevel, HasBreakableBlock)) {
              cutIf10:
              {
                if (YP.termEqual(HasBreakableBlock, 1)) {
                  convertIndentationPython(Level);
                  YP.write(Atom.a("if doBreak:"));
                  YP.nl();
                  convertIndentationPython(NextLevel);
                  YP.write(Atom.a("break"));
                  YP.nl();
                  for each (var l9 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                    yield true;
                    return;
                  }
                  break cutIf10;
                }
                for each (var l8 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
                  yield true;
                  return;
                }
              }
            }
          }
        }
      }
    }
  }
  {
    var Level = arg2;
    var HasBreakableBlock = arg3;
    var Expression = new Variable();
    var RestStatements = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(new Functor1("throw", Expression), RestStatements))) {
      convertIndentationPython(Level);
      YP.write(Atom.a("raise "));
      convertExpressionPython(Expression);
      YP.nl();
      for each (var l3 in convertStatementListPython(RestStatements, Level, HasBreakableBlock)) {
        yield true;
        return;
      }
    }
  }
}

function convertIndentationPython(Level) {
  {
    var N = new Variable();
    for each (var l2 in YP.unify(N, YP.multiply(Level, 2))) {
      repeatWrite(Atom.a(" "), N);
      return;
    }
  }
}

function convertArgListPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Head = new Variable();
    var Tail = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Head, Tail))) {
      convertExpressionPython(Head);
      cutIf1:
      {
        if (YP.termNotEqual(Tail, Atom.NIL)) {
          YP.write(Atom.a(", "));
          convertArgListPython(Tail);
          return;
          break cutIf1;
        }
        convertArgListPython(Tail);
        return;
      }
    }
  }
}

function convertExpressionPython(arg1) {
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("arg", X))) {
      YP.write(X);
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("call", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var _FunctorArgs = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("functorCall", Name, _FunctorArgs, ArgList))) {
      convertExpressionPython(new Functor2("call", Name, ArgList));
      return;
    }
  }
  {
    var Obj = new Variable();
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor3("callMember", new Functor1("var", Obj), Name, ArgList))) {
      YP.write(Obj);
      YP.write(Atom.a("."));
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("new", Name, ArgList))) {
      YP.write(Name);
      YP.write(Atom.a("("));
      convertArgListPython(ArgList);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var Name = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("var", Name))) {
      YP.write(Name);
      return;
    }
  }
  {
    for each (var l2 in YP.unify(arg1, Atom.a("null"))) {
      YP.write(Atom.a("None"));
      return;
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("not", X))) {
      YP.write(Atom.a("not ("));
      convertExpressionPython(X);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var X = new Variable();
    var Y = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor2("and", X, Y))) {
      YP.write(Atom.a("("));
      convertExpressionPython(X);
      YP.write(Atom.a(") and ("));
      convertExpressionPython(Y);
      YP.write(Atom.a(")"));
      return;
    }
  }
  {
    var ArgList = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("objectArray", ArgList))) {
      YP.write(Atom.a("["));
      convertArgListPython(ArgList);
      YP.write(Atom.a("]"));
      return;
    }
  }
  {
    var X = new Variable();
    var Codes = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      if (YP.atom(X)) {
        YP.write(Atom.a("\""));
        for each (var l4 in YP.atom_codes(X, Codes)) {
          convertStringCodesPython(Codes);
          YP.write(Atom.a("\""));
          return;
        }
      }
    }
  }
  {
    var X = new Variable();
    for each (var l2 in YP.unify(arg1, new Functor1("object", X))) {
      YP.write(X);
      return;
    }
  }
}

function convertStringCodesPython(arg1) {
  {
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      return;
    }
  }
  {
    var Code = new Variable();
    var RestCodes = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(Code, RestCodes))) {
      for each (var l3 in putCStringCode(Code)) {
        convertStringCodesPython(RestCodes);
        return;
      }
    }
  }
}

function putCStringCode(Code) {
  {
    var HexDigit = new Variable();
    var HexChar = new Variable();
    if (YP.lessThanOrEqual(Code, 31)) {
      cutIf1:
      {
        if (YP.lessThanOrEqual(Code, 15)) {
          YP.write(Atom.a("\\x0"));
          for each (var l5 in YP.unify(HexDigit, Code)) {
            cutIf2:
            {
              if (YP.lessThanOrEqual(HexDigit, 9)) {
                for each (var l8 in YP.unify(HexChar, YP.add(HexDigit, 48))) {
                  YP.put_code(HexChar);
                  yield true;
                  return;
                }
                break cutIf2;
              }
              for each (var l7 in YP.unify(HexChar, YP.add(HexDigit, 55))) {
                YP.put_code(HexChar);
                yield true;
                return;
              }
            }
          }
          break cutIf1;
        }
        YP.write(Atom.a("\\x1"));
        for each (var l4 in YP.unify(HexDigit, YP.subtract(Code, 16))) {
          cutIf3:
          {
            if (YP.lessThanOrEqual(HexDigit, 9)) {
              for each (var l7 in YP.unify(HexChar, YP.add(HexDigit, 48))) {
                YP.put_code(HexChar);
                yield true;
                return;
              }
              break cutIf3;
            }
            for each (var l6 in YP.unify(HexChar, YP.add(HexDigit, 55))) {
              YP.put_code(HexChar);
              yield true;
              return;
            }
          }
        }
      }
    }
  }
  {
    if (YP.termEqual(Code, 34)) {
      YP.put_code(92);
      YP.put_code(34);
      yield true;
      return;
    }
  }
  {
    if (YP.termEqual(Code, 92)) {
      YP.put_code(92);
      YP.put_code(92);
      yield true;
      return;
    }
  }
  {
    YP.put_code(Code);
    yield true;
    return;
  }
}

function member(X, arg2) {
  {
    var x2 = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(X, x2))) {
      yield false;
    }
  }
  {
    var x2 = new Variable();
    var Rest = new Variable();
    for each (var l2 in YP.unify(arg2, new ListPair(x2, Rest))) {
      for each (var l3 in member(X, Rest)) {
        yield false;
      }
    }
  }
}

function append(arg1, arg2, arg3) {
  {
    var List = new Variable();
    for each (var l2 in YP.unify(arg1, Atom.NIL)) {
      for each (var l3 in YP.unify(arg2, List)) {
        for each (var l4 in YP.unify(arg3, List)) {
          yield false;
        }
      }
    }
  }
  {
    var List2 = arg2;
    var X = new Variable();
    var List1 = new Variable();
    var List12 = new Variable();
    for each (var l2 in YP.unify(arg1, new ListPair(X, List1))) {
      for each (var l3 in YP.unify(arg3, new ListPair(X, List12))) {
        for each (var l4 in append(List1, List2, List12)) {
          yield false;
        }
      }
    }
  }
}

