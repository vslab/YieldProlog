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
