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
