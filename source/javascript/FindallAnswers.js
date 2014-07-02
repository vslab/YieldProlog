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
