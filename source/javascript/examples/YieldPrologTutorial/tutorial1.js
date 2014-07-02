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

function main() {
    document.write("Names using a return value:<br>");
    for each (var p in personWithReturnValue())
        document.write(p + "<br>");

    document.write("Names using SimpleVariable:<br>");
    P = new SimpleVariable();
    for each (var l1 in personWithSimpleVariable(P))
        document.write(P._value + "<br>");

    document.write("Names using UnifyingVariable:<br>");
    Person = new UnifyingVariable();
    for each (var l1 in personWithUnify(Person))
        document.write(Person._value + "<br>");

    document.write("Use unify to check a person:<br>");
    for each (var l1 in Person.unify("Hillary")) {
         for each (var l2 in personWithUnify(Person))
            document.write("Hillary is a person.<br>");
	}
    for each (l1 in Person.unify("Buddy")) {
        for each (var l2 in personWithUnify(Person))
            // This won't print.
            document.write("Buddy is a person.<br>");
	}

    document.write("Use generalUnify to check a person:<br>");
    for each (var l1 in person("Hillary"))
        document.write("Hillary is a person.<br>");
    for each (var l1 in person("Buddy"))
        // This won't print.
        document.write ("Buddy is a person.<br>");

    document.write("Find relations:<br>");
    Brother = new UnifyingVariable();
    for each (var l1 in brother("Hillary", Brother))
        document.write("Hillary has brother " + Brother._value + ".<br>");

    document.write("Joining functions:<br>");
    Uncle = new UnifyingVariable();
    for each (var l1 in uncle(Person, Uncle))
        document.write(Person._value + " has uncle " + Uncle._value + ".<br>");
}

function personWithReturnValue() {
    yield "Chelsea";
    yield "Hillary";
    yield "Bill";
}

function SimpleVariable() {
}

function personWithSimpleVariable(Person) {
    Person._value = "Chelsea";
    yield false;
    Person._value = "Hillary";
    yield false;
    Person._value = "Bill";
    yield false;
}

function UnifyingVariable() {
    this._value = null;

    this.unify = function(arg) {
        if (this._value == null) {
            this._value = arg;
            yield false;
            // Remove the binding.
            this._value = null;
		}
        else if (this._value == arg)
            yield false;
	}
}

function personWithUnify(Person) {
    for each (var l1 in Person.unify("Chelsea"))
        yield false;
    for each (var l1 in Person.unify("Hillary"))
        yield false;
    for each (var l1 in Person.unify("Bill"))
        yield false;
}

function generalGetValue(value) {
    if (value instanceof UnifyingVariable) {
        if (value._value == null)
            return value;
        else
            return value._value;
	}
    else
        return value;
}

function generalUnify(arg1, arg2) {
    var arg1Value = generalGetValue(arg1);
    var arg2Value = generalGetValue(arg2);
    if (arg1Value instanceof UnifyingVariable) {
        for each (var l1 in arg1Value.unify(arg2Value))
            yield false;
	}
    else if (arg2Value instanceof UnifyingVariable) {
        for each (var l1 in arg2Value.unify(arg1Value))
            yield false;
	}
    else {
        // Arguments are "normal" types.
        if (arg1Value == arg2Value)
            yield false;
	}
}

function person(Person) {
    for each (var l1 in generalUnify(Person, "Chelsea"))
        yield false;
    for each (var l1 in generalUnify(Person, "Hillary"))
        yield false;
    for each (var l1 in generalUnify(Person, "Bill"))
        yield false;
}

function brother(Person, Brother) {
    for each (var l1 in generalUnify(Person, "Hillary")) {
        for each (var l2 in generalUnify(Brother, "Tony"))
            yield false;
        for each (var l2 in generalUnify(Brother, "Hugh"))
            yield false;
	}
    for each (var l1 in generalUnify(Person, "Bill")) {
        for each (var l2 in generalUnify(Brother, "Roger"))
            yield false;
	}
}

function parent(Person, Parent) {
    for each (var l1 in generalUnify(Person, "Chelsea")) {
        for each (var l2 in generalUnify(Parent, "Hillary"))
            yield false;
	}
    for each (var l1 in generalUnify(Person, "Chelsea")) {
        for each (var l2 in generalUnify(Parent, "Bill"))
            yield false;
	}
}

function uncle(Person, Uncle) {
    var Parent = new UnifyingVariable();
    for each (var l1 in parent(Person, Parent)) {
        for each (var l2 in brother(Parent, Uncle))
            yield false;
	}
}

main();
