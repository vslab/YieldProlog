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
    document.write("Find relations:<br>");
    var Brother = new Variable();
    for each (var l1 in brother("Hillary", Brother))
        document.write("Hillary has brother " +
              Brother.getValue() + ".<br>");
			  
    document.write("Check if it is square:<br>");
    for each (var l1 in squaredRectangle(10, 10))
        document.write("10 by 10 rectangle is square.<br>");

    document.write("Make it square:<br>");
    var Width = new Variable();
    var Height = new Variable();
    for each (var l1 in Width.unify(10)) {
        for each (var l2 in squaredRectangle(Width, Height))
            document.write("A square of width " + 
                Width.getValue() + " has height " +
                Height.getValue() + ".<br>");
	}

    document.write("Make it square before we know the width:<br>");
    for each (var l1 in squaredRectangle(Width, Height)) {
        for each (var l2 in Width.unify(10))
            document.write("A square of width " +
                Width.getValue() + " has height " +
                Height.getValue() + ".<br>");
	}

    document.write("Get one match:<br>");
    for each (var l1 in anyBrother("Hillary", Brother))
        document.write("Hillary has a brother " +
              Brother.getValue() + ".<br>");
    for each (var l1 in anyBrother("Bill", Brother))
        document.write("Bill has a brother " +
              Brother.getValue() + ".<br>");

    document.write("Use cut for negation:<br>");
    for each (var l1 in noBrother("Hillary"))
        document.write("Hillary has no brother.<br>");
    for each (var l1 in noBrother("Chelsea"))
        document.write("Chelsea has no brother.<br>");
}

function brother(Person, Brother) {
    for each (var l1 in YP.unify(Person, "Hillary")) {
        for each (var l2 in YP.unify(Brother, "Tony"))
            yield false;
        for each (var l2 in YP.unify(Brother, "Hugh"))
            yield false;
	}
    for each (var l1 in YP.unify(Person, "Bill")) {
        for each (var l2 in YP.unify(Brother, "Roger"))
            yield false;
	}
}

function squaredRectangle(Width, Height) {
    for each (var l1 in YP.unify(Width, Height))
        yield false;
}

function anyBrother(Person, Brother) {
    for each (var l1 in brother(Person, Brother)) {
        yield false;
        break;
	}
}

function noBrother(Person) {
    var Brother = new Variable();
    for each (var l1 in brother(Person, Brother))
        return;
    yield false;
}

main();


