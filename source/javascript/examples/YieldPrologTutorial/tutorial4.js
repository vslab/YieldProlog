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
    YP.assertFact(Atom.a("brother"), 
        [Atom.a("Hillary"), Atom.a("Hugh")]);
    YP.assertFact(Atom.a("brother"), 
        [Atom.a("Hillary"), Atom.a("Tony")]);
    YP.assertFact(Atom.a("brother"), 
        [Atom.a("Bill"), Atom.a("Roger")]);

    var Brother = new Variable();
    document.write("Using dynamic assert:<br>");
    for each (var l1 in YP.matchDynamic
              (Atom.a("brother"),
               [Atom.a("Hillary"), Brother]))
        document.write("Hillary has brother " +
            Brother.getValue() + ".<br>");

    var prologCode =
        "uncle(Person, Uncle) :- \n" +
        "  parent(Person, Parent), \n" +
        "  brother(Parent, Uncle). \n";
    document.write("<br>// Compiled code:<br>");
    compileAndWrite(prologCode);

    prologCode =
        ":- import('', [parent/2]). \n" +
        "uncle(Person, Uncle) :- \n" +
        "  parent(Person, Parent), \n" +
        "  brother(Parent, Uncle). \n";
    document.write("// Calling an imported function:<br>");
    compileAndWrite(prologCode);

    prologCode =
        "parent('Chelsea', 'Hillary'). \n" +
        "parent('Chelsea', 'Bill'). \n" +

        "uncle(Person, Uncle) :- \n" +
        "  parent(Person, Parent), \n" +
        "  brother(Parent, Uncle). \n";
    document.write("// Calling a locally-defined function:<br>");
    compileAndWrite(prologCode);

    prologCode =
        ":- import('', [parent/2]). \n" +
        "uncle(Person, Uncle) :- \n" +
        "  Goal = parent(Person, Parent), \n" +
        "  Goal, \n" +
        "  brother(Parent, Uncle). \n";
    document.write("// Calling a dynamic goal:<br>");
    compileAndWrite(prologCode);

    document.write
      ("Calling compiled code having a dynamic goal:<br>");
    var Person = new Variable();
    var Uncle = new Variable();
    for each (var l1 in uncle(Person, Uncle))
        document.write(Person.getValue() +
            " has uncle " + Uncle.getValue() + ".<br>");
}

function compileAndWrite(prologCode) {
    YP.see(new YP.StringReader(prologCode));
    var output = new YP.StringWriter();
    YP.tell(output);
    var TermList = new Variable();
    var PseudoCode = new Variable();
    for each (var l1 in parseInput(TermList)) {
        for each (var l2 in makeFunctionPseudoCode
                  (TermList, PseudoCode))
            convertFunctionJavascript(PseudoCode);
    }
    YP.seen();
    YP.told();
    document.write
      (output.toString().replace
       (/\n/g,"<br>").replace(/ /g,"&nbsp;"));
}

function getDeclaringClass() { return null; }

function parent(Person, Parent) {
  for each (var l2 in YP.unify
            (Person, Atom.a("Chelsea"))) {
    for each (var l3 in YP.unify
              (Parent, Atom.a("Hillary"))) {
      yield false;
    }
  }
  for each (var l2 in YP.unify
            (Person, Atom.a("Chelsea"))) {
    for each (var l3 in YP.unify
              (Parent, Atom.a("Bill"))) {
      yield false;
    }
  }
}

function uncle(Person, Uncle) {
  {
    var Goal = new Variable();
    var Parent = new Variable();
    for each (var l2 in YP.unify
              (Goal, new Functor2
               (Atom.a("parent", Atom.a("")), 
                Person, Parent))) {
      for each (var l3 in YP.getIterator
                (Goal, getDeclaringClass())) {
        for each (var l4 in YP.matchDynamic
                  (Atom.a("brother"), [Parent, Uncle])) {
          yield false;
        }
      }
    }
  }
}

main();


