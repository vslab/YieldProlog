# Copyright (C) 2007-2008, Jeff Thompson
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without 
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright 
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright 
#       notice, this list of conditions and the following disclaimer in the 
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the copyright holder nor the names of its contributors 
#       may be used to endorse or promote products derived from this software 
#       without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import sys
# Hack sys.path for the examples.
sys.path.append("../..")
from YP import *
from Atom import *
from Compiler import *
from ListPair import *
from Variable import *

def main():
    YP.assertFact(Atom.a("brother"), \
        [Atom.a("Hillary"), Atom.a("Hugh")])
    YP.assertFact(Atom.a("brother"), \
        [Atom.a("Hillary"), Atom.a("Tony")])
    YP.assertFact(Atom.a("brother"), \
        [Atom.a("Bill"), Atom.a("Roger")])

    Brother = Variable()
    print "Using dynamic assert:"
    for l1 in YP.matchDynamic \
              (Atom.a("brother"), \
               [Atom.a("Hillary"), Brother]):
        print "Hillary has brother", \
            Brother.getValue()

    prologCode = \
        "uncle(Person, Uncle) :- \n" + \
        "  parent(Person, Parent), \n" + \
        "  brother(Parent, Uncle). \n"
    print "# Compiled code:"
    compileAndWrite(prologCode)

    prologCode = \
        ":- import('', [parent/2]). \n" + \
        "uncle(Person, Uncle) :- \n" + \
        "  parent(Person, Parent), \n" + \
        "  brother(Parent, Uncle). \n"
    print "# Calling an imported function:"
    compileAndWrite(prologCode)

    prologCode = \
        "parent('Chelsea', 'Hillary'). \n" + \
        "parent('Chelsea', 'Bill'). \n" + \
        \
        "uncle(Person, Uncle) :- \n" + \
        "  parent(Person, Parent), \n" + \
        "  brother(Parent, Uncle). \n"
    print "# Calling a locally-defined function:"
    compileAndWrite(prologCode)

    prologCode = \
        ":- import('', [parent/2]). \n" + \
        "uncle(Person, Uncle) :- \n" + \
        "  Goal = parent(Person, Parent), \n" + \
        "  Goal, \n" + \
        "  brother(Parent, Uncle). \n"
    print "# Calling a dynamic goal:"
    compileAndWrite(prologCode)

    print "Calling compiled code having a dynamic goal:"
    Person = Variable()
    Uncle = Variable()
    for l1 in uncle(Person, Uncle):
        print Person.getValue(), "has uncle", \
              Uncle.getValue()

def compileAndWrite(prologCode):
    YP.tell(sys.stdout)
    YP.see(YP.StringReader(prologCode))
    TermList = Variable()
    PseudoCode = Variable()
    for l1 in parseInput(TermList):
        for l2 in makeFunctionPseudoCode \
                  (TermList, PseudoCode):
            convertFunctionPython(PseudoCode)
    YP.seen()

def getDeclaringClass():
  return globals()

def parent(arg1, arg2):
  for l1 in YP.unify \
      (arg1, Atom.a("Chelsea")):
    for l2 in YP.unify \
        (arg2, Atom.a("Hillary")):
      yield False
  for l1 in YP.unify \
      (arg1, Atom.a("Chelsea")):
    for l2 in YP.unify \
        (arg2, Atom.a("Bill")):
      yield False

def uncle(Person, Uncle):
  Goal = Variable()
  Parent = Variable()
  for l1 in YP.unify \
      (Goal, Functor2 \
       (Atom.a("parent", Atom.a("")), \
        Person, Parent)):
    for l2 in YP.getIterator \
        (Goal, getDeclaringClass()):
      for l3 in YP.matchDynamic \
          (Atom.a("brother"), [Parent, Uncle]):
        yield False

main()


