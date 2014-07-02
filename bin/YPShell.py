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
# Hack sys.path for imports.
sys.path.append("../source/python")
from YP import *
from Atom import *
from Compiler import *
from ListPair import *
from Variable import *

def main():
    print "Yield Prolog Shell.  Enter ctrl-D to exit."

    while True:
        print "?- ",
        goalString = sys.stdin.readline()
        if goalString == "":
            break
        if goalString == "\n":
            print "Enter ctrl-D to exit."
            continue

        Goal = Variable()
        VariableList = Variable()
        for l1 in parseGoal(goalString, Goal, VariableList):
            try:
                gotMatch = False
                for l2 in YP.getIterator(Goal, None):
                    gotMatch = True
                    if YP.getValue(VariableList) != Atom.NIL:
                        # We are showing values, so allow the user to enter ";" to display the next match.
                        writeValues(VariableList)
                        continuePrompt = sys.stdin.readline()
                        if len(continuePrompt) > 0 and continuePrompt[0] != ';':
                            break

                if gotMatch:
                    if YP.getValue(VariableList) == Atom.NIL:
                        # We didn't show any values.  So just write true after matching (one or more times).
                        print "true"
                    else:
                        print
                else:
                    print "fail"
            except PrologException as exception:
                print str(exception._term)

# Parse goalString and yield for each Goal, setting VariableList to a list of
# (variableAtom = Var).
def parseGoal(goalString, Goal, VariableList):
    # The parser requires a newline at the end.
    YP.see(YP.StringReader(goalString + '\n'))
    TermList = Variable()
    # parseInput sets TermList to a list of f(Goal, VariableList).
    for l1 in parseInput(TermList):
        # Close the input now before yielding.
        YP.seen()
        # Iterate through each member of TermList.
        TermList = YP.getValue(TermList)
        while isinstance(TermList, Functor2) and TermList._name == Atom.DOT:
            # Unify the head of the list with f(Goal, VariableList).
            for l2 in YP.unify(TermList._arg1, Functor2(Atom.F, Goal, VariableList)):
                yield False
                TermList = YP.getValue(TermList._arg2)
            return

    # Close the input in case parseInput failed.
    YP.seen()

# Write out each entry in VariableList which is a list of (variableAtom = Var)
def writeValues(VariableList):
    VariableList = YP.getValue(VariableList)
    while isinstance(VariableList, Functor2) and VariableList._name == Atom.DOT:
        variableAndValue = YP.getValue(VariableList._arg1)
        if isinstance(variableAndValue, Functor2):
            print
            print YP.getValue(variableAndValue._arg1), "=", YP.getValue(variableAndValue._arg2),
        VariableList = YP.getValue(VariableList._arg2)

main()


