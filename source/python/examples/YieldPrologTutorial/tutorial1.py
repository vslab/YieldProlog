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

def main():
    print("Names using a return value:")
    for p in personWithReturnValue():
        print(p)

    print("Names using SimpleVariable:")
    P = SimpleVariable()
    for l1 in personWithSimpleVariable(P):
        print(P._value)

    print("Names using UnifyingVariable:")
    Person = UnifyingVariable()
    for l1 in personWithUnify(Person):
        print(Person._value)

    print("Use unify to check a person:")
    for l1 in Person.unify("Hillary"):
        for l2 in personWithUnify(Person):
            print("Hillary is a person.")
    for l1 in Person.unify("Buddy"):
        for l2 in personWithUnify(Person):
            # This won't print.
            print("Buddy is a person.")

    print("Use generalUnify to check a person:")
    for l1 in person("Hillary"):
        print("Hillary is a person.")
    for l1 in person("Buddy"):
        # This won't print.
        print ("Buddy is a person.")

    print("Find relations:")
    Brother = UnifyingVariable()
    for l1 in brother("Hillary", Brother):
        print("Hillary has brother " + Brother._value + ".")

    print("Joining functions:")
    Uncle = UnifyingVariable()
    for l1 in uncle(Person, Uncle):
        print(Person._value + " has uncle " + Uncle._value + ".")

def personWithReturnValue():
    yield "Chelsea"
    yield "Hillary"
    yield "Bill"

class SimpleVariable:
    pass

def personWithSimpleVariable(Person):
    Person._value = "Chelsea"
    yield False
    Person._value = "Hillary"
    yield False
    Person._value = "Bill"
    yield False

class UnifyingVariable:
    def __init__(self):
        self._isBound = False

    def unify(self, arg):
        if not self._isBound:
            self._value = arg
            self._isBound = True
            yield False
            # Remove the binding.
            self._isBound = False
        elif self._value == arg:
            yield False

def personWithUnify(Person):
    for l1 in Person.unify("Chelsea"):
        yield False
    for l1 in Person.unify("Hillary"):
        yield False
    for l1 in Person.unify("Bill"):
        yield False

def generalGetValue(value):
    if isinstance(value, UnifyingVariable):
        if not value._isBound:
            return value
        else:
            return value._value
    else:
        return value

def generalUnify(arg1, arg2):
    arg1Value = generalGetValue(arg1)
    arg2Value = generalGetValue(arg2)
    if isinstance(arg1Value, UnifyingVariable):
        for l1 in arg1Value.unify(arg2Value):
            yield False
    elif isinstance(arg2Value, UnifyingVariable):
        for l1 in arg2Value.unify(arg1Value):
            yield False
    else:
        # Arguments are "normal" types.
        if arg1Value == arg2Value:
            yield False

def person(Person):
    for l1 in generalUnify(Person, "Chelsea"):
        yield False
    for l1 in generalUnify(Person, "Hillary"):
        yield False
    for l1 in generalUnify(Person, "Bill"):
        yield False

def brother(Person, Brother):    
    for l1 in generalUnify(Person, "Hillary"):
        for l2 in generalUnify(Brother, "Tony"):
            yield False
        for l2 in generalUnify(Brother, "Hugh"):
            yield False
    for l1 in generalUnify(Person, "Bill"):
        for l2 in generalUnify(Brother, "Roger"):
            yield False

def parent(Person, Parent):
    for l1 in generalUnify(Person, "Chelsea"):
        for l2 in generalUnify(Parent, "Hillary"):
            yield False
    for l1 in generalUnify(Person, "Chelsea"):
        for l2 in generalUnify(Parent, "Bill"):
            yield False

def uncle(Person, Uncle):
    Parent = UnifyingVariable()
    for l1 in parent(Person, Parent):
        for l2 in brother(Parent, Uncle):
            yield False

main()
