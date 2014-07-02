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
from Variable import *

def main():
    Brother = Variable()
    print "Find relations:" 
    for l1 in brother("Hillary", Brother):
        print "Hillary has brother", \
              Brother.getValue(), "."

    print "Check if it is square:"
    for l1 in squaredRectangle(10, 10):
        print "10 by 10 rectangle is square."

    print "Make it square:"
    Width = Variable()
    Height = Variable()
    for l1 in Width.unify(10):
        for l2 in squaredRectangle(Width, Height):
            print "A square of width", \
                Width.getValue(), "has height", \
                Height.getValue(), "."

    print "Make it square before we know the width:"
    for l1 in squaredRectangle(Width, Height):
        for l2 in Width.unify(10):
            print "A square of width", \
                Width.getValue(), "has height", \
                Height.getValue(), "." 

    print "Get one match:"
    for l1 in anyBrother("Hillary", Brother):
        print "Hillary has a brother", \
              Brother.getValue(), "."
    for l1 in anyBrother("Bill", Brother):
        print "Bill has a brother", \
              Brother.getValue(), "."

    print "Use cut for negation:"
    for l1 in noBrother("Hillary"):
        print "Hillary has no brother."
    for l1 in noBrother("Chelsea"):
        print "Chelsea has no brother."

def brother(Person, Brother):
    for l1 in YP.unify(Person, "Hillary"):
        for l2 in YP.unify(Brother, "Tony"):
            yield False
        for l2 in YP.unify(Brother, "Hugh"):
            yield False
    for l1 in YP.unify(Person, "Bill"):
        for l2 in YP.unify(Brother, "Roger"):
            yield False

def squaredRectangle(Width, Height):
    for l1 in YP.unify(Width, Height):
        yield False

def anyBrother(Person, Brother):
    for l1 in brother(Person, Brother):
        yield False
        break

def noBrother(Person):
    Brother = Variable()
    for l1 in brother(Person, Brother):
        return
    yield False

main()


