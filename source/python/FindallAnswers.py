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

from YP import *
from ListPair import *
from Variable import *

# A FindallAnswers holds answers for findall.
class FindallAnswers(object):
    def __init__(self, Template):
        self._template = Template;
        self._bagArray = [];

    def add(self):
        self._bagArray.append(YP.makeCopy(self._template, Variable.CopyStore()))

    def resultArray(self):
        return self._bagArray

    # Unify Bag with the result. This frees the internal answers, so you can only call this once.
    def result(self, Bag):
        value = ListPair.make(self._bagArray)
        # Try to free the memory.
        self._bagArray = None
        return YP.unify(Bag, value)

    # This is a simplified findall when the goal is a single call.
    @staticmethod
    def findall(Template, goal, Bag):
        findallAnswers = FindallAnswers(Template)
        for l1 in goal:
            findallAnswers.add()
        return findallAnswers.result(Bag)

    # Like findall, except return an array of the results.
    @staticmethod
    def findallArray(Template, goal):
        findallAnswers = FindallAnswers(Template)
        for l1 in goal:
            findallAnswers.add()
        return findallAnswers.resultArray()
    
