copy /B readme.js + makeYieldProlog.bat + endComment.js + ^
  YP.js + ^
  Variable.js + ^
  Atom.js + ^
  Functor1.js + ^
  Functor2.js + ^
  Functor3.js + ^
  Functor.js + ^
  ListPair.js + ^
  PrologException.js + ^
  IndexedAnswers.js + ^
  FindallAnswers.js + ^
  BagofAnswers.js + ^
  Parser.js + ^
  Compiler.js ^
  YieldProlog.js
rem Copy the result to the web interface and bin.
copy YieldProlog.js ..\..\doc
copy YieldProlog.js ..\..\bin
copy ..\..\doc\queryEditor.html ..\..\bin
