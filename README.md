Yield Prolog
============

Welcome to Yield Prolog. This fork is based on the original YieldProlog project available here:
http://yieldprolog.sourceforge.net/

Contents
--------

- **bin** contains executables, including:
  - YPShell.exe - Yield Prolog for C# interactive shell for Windows.
  - queryEditor.html - Yield Prolog browser shell for Javascript.
  - YPShell.py - Yield Prolog for Python interactive shell.

- **doc** contains a snapshot of the Yield Prolog web site including the tutorial.
   Open index.html in your browser.

- **source** contains a folder for the source of each supported platform.
  The examples subfolder for each platform contains the YPShell, tutorial examples and benchmarks code.

Make
----

Here are instructions to make and run Tutorial1 on various platforms.
You can do something similar to make the other tutorials, the benchmarks,
or your own Yield Prolog program.

To make Tutorial1 under Visual C# 2008:
- Open source\csharp\examples\YieldPrologTutorial\YieldPrologTutorial.sln
- Make sure the Startup Object is set correctly, e.g. to Tutorial1
- Press F5 to build and run

To make Tutorial1 under Mono 1.9.1 in Windows:
- On the Start menu, run Programs/Mono 1.9.1 for Windows/Mono-1.9.1 Command Prompt
- Use cd to change directory to source\csharp\examples\YieldPrologTutorial
- Enter: monomake Tutorial1.cs
- Run Tutorial1.exe

To make Tutorial1 under Python 2.6 for Windows:
- On the Start menu, run Programs/Python 2.5/IDLE (Python GUI)
- Select the menu File/Open and open source\python\examples\YieldPrologTutorial\tutorial1.py
- Press F5 to run the module
