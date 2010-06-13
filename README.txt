================================================================
Verdi Neruda - Meta-interpreter collection for Prolog.
Release 1.0 

Copyright (c) 2010  Victor Lagerkvist.      All Rights Reserved.
Verdi Neruda is free software.    You can redistribute it and/or
modify it under the terms of the simplified BSD license.
================================================================


CONTENTS

 1. License
 2. Verdi Neruda web site
 3. Installation and running
 4. Examples

1. LICENSE

Copyright 2010 Victor Lagerkvist. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation
are those of the authors and should not be interpreted as representing
official policies, either expressed or implied, of the copyright holders.

2. VERDI NERUDA WEB SITE

Visit the Verdi Neruda Github www-page at:
http://joelbyte.github.com/verdi-neruda/

3. INSTALLATION AND RUNNING

Verdi Neruda requires Logtalk 2.x.

* Fetch the latest source code, either as an archive or from the git
  repository, and extract it to a directory of your choice.
* Start Logtalk from that directory.
* Type {loader}. (Including '.').
* If everything went according to the plan you should be greeted by
  the welcoming message.

4. EXAMPLES

Follow the previous instructions to get everything up and
running. First we're going to run some predefined programs in the
included databases. Begin by typing 'databases.'  from the shell -
this should print a list of the currently loaded databases. The demo
database 'demodb' should be included in the list. Next type
'listing(demodb).' to print the contents of the database. The output
should look something like:

    append([],A,A) <-
    	  true.
    append([A|B],C,[A|D]) <-
    	  append(B,C,D).
    .  
    .  
    .

Which means that the append/3 program is loaded and ready for
action. Next we need to decide which interpreter to use. Fortunately
the shell does not leave much to the imagination - as might be
expected, the 'interpreters.' command prints the currently loaded
interpreters. The list should look like:

    dfs_interpreter 
    bfs_interpreter 
    iddfs_interpreter(A)
    bup_interpreter 
    a_star_interpreter(A)

The variables means that the interpreters are parametric objects and
that additional information is needed in order to run it. The
iddfs-interpreter needs to know the increment and the A*-interpreter
needs to know what weight should be used when calculating the cost of
nodes. To start with let's use the dfs-interpreter and do something
exciting, namely appending two lists!

    prove(dfs_interpreter, append([a,b], [c,d], Xs), demodb).

The prove command takes three arguments. The first is a interpreter,
the second the goal that shall be proved and the last the database
that the clauses are derived from.

To accomplish the same thing with the iddfs-interpreter with an
increment of 1 we need only type

   prove(iddfs_interpreter(1), append([a,b], [c,d], Xs), demodb).

The shell also has support for counting logical inferences. To compare
the dfs- and iddfs-interpreter with the append program we could write:

benchmark(dfs_interpreter, append([a,b,c,d],[e,f], Xs), demodb).  ->
dfs_interpreter inferences: 5

benchmark(iddfs_interpreter(1), append([a,b,c,d],[e,f], Xs), demodb).
-> iddfs_interpreter(1) inferences: 15

For more information regarding the built in shell commands consult the
'help.' command.
