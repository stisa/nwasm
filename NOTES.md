Notes on nim compiler backends
------------------------------

Here is what I understand of the code generation pass:  
the generator module (cgen, jsgen, vmgen etc) exports a `const`, the pass, that receives the module graph (which to my understanding is the ast after it's type checked and lowered).   
This pass is composed of 4 procs, normally called ``myOpen, myOpenCached, myClose, myProcess``.

These four procs operate on (the same) context object, that generally holds a rope structure which holds the target code generated.

`myOpen` receives the module symbol and is the place in which the context is initialized.   
Then `myProcess` is called for every top level statement, and is where the ast of the statement is converted to target source code (usually by a proc named ``gen``).  
Lastly, `myClose` is called, which closes the module.

These passages are repeated for all modules used by the project.