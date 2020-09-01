# working on TODO:
- nkCast
- timporting error in magic
- unmapped type tyrange
- nkCurly properly ( currently it's just an array )
- mnew
- mdotdot
- nkCaseStmt
- proc quit*(errorcode: int = QuitSuccess) {.magic: "Exit",
    importc: "exit", header: "glue" noreturn.}
- varargs ( would unlock `echo`)


- ref objects

FIXME: PROC ORDERING IS WRONG? FOR RECURSIVE PROCS the symbol isn't in the list when the body is processed?
TODO: add " [" & name exceptn & "]\n" when throwing

TODO: Split `symLoc` into `backendDeref`and `backendAddr` and review where they're used. Try to merge getLoc with those
There's a wrong store of b.stackptr when nkObjconstr is called (may be in genAsgn) (tobjects)

- nimwasm.cfg: only concerns about js, separate html template (-d:html)