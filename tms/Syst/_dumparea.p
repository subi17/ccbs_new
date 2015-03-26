def var lLoop  as i  no-undo.
def var lArea  as c  no-undo.

SESSION:NUMERIC-FORMAT = "EUROPEAN".

do lLoop = 1 to NUM-DBS:

   lArea = LDBNAME(lLoop).

   create alias DICTDB for database value(lArea).

   run ./_dumptable.i lArea.

end.
