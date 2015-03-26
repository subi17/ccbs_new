define input parameter icOutputFile as char no-undo.
def input param ldtPaiva  as date no-undo.

DEFINE VARIABLE ok AS LOGICAL NO-UNDO init true.
def stream sout.
output stream sout to value(icOutputFile) append.
put stream sout unformatted 
  "TEST yoigo_0count.p" skip(1).

DEF VAR liCount  AS INT  NO-UNDO.

FOR EACH Invoice NO-LOCK WHERE
         Invoice.InvDate = ldtPaiva AND
         Invoice.InvAmt  = 0        AND
         Invoice.InvType = 1:

    liCount = liCount + 1.

END.

DISP stream sout liCount skip(2).
output stream sout close.
