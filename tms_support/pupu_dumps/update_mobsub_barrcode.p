DISABLE TRIGGERS FOR LOAD OF MobSub.

DEF VAR lcLine      AS CHAR NO-UNDO.
DEF VAR lcBarrCode  AS CHAR NO-UNDO.
DEF VAR liMsSeq     AS INT  NO-UNDO.
DEF VAR liNum       AS INT  NO-UNDO.

DEF STREAM sin.
DEF STREAM sout.

OUTPUT STREAM sout TO "/store/riftp/pupu_dumps/logs/update_mobsub_barrcode.txt".

INPUT STREAM sin from "/store/riftp/pupu_dumps/logs/check_mobsub_barrcode.txt".

REPEAT:

IMPORT STREAM sin UNFORMATTED lcLine.

IF lcLine = "" THEN NEXT.

liMsSeq = INT(ENTRY(1,lcLine,"|")).
IF liMsSeq = 0 OR liMsSeq = ? THEN DO:
   PUT STREAM sout UNFORMATTED lcLine "|Invalid MsSeq" SKIP.
   NEXT.
END.

lcBarrCode = ENTRY(2,lcLine,"|").
IF lcBarrCode = "" THEN DO:
   PUT STREAM sout UNFORMATTED lcLine "|Invalid barrcode" SKIP.
   NEXT.
END.

liNum = liNum + 1.
STATUS DEFAULT STRING(liNum).

FIND FIRST MobSub WHERE
           MobSub.MsSeq = liMsSeq AND
           MobSub.MsStatus = 8 EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL MobSub THEN DO:
   MobSub.BarrCode = lcBarrCode.

   PUT STREAM sout UNFORMATTED lcLine "|"
       STRING(MobSub.MsSeq) "|" MobSub.CLI "|"
       MobSub.BarrCode SKIP.
END.
RELEASE MobSub.

END. /* REPEAT: */

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.
