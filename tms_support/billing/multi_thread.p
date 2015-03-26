DEFINE VARIABLE liThreads           AS INTEGER NO-UNDO INIT 10.
DEFINE VARIABLE liCount             AS INTEGER NO-UNDO.
DEFINE VARIABLE liCountPerThread    AS INTEGER NO-UNDO.
DEFINE VARIABLE liFirstRecord       AS INTEGER NO-UNDO.
DEFINE VARIABLE liLastRecord        AS INTEGER NO-UNDO.
DEFINE VARIABLE liLastSeq           AS INTEGER NO-UNDO.
DEFINE VARIABLE lcThreadMode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcParamList         AS CHARACTER NO-UNDO.

lcThreadMode = "InvSeq".

IF lcThreadMode = "InvSeq" THEN
   FOR EACH InvSeq NO-LOCK BY InvSeq DESC:
      liLastSeq = InvSeq.InvSeq.
      LEAVE.
   END. /* FOR EACH InvSeq NO-LOCK BY InvSeq DESC: */

liCountPerThread = (liLastSeq - (liLastSeq mod liThreads)) / liThreads.

DO liCount = 1 TO liThreads:
   IF liCount = 1 THEN
      liFirstRecord = 1.
   ELSE
      liFirstRecord = liLastRecord + 1.

   liLastRecord = liLastRecord + liCountPerThread.

   IF liCount = liThreads THEN
      liLastRecord = liLastRecord + (liLastSeq mod liThreads).
   
   lcParamList = STRING(liFirstRecord) + "," + STRING(liLastRecord).

   unix silent mpro -b -pf /apps/yoigo/tms/pf/tms.pf  -p /apps/yoigo/tms_support/billing/unbilled_calls.p -param value(lcParamList) &.
END. /* DO liCount = 1 TO liThreads: */
