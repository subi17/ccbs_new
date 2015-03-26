{commali.i}
{timestamp.i}
{date.i}
{cparam2.i}

DEFINE VARIABLE lcBarring  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcActive   AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeTS      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lcDate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpool    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOut      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO.

ASSIGN
   ldeTS      = fMakeTS()
   lcDate     = fDateFMT(TODAY,"YYYYMMDD")
   lcFileName = "ccgw_" + lcDate + ".dump"
   lcSpool    = fCParam("DUMPSPOOL","ccgwdump.p")
   lcOut      = fCParam("DUMPOUTGOING","ccgwdump.p").


OUTPUT TO VALUE(lcSpool + lcFileName).

FOR EACH MSOwner NO-LOCK
BREAK
   BY MSOwner.CLI
   BY MSOwner.TSEnd:

   IF MSOwner.CLI = "" THEN NEXT.

   IF LAST-OF(MSOwner.TSEnd) THEN DO:

      FIND FIRST SubSer WHERE
                 SubSer.MsSeq   = MsOwner.MsSeq AND
                 SubSer.ServPac = "BPR"
      NO-LOCK NO-ERROR.
   
      IF AVAIL SubSer AND SubSer.SSStat = 1 THEN
         lcBarring = "Y".
      ELSE
         lcBarring = "N".

      IF MSOwner.TSBegin <= ldeTS AND
         MSOwner.TSEnd   >= ldeTS THEN lcActive = "Y".
      ELSE lcActive = "N".
   
      PUT UNFORMATTED
         TRIM("34" + MSOwner.CLI) ","
         TRIM(STRING(MSOwner.PayType,"PREPAID/POSTPAID")) ","
         TRIM(MSOwner.IMSI)       ","
         TRIM(lcActive)           ","
         TRIM(lcBarring)          CHR(10).

   END.

END.

OUTPUT CLOSE.

UNIX SILENT VALUE("mv " + lcSpool + lcFileName + " " + lcOut).
