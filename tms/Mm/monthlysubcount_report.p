/* ----------------------------------------------------------------------
  MODULE .......: monthlysubcount_report.p
  TASK .........: Count subscriptions of previous month
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 30.07.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}
{Func/email.i}
{Func/multitenantfunc.i}

DEF INPUT PARAMETER idaRunDate AS DATE NO-UNDO.

DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 

lcOutDir = fCparam("DUMPOUTGOING","monthlysubcount.p").
lcConfDir = fCParamC("RepConfDir").

DEF VAR liAmt AS INT NO-UNDO.
DEF VAR ldeStamp AS DEC NO-UNDO.

DEF VAR liloop  AS INT NO-UNDO.
DEF VAR liloop2 AS INT NO-UNDO. 
DEF VAR ldtDate AS DATE NO-UNDO.
DEF VAR lcOutFile AS CHARACTER NO-UNDO. 
DEF VAR ldaMonth AS DATE NO-UNDO.

ldaMonth = idaRunDate.
ldtDate = DATE(MONTH(ldaMonth), 1, YEAR(ldaMonth)).

lcOutFile = lcOutDir +
            CAPS(fgetBrandNamebyTenantId(TENANT-ID(LDBNAME(1)))) + 
            "_monthlysubcount_" +
            STRING(YEAR(ldaMonth) * 100 + MONTH(ldaMonth)) +
            ".txt".

DEF TEMP-TABLE ttMobsub
FIELD CliType  AS CHAR
FIELD PayType  AS LOGICAL
FIELD liCDR    AS INT COLUMN-LABEL "With CDRs"
FIELD liNotCDR AS INT COLUMN-LABEL "Without CDRs"
INDEX CliType CliType.

find last msowner use-index msseq no-lock no-error.

ASSIGN
   ldeStamp = YEAR(ldtDate)  * 10000 +
              MONTH(ldtDate) * 100   +
              DAY(ldtDate) 
   liloop2  = Msowner.msseq.

ldeStamp = fSecOffSet(ldeStamp,-1).

SUBS_LOOP:
DO liLoop = 1 TO liloop2.

FOR EACH MSOwner NO-LOCK where
         MSOwner.msseq   = liloop     AND 
         MSOwner.TSBegin <= ldeStamp  AND
         MSOwner.TSEnd >= ldeStamp
BREAK 
BY MSOwner.MSSeq. 
         
   IF FIRST-OF(MSOwner.MSSeq) THEN DO:      

      liAmt = liAmt + 1.
      
      IF NOT SESSION:BATCH THEN
      IF liamt mod 100 = 0 THEN 
      PUT SCREEN ROW 1 string(liAMT).
   
      FIND FIRST ttMobsub WHERE 
                 ttMobsub.CliType = MSOWner.CliType NO-ERROR.

      IF NOT AVAIL ttMobsub THEN DO:             
         CREATE ttMobsub.
         ASSIGN
         ttMobsub.CliType = MSOWner.CliType
         ttMobsub.PayType = MSOWner.PayType.
      END.

      IF MSowner.clitype = "TARJ3"  THEN DO:
         FIND FIRST PrepCDR WHERE 
                    PrepCDR.cli = MSOwner.Cli AND 
                    Prepcdr.datest >= ldtDate - 60 AND
                    PrepCDR.datest < ldtDate NO-LOCK NO-ERROR.

         IF     AVAIL PrepCDR THEN ttMobsub.liCDR    = ttMobsub.liCDR    + 1.
         IF NOT AVAIL PrepCDR THEN ttMobsub.liNotCDR = ttMobsub.liNotCDR + 1.

      END.
      ELSE ttMobsub.liCDR = ttMobsub.liCDR + 1.
   END.
end.
ENd.

def stream sout.
output stream sout to VALUE(lcOutFile).

put stream sout "Run date " ldtdate FORMAT "99.99.9999" skip.

FOR EACH ttMobsub  NO-LOCK:
   disp stream sout ttMobsub.clitype ttMobsub.licdr ttMobsub.linotcdr when ttMobsub.clitype = "tarj3".
end.

DEFINE VARIABLE liTotalPost AS INTEGER NO-UNDO. 
DEFINE VARIABLE liTotalPre AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPreact AS INTEGER NO-UNDO. 

FOR EACH ttMobsub where
   ttMobsub.paytype = false NO-LOCK:
   accumulate ttMobsub.licdr (total).
END.
liTotalPost = accum total ttMobsub.licdr.
put stream sout unformatted skip(1) "Total postpaid: " liTotalPost skip.

FOR EACH ttMobsub where
   ttMobsub.paytype = true NO-LOCK:
   accumulate ttMobsub.licdr (total).
END.
liTotalPre = accum total ttMobsub.licdr.
put stream sout unformatted "Total prepaid: " liTotalPre " (excl. preactivated without cdrs)" skip.

find ttMobsub where
   ttMobsub.clitype = "tarj3" NO-LOCK NO-ERROR.
IF AVAIL ttMobsub then do:
   liPreact = ttMobsub.linotcdr.
END.

put stream sout unformatted "Preactivated without CDRs: " liPreact skip.
put stream sout unformatted "Total subscriptions (post+prep+preact): " liTotalPost + liTotalPre + liPreact skip.
put stream sout unformatted "Total subscriptions (preact reduction noticed: post+prep+(preact/3)): " liTotalPost + liTotalPre + int(liPreact / 3) skip.

output stream sout close.

/* mail recipients */

GetRecipients(lcConfDir + "monthlysubcount.email").

/* send via mail */
IF xMailAddr > "" THEN DO:

    SendMail(lcOutFile,
             "").
END.

