/* ----------------------------------------------------------------------
  MODULE .......: cdr_rerate.i 
  TASK .........: CDR re-rating (logging) functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 09.04.09
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

&IF "{&CDR_RERATE_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE CDR_RERATE_I YES

{Syst/commali.i}
{Func/date.i}
{Func/cparam2.i}

DEF STREAM sRerateRep.

DEFINE TEMP-TABLE ttRerateRep NO-UNDO
   FIELD RateCCN AS INT 
   FIELD ErrorCode AS INT
   FIELD CountOrig AS INT
   FIELD CountAfter AS INT
   FIELD AmountOrig AS DEC
   FIELD AmountAfter AS DEC
   FIELD QtyChanged AS INT
INDEX CDRRerate IS PRIMARY UNIQUE RateCCN ErrorCode. 

DEF VAR ldaReratedLast AS DATE NO-UNDO. 
DEF VAR ldaReratedFirst AS DATE NO-UNDO. 

FUNCTION fRerateLogStart RETURNS INT
  (icUserCode AS CHAR,
   idtFrom AS DATE,
   idtTo AS DATE,
   icCLI AS CHAR,
   iiCustNumFrom AS INT,
   iiCustNumTo AS INT,
   icCLIType AS CHAR,
   iiErrorCode AS INT,
   icInvGroup AS CHAR,
   icInvRunCode AS CHAR):

   DEF VAR liRerateID AS INTEGER NO-UNDO.
   DEF VAR lcReport AS CHARACTER NO-UNDO. 
   DEF VAR lcStarted AS CHARACTER NO-UNDO. 
   DEF VAR lcSep AS CHARACTER NO-UNDO INIT ";".
   
   ASSIGN
      liRerateID = NEXT-VALUE(RerateSeq).
      lcStarted = fTS2HMS(fMakeTS()).

   lcReport = 
      "Re-rate report" + CHR(10) + CHR(10) + 
      "Re-rate run details:" + CHR(10) + 
      "Re-rate run ID" + lcSep + STRING(liRerateID) + CHR(10) +
      "User ID" + lcSep + icUserCode + CHR(10) + 
      "Started" + lcSep + ENTRY(1, lcStarted, " ") + lcSep + 
         ENTRY(2, lcStarted, " ") + CHR(10) + 
      "Ended" + lcSep + "#ENDDATE" + lcSep + "#ENDTIME" + CHR(10) + 
      CHR(10) + 
      "Re-rated CDRs:" + CHR(10) + 
      "Call dates" + lcSep + 
      (IF idtFrom NE ? THEN STRING(idtFrom,"99.99.9999") ELSE "") + lcSep + 
      (IF idtTo NE ? THEN STRING(idtTo,"99.99.9999") ELSE "") + CHR(10) +
      "Re-rated call dates" + lcSep + 
         "#RERATEFROM" + lcSep + "#RERATETO" + CHR(10) +
      "Invoice group" + lcSep + icInvGroup + CHR(10) +
      "Customer Number" + lcSep + 
         (IF iiCustNumFrom > 0 THEN STRING(iiCustnumFrom) ELSE "") + lcSep + 
         (IF iiCustnumTo > 0 THEN STRING(iiCustnumTo) ELSE "") + CHR(10) + 
      "Invoice run code" + lcSep + icInvRunCode + CHR(10) +
      "CLI Type" + lcSep + icCliType + CHR(10) +
      "CLI" + lcSep + icCli + CHR(10) +
      "Error Code" + lcSep + 
         (IF iiErrorCode NE 0 THEN STRING(iiErrorCode) ELSE "") + CHR(10).

   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand = gcBrand
      ActionLog.UserCode = icUserCode
      ActionLog.ActionID = "Rerate"
      ActionLog.ActionStatus = 0
      ActionLog.TableName = "MobCDR"
      ActionLog.KeyValue = STRING(liRerateID)
      ActionLog.ActionChar = lcReport
      ActionLog.FromDate = idtFrom
      ActionLog.ToDate = idtTo.
   
   ActionLog.ActionTS = fMakeTS().
   
   RELEASE ActionLog.

   RETURN liRerateID.

END FUNCTION. 

FUNCTION fRerateReportFile RETURNS INT 
   (iiRerateId AS INTEGER):
   
   DEF VAR lcReportDir AS CHARACTER NO-UNDO.
   DEF VAR lcReportFile AS CHARACTER NO-UNDO.
   
   FIND ActionLog EXCLUSIVE-LOCK WHERE
        ActionLog.Brand = gcBrand AND
        ActionLog.TableName = "MobCDR" AND
        ActionLog.KeyValue = STRING(iiRerateID) AND
        ActionLog.ActionId = "Rerate" NO-ERROR.
   
   IF NOT AVAIL ActionLog THEN RETURN 1.
   
   lcReportDir = fCparam("DUMPOUTGOING","cdr_rerate.i").
   lcReportFile = "rerate_" + FILL("0", 5 - LENGTH(ActionLog.KeyValue)) + 
      ActionLog.KeyValue + "_" + ActionLog.UserCode + ".csv".

   /* Dump report to file */
   IF lcReportDir NE ? THEN DO:
      OUTPUT STREAM sRerateRep TO VALUE(lcReportDir + lcReportFile).
      PUT STREAM sRerateRep UNFORMATTED ActionLog.ActionChar.
      OUTPUT STREAM sRerateRep CLOSE.
   END.

   RETURN 0.

END FUNCTION. 

FUNCTION fRerateLogFinish RETURNS INT 
  (iiRerateID AS INT):
   
   DEF VAR lcReport AS CHARACTER NO-UNDO.
   DEF VAR lcSep AS CHARACTER NO-UNDO INIT ";".
   DEF VAR lcEnded AS CHARACTER NO-UNDO. 

   FIND ActionLog EXCLUSIVE-LOCK WHERE
        ActionLog.Brand = gcBrand AND
        ActionLog.TableName = "MobCDR" AND
        ActionLog.KeyValue = STRING(iiRerateID) AND
        ActionLog.ActionId = "Rerate" NO-ERROR.

   IF NOT AVAIL ActionLog THEN RETURN 1.
   
   ASSIGN
      lcEnded = fTS2HMS(fMakeTS())
      ActionLog.ActionChar =
         REPLACE(ActionLog.ActionChar, "#ENDDATE", ENTRY(1, lcEnded, " "))
      ActionLog.ActionChar = 
         REPLACE(ActionLog.ActionChar, "#ENDTIME", ENTRY(2, lcEnded, " "))
      ActionLog.ActionChar = 
         REPLACE(ActionLog.ActionChar, "#RERATEFROM",
         (IF ldaReratedFirst NE ? THEN STRING(ldaReratedFirst,"99.99.9999") 
          ELSE ""))
      ActionLog.ActionChar = 
         REPLACE(ActionLog.ActionChar, "#RERATETO",
         (IF ldaReratedLast NE ? THEN STRING(ldaReratedLast,"99.99.9999") 
          ELSE "")).

   /* Generate formatted report */

   lcReport = /* 1st row */ 
      CHR(10) + FILL(lcSep,2) + 
      "Before:" + FILL(lcSep,2) + 
      "After:" + FILL(lcSep,2) +
      "Change:" + CHR(10) + 
      /* 2nd row */ 
      "Call case" + lcSep +
      "Error code" + lcSep +
      "CDRs" + lcSep + "Amount (Eur)" + lcSep + 
      "CDRs" + lcSep + "Amount (Eur)" + lcSep + 
      "CDRs" + lcSep + "Amount (Eur)" + lcSep + 
      "Qty changed" + CHR(10). 

   /* data rows */
   FOR EACH ttRerateRep NO-LOCK:

      accumulate ttRerateRep.CountOrig (total).
      accumulate ttRerateRep.AmountOrig (total).
      accumulate ttRerateRep.CountAfter (total).
      accumulate ttRerateRep.AmountAfter (total).
      ACCUMULATE ttRerateRep.QtyChanged (TOTAL).

      lcReport = lcReport + 
         STRING(ttRerateRep.RateCCN) + lcSep +
         STRING(ttRerateRep.ErrorCode) + lcSep + 
         STRING(ttRerateRep.CountOrig) + lcSep +
         REPLACE(STRING(ttRerateRep.AmountOrig),".",",") + lcSep + 
         STRING(ttRerateRep.CountAfter) + lcSep +
         REPLACE(STRING(ttRerateRep.AmountAfter),".",",") + lcSep + 
         STRING(ttRerateRep.CountAfter - ttRerateRep.CountOrig) + lcSep +
         REPLACE(STRING(ttRerateRep.AmountAfter - 
                        ttRerateRep.AmountOrig),".",",") + lcSep +
         STRING(ttRerateRep.QtyChanged) + CHR(10).
   END.
   
   /* accumulated total values */
   lcReport = lcReport + "Total" + FILL(lcSep,2) +
      STRING((accum total ttRerateRep.CountOrig)) + lcSep +
      REPLACE(STRING((accum total ttRerateRep.AmountOrig)),".",",") + lcSep + 
      STRING((accum total ttRerateRep.CountAfter)) + lcSep + 
      REPLACE(STRING((accum total ttRerateRep.AmountAfter)),".",",") + lcSep + 
      STRING((accum total ttRerateRep.CountAfter) - 
             (accum total ttRerateRep.CountOrig)) + lcSep +  
      REPLACE(STRING((accum total ttRerateRep.AmountAfter) - 
             (accum total ttRerateRep.AmountOrig)),".",",") + lcSep + 
      STRING(ACCUM TOTAL ttRerateRep.QtyChanged) + CHR(10).  

   ASSIGN
      ActionLog.ActionStatus = 2
      ActionLog.ActionChar = ActionLog.ActionChar + lcReport.

   FIND CURRENT ActionLog NO-LOCK.

   fRerateReportFile(iiRerateID).
         
   RETURN 0.

END FUNCTION. 

FUNCTION fRerateCDRBefore RETURNS LOGICAL
   (iiRateCCN AS INT,
    iiErrorCode AS INT,
    ideAmount AS DEC,
    idaCallDate AS DATE):

   FIND ttRerateRep WHERE
        ttRerateRep.RateCCN = iiRateCCN AND
        ttRerateRep.ErrorCode = iiErrorCode EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAIL ttRerateRep THEN DO:
      CREATE ttRerateRep.
      ASSIGN
         ttRerateRep.RateCCN = iiRateCCN
         ttRerateRep.ErrorCode = iiErrorCode.
   END.

   IF ldaReratedFirst = ? THEN ASSIGN
      ldaReratedFirst = idaCallDate
      ldaReratedLast = idaCallDate.

   ASSIGN
      ttRerateRep.CountOrig = ttRerateRep.CountOrig + 1
      ttRerateRep.AmountOrig = ttRerateRep.AmountOrig + ideAmount
      ldaReratedFirst = idaCallDate WHEN idaCallDate < ldaReratedFirst
      ldaReratedLast = idaCallDate WHEN idaCallDate > ldaReratedLast.

   RELEASE ttRerateRep.
   
END FUNCTION. 

FUNCTION fRerateCDRAfter RETURNS LOGICAL
   (iiRateCCN AS INT,
    iiErrorCode AS INT,
    ideAmount AS DEC,
    ilChanged AS LOG):

   FIND ttRerateRep WHERE
        ttRerateRep.RateCCN = iiRateCCN AND
        ttRerateRep.ErrorCode = iiErrorCode EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAIL ttRerateRep THEN DO:
      CREATE ttRerateRep.
      ASSIGN
         ttRerateRep.RateCCN = iiRateCCN
         ttRerateRep.ErrorCode = iiErrorCode.
   END.

   ASSIGN
      ttRerateRep.CountAfter = ttRerateRep.CountAfter + 1
      ttRerateRep.AmountAfter = ttRerateRep.AmountAfter + ideAmount
      ttRerateRep.QtyChanged  = ttRerateRep.QtyChanged + INTEGER(ilChanged).

   RELEASE ttRerateRep.
   
END FUNCTION. 

&ENDIF

