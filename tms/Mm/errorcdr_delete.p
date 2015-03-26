/* -----------------------------------------------
  MODULE .......: errorcdr_delete.p
  FUNCTION .....: Delete old error cdrs
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 18.10.10
------------------------------------------------------ */

{commali.i}
{timestamp.i}
{cparam2.i}

DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
DEF OUTPUT PARAMETER oiQty       AS INT  NO-UNDO.

DEF VAR ldtStarted  AS DATETIME NO-UNDO.
DEF VAR ldaFrom     AS DATE NO-UNDO.
DEF VAR ldaTo       AS DATE NO-UNDO. 
DEF VAR liTimeLimit AS INT  NO-UNDO.                
DEF VAR lcErrorList AS CHAR NO-UNDO.
DEF VAR lcKeyValue  AS CHAR NO-UNDO.                
DEF VAR ldaDate     AS DATE NO-UNDO.

DEF BUFFER bErrorCDR FOR ErrorCDR.

IF idaFromDate = ? OR idaToDate = ? OR idaToDate > TODAY - 30 THEN 
   RETURN "ERROR:Invalid period".

ASSIGN 
   ldtStarted  = NOW
   liTimeLimit = fCParamI("ErrorCDRDelTime")
   lcErrorList = fCParamC("ErrorCDRDelCodes").
   
IF liTimeLimit = ? THEN liTimeLimit = 120.

IF liTimeLimit > 0 AND DAY(TODAY) NE 1 THEN RUN pDelete.

RETURN "".


PROCEDURE pDelete:

   FOR EACH ErrorCDR NO-LOCK WHERE
            ErrorCDR.DateSt >= idaFromDate AND
            ErrorCDR.DateSt < idaToDate AND
            LOOKUP(STRING(ErrorCDR.ErrorCode),lcErrorList) > 0 
   TRANS:
         
      FIND bErrorCDR WHERE RECID(bErrorCDR) = RECID(ErrorCDR) 
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF LOCKED(bErrorCDR) THEN NEXT.
      
      IF ldaFrom = ? THEN ASSIGN
         ldaFrom = ErrorCDR.DateSt
         ldaTo   = ErrorCDR.DateSt.
     
      ELSE ASSIGN
         ldaFrom = MIN(ldaFrom,ErrorCDR.DateSt)
         ldaTo   = MAX(ldaTo,ErrorCDR.DateSt).
      
      FIND FIRST ErrorDtl WHERE
                 ErrorDtl.DateSt = bErrorCDR.DateSt AND
                 ErrorDtl.DtlSeq = bErrorCDR.DtlSeq EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE ErrorDtl THEN DELETE ErrorDtl.

      DELETE bErrorCDR.
   
      oiQty = oiQty + 1.
   
      IF oiQty MOD 100 = 0 THEN DO:
         /* process only a limited time on one day */
         IF INTERVAL(NOW,ldtStarted,"minutes") > liTimeLimit THEN LEAVE.
      END.
   END.

   DO TRANS:
      CREATE ActionLog.
      ASSIGN 
         ActionLog.Brand        = gcBrand   
         ActionLog.TableName    = "ErrorCDR"  
         ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                                  STRING(MONTH(TODAY),"99")  +
                                  STRING(DAY(TODAY),"99")
         ActionLog.ActionID     = "DELETECDR"
         ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                                  MONTH(TODAY)
         ActionLog.ActionDec    = oiQty
         ActionLog.ActionChar   = STRING(oiQty) + 
                                  " ErrorCDRs dated before " + 
                                  STRING(idaToDate,"99.99.9999") +
                                  " (codes " + lcErrorList + 
                                  ") were deleted." + CHR(10) +
                                  "Started at " + 
                                  REPLACE(STRING(ldtStarted),"/","-")
         ActionLog.ActionStatus = 3
         ActionLog.UserCode     = katun
         ActionLog.FromDate     = ldaFrom
         ActionLog.ToDate       = ldaTo.
         ActionLog.ActionTS     = fMakeTS().
   END.

END PROCEDURE.



