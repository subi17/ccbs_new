/* conv_invrowcounter.p   05.11.10/aam
*/

{Syst/commali.i}
{Func/callquery.i}

DEF INPUT  PARAMETER iiInvSeq  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiEvents  AS INT  NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

DEF VAR liDeleted AS INT  NO-UNDO.
 

/***** Main start *******/

FIND FIRST InvSeq WHERE InvSeq.InvSeq = iiInvSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE InvSeq THEN RETURN "ERROR:Unknown InvSeq".
 
RUN pCollectCDRs.
IF RETURN-VALUE BEGINS "ERROR" THEN
   RETURN RETURN-VALUE.

RUN pDeleteCurrentCounters.

RUN pAccumulateCounters.
IF RETURN-VALUE BEGINS "ERROR" THEN
   RETURN RETURN-VALUE.

RETURN "".

/***** Main end ********/


PROCEDURE pDeleteCurrentCounters:
      
   FOR EACH InvRowCounter EXCLUSIVE-LOCK WHERE
            InvRowCounter.InvCust = InvSeq.CustNum AND
            InvRowCounter.InvSeq  = InvSeq.InvSeq AND
            InvRowCounter.InvNum  = 0:
               
      liDeleted = liDeleted + 1.
      DELETE InvRowCounter.
   END.           

END PROCEDURE.

PROCEDURE pCollectCDRs:

   DEF VAR liErrorCodeOut AS INT  NO-UNDO.
   DEF VAR liInvCust      AS INT  NO-UNDO.
   DEF VAR lcCLI          AS CHAR NO-UNDO.
   DEF VAR ldaFromDate    AS DATE NO-UNDO.
   DEF VAR ldaToDate      AS DATE NO-UNDO.
   DEF VAR ldBegStamp     AS DEC  NO-UNDO.
   DEF VAR ldEndStamp     AS DEC  NO-UNDO.
   
   DEF VAR tthCDR AS HANDLE NO-UNDO.

   tthCDR = TEMP-TABLE ttCall:HANDLE.

   EMPTY TEMP-TABLE ttCall.
  
   fMobCDRCollect(INPUT "post",
                  INPUT Syst.CUICommon:gcBrand,
                  INPUT katun,
                  INPUT InvSeq.FromDate,   
                  INPUT InvSeq.ToDate,
                  INPUT 0,
                  INPUT "inv",
                  INPUT "",
                  INPUT iiInvSeq,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liErrorCodeOut,
                  INPUT-OUTPUT tthCDR).
   IF liErrorCodeOut > 0 THEN
      RETURN "ERROR:EDR collection failed on error " + STRING(liErrorCodeOut).

   RETURN "".
   
END PROCEDURE.

PROCEDURE pAccumulateCounters:

   DEF VAR ldaToDate AS DATE NO-UNDO.
   DEF VAR liLockCnt AS INT  NO-UNDO.
   DEF VAR liSign    AS INT  NO-UNDO.
   DEF VAR lcReportingID AS CHAR NO-UNDO.
   DEF VAR liQty AS INT NO-UNDO.

   FOR EACH ttCall:
   
      liQty = liQty + 1.
      IF liQty MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP liQty ttCall.CLI ttCall.DateSt WITH ROW 10 1 DOWN.
      END.
      
      /* always on calendar month level */
      IF MONTH(ttCall.DateSt) = 12 THEN 
         ldaToDate = DATE(12,31,YEAR(ttCall.DateSt)).
      ELSE ldaToDate = DATE(MONTH(ttCall.DateSt) + 1,1,
                            YEAR(ttCall.DateSt)) - 1.
   
      lcReportingId = ttCall.ServRid + "," + ttCall.MPMRid.
      
      FIND FIRST InvRowCounter WHERE
                 InvRowCounter.InvCust     = ttCall.InvCust AND
                 InvRowCounter.InvSeq      = ttCall.InvSeq AND
                 InvRowCounter.BillCode    = ttCall.BillCode AND
                 InvRowCounter.CCN         = ttCall.CCN AND
                 InvRowCounter.MsSeq       = ttCall.MsSeq AND
                 InvRowCounter.CLI         = ttCall.CLI AND
                 InvRowCounter.TariffNum   = ttCall.TariffNum AND
                 InvRowCounter.VatIncl     = ttCall.VatIncl AND
                 InvRowCounter.ReportingID = lcReportingID AND
                 InvRowCounter.DCEvent     = ttCall.DCEvent AND
                 InvRowCounter.ToDate      = ldaToDate NO-LOCK NO-ERROR.

      IF NOT AVAILABLE InvRowCounter THEN DO:
         CREATE InvRowCounter.
         ASSIGN 
            InvRowCounter.InvCust     = ttCall.InvCust
            InvRowCounter.InvSeq      = ttCall.InvSeq
            InvRowCounter.BillCode    = ttCall.BillCode 
            InvRowCounter.CCN         = ttCall.CCN 
            InvRowCounter.MsSeq       = ttCall.MsSeq 
            InvRowCounter.CLI         = ttCall.CLI 
            InvRowCounter.TariffNum   = ttCall.TariffNum 
            InvRowCounter.VatIncl     = ttCall.VatIncl 
            InvRowCounter.ReportingID = lcReportingID 
            InvRowCounter.DCEvent     = ttCall.DCEvent
            InvRowCounter.FromDate    = DATE(MONTH(ldaToDate),1,
                                             YEAR(ldaToDate))
            InvRowCounter.ToDate      = ldaToDate
            InvRowCounter.InvNum      = InvSeq.InvNum
            InvRowCounter.SubInvNum   = InvSeq.SubInvNum.
      END.

      ELSE REPEAT:
         FIND CURRENT InvRowCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED(InvRowCounter) THEN DO:
            liLockCnt = liLockCnt + 1. 
            IF liLockCnt > 100 THEN RETURN "ERROR:Counter locked".
            PAUSE 3 NO-MESSAGE. 
         END. 
         ELSE LEAVE.
      END.

      ASSIGN
         InvRowCounter.Quantity = InvRowCounter.Quantity + 1
         InvRowCounter.Duration = InvRowCounter.Duration + ttCall.BillDur 
         InvRowCounter.Amount   = InvRowCounter.Amount + ttCall.Amount
         InvRowCounter.DataAmt  = InvRowCounter.DataAmt +
                                  (ttCall.DataIn + ttCall.DataOut)
         InvRowCounter.RefPrice = InvRowCounter.RefPrice +
                                        ttCall.RefPrice
         InvRowCounter.ExtraAmount = InvRowCounter.ExtraAmount + 
                                        ttCall.MPMAmt
         oiEvents = oiEvents + 1.                                
      
   END.
   
   RETURN "".
   
END PROCEDURE.


