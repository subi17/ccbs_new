
/*

- STC from postpaid data subscription to another postpaid data subscription (CONTRD1 to CONTRD2 or vice versa) 
- STC date is 1.1., 1.2., .1.3. or 1.4.2010 
- Subscription has data usage on STC date before actual STC has taken place 
- Data usage billing item is 14100001 (not included into IPL data package) 

Report details: 
Customer number, MSISDN, STC date, old subs type, new subs type, billig item, number of CDRs, euro amount, invoice number (if invoiced)

*/

{timestamp.i}
{tmsconst.i}

DEF VAR ldtInputDate AS DATE NO-UNDO. 
DEF VAR ldBeginStamp AS DEC NO-UNDO. 
DEF VAR ldEndStamp AS DEC NO-UNDO. 
DEF VAR gcBrand AS CHAR NO-UNDO INITIAL "1". 
DEF VAR liQty AS INT NO-UNDO. 
DEF VAR ldAmt As DEC NO-UNDO.
DEF VAR lcInvNum AS CHAR NO-UNDO. 
DEF VAR lcBillCode AS CHAR NO-UNDO INITIAL "14100001". 
DEF STREAM sLog.

FUNCTION fReadCDRs RETURNS LOGICAL 
   (INPUT pcCLI AS CHAR,
    INPUT pdtCDR AS DATE,
    INPUT pdStartTS AS DEC,
    INPUT pdEndTS AS DEC,
    OUTPUT piQty AS INT,
    OUTPUT pdAmt AS DEC,
    OUTPUT pcInvNum AS CHAR):
   
   DEFINE VARIABLE ldeTime AS DECIMAL NO-UNDO.

   piQty = 0. 
   pdAmt = 0.
   FOR EACH MobCDR NO-LOCK WHERE
            MobCDR.CLI = pcCLI AND
            MobCDR.DateSt = pdtCDR:

       IF Mobcdr.errorcode NE 0        THEN NEXT.
       IF MobCDR.BillCode NE "14100001" THEN NEXT. 

       ldeTime  = fMake2Dt(Mobcdr.datest, Mobcdr.TimeStart).
/*       IF Mobcdr.ReadInTS < pdStartTS THEN NEXT.
       IF Mobcdr.ReadInTS > pdEndTS   THEN NEXT. */

       IF ldeTime < pdStartTS THEN NEXT.
       IF ldeTime > pdEndTS   THEN NEXT. 
       
       piQty = piQty + 1 .
       pdAmt = pdAmt + MobCDR.Amount. 
       IF MobCDR.InvSeq NE 0 THEN DO:
          find invseq where
               invseq.invseq = MobCDR.invseq NO-LOCK.
          FIND LAST Invoice WHERE
                    Invoice.invnum = invseq.invnum NO-LOCK NO-ERROR. 
          IF AVAIL Invoice THEN pcInvNum = Invoice.ExtInvID. 
       END.
   
   END.
 
   RETURN TRUE.
END FUNCTION. 


ldtInputDate = DATE(11,1,2011).
ldBeginStamp = fHMS2TS(ldtInputDate,"00:00:00").
ldEndStamp = fHMS2TS(ldtInputDate,"23:59:59").
DEFINE VARIABLE ldeLastMonthLastSecond AS DECIMAL NO-UNDO. 
ldeLastMonthLastSecond = fSecOffSet(ldBeginStamp,-1).

OUTPUT STREAM sLog TO VALUE("/apps/yoigo/tms_support/billing/check_1st_day_gprs_rating_" + STRING(YEAR(ldtInputdate) * 10000 
         + 100 * MONTH(ldtInputdate)
         + DAY(ldtInputdate)) + ".log").

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand = gcBrand AND
         MsRequest.ReqType = ({&REQTYPE_SUBSCRIPTION_TYPE_CHANGE}) AND
         MsRequest.ReqStatus = ({&REQUEST_STATUS_DONE}) AND
         MsRequest.ActStamp >= ldBeginStamp AND 
         MSRequest.ActStamp <= ldEndStamp AND 
         LOOKUP(MsRequest.ReqCParam1,"CONTRD,CONTRD1,CONTRD2,CONTRD3,CONT6") > 0 /* AND 
         LOOKUP(MsRequest.ReqCParam2,"CONTRD1,CONTRD2,CONTRD3") > 0 */ :

        liQty = 0.
        ldAmt = 0.
        lcInvNum = "".

        find first msowner where
                   msowner.msseq = msrequest.msseq and
                   msowner.tsbegin < ldBeginStamp and
                   msowner.tsend >= ldeLastMonthLastSecond NO-LOCK no-error.
        IF NOT AVAIL msowner then disp msrequest.msseq.

        fReadCDRs(MsRequest.CLI,
                  ldtInputdate,
                  ldBeginStamp,
                  msowner.tsend, /*  MsRequest.DoneStamp, */
                  OUTPUT liQty,
                  OUTPUT ldAmt,
                  OUTPUT lcInvNum). 

        IF liQty EQ 0 THEN NEXT.  

        PUT STREAM sLog UNFORMATTED
            MsRequest.CustNum ";"
            MsRequest.CLI     ";"
            ldtInputDate      ";"
            MsRequest.ReqCParam1 ";"
            MsRequest.ReqCParam2 ";"
            lcBillCode           ";"
            liQty                ";"
            ldAmt                ";"
            lcInvNum             SKIP.

END.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand = gcBrand AND
         MsRequest.ReqType = 81 AND
         MsRequest.ReqStatus = ({&REQUEST_STATUS_DONE}) AND
         MsRequest.ActStamp >= ldBeginStamp AND 
         MSRequest.ActStamp <= ldEndStamp:

        liQty = 0.
        ldAmt = 0.
        lcInvNum = "".

        fReadCDRs(MsRequest.CLI,
                  ldtInputdate,
                  20010101,
                  20490101, /*  MsRequest.DoneStamp, */
                  OUTPUT liQty,
                  OUTPUT ldAmt,
                  OUTPUT lcInvNum). 

        IF liQty EQ 0 THEN NEXT.  

        PUT STREAM sLog UNFORMATTED
            MsRequest.CustNum ";"
            MsRequest.CLI     ";"
            ldtInputDate      ";"
            MsRequest.ReqCParam1 ";"
            MsRequest.ReqCParam2 ";"
            lcBillCode           ";"
            liQty                ";"
            ldAmt                ";"
            lcInvNum             SKIP.

END.

OUTPUT STREAM sLog CLOSE.
