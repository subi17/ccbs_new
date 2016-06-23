/* ----------------------------------------------------------------------
  MODULE .......: gbilling.i
  TASK .........: Functions for Google Billing needs
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......: 
  CREATED ......: 19.11.15
  CHANGED ......: 
  ------------------------------------------------------------------------*/

{commali.i}
{timestamp.i}
{cparam2.i}
{fgettxt.i}
{date.i}
{fduedate.i}
{ftransdir.i}
{tmsconst.i}
{ftaxdata.i}
{ftopup.i}
{fcreditreq.i}
{fcpfat.i}

DEF VAR lcGBOutDir AS CHAR NO-UNDO.
DEF VAR lcGBInDir AS CHAR NO-UNDO.
DEF VAR lcGBLogDir AS CHAR NO-UNDO.
DEF VAR lcGBSpoolDir AS CHAR NO-UNDO.
DEF VAR lcGBProcessedDir AS CHAR NO-UNDO.

DEF STREAM Sout.

FUNCTION fInitGBParameters RETURNS CHAR
   ():
   ASSIGN
      lcGBOutDir = fCParam("GBILLING","GBOutDir")
      lcGBInDir = fCParam("GBILLING","GBInDir")
      lcGBProcessedDir = fCParam("GBILLING","GBProcDir")
      lcGBLogDir = fCParam("GBILLING","GBLogDir")
      lcGBSpoolDir = fCParam("GBILLING","GBSpoolDir").
 
   IF lcGBOutDir  EQ "" OR lcGBOutDir EQ ? THEN lcGBOutDir = "/tmp/".
   IF lcGBInDir  EQ "" OR lcGBInDir EQ ? THEN lcGBInDir = "/tmp/".
   IF lcGBLogDir  EQ "" OR lcGBLogDir EQ ? THEN lcGBLogDir = "/tmp/".
   IF lcGBSpoolDir  EQ "" OR lcGBSpoolDir EQ ? THEN lcGBSpoolDir = "/tmp/".

END.   

/*function checks that is requeasted refund already billed.*/
/*This is checked only when we are checking older thatn current period cases.*/
/*Idea:
   Find MobCDR.
   By MobCDR find invseq.
   By invseq check if billed:
   NO -> Return FALSE
   YES -> Find invoice where invseq is same and check if it is not test invoice
   If test invoice 99 -> Return FALSE.
   If invoice is not test invoice -> TRUE.
   */
FUNCTION fIsBilled RETURNS LOGICAL
   (icCLI AS CHAR,
    icTimeInfo AS CHAR,
    OUTPUT oiInvoiceNum AS INT):
   DEF VAR liSeconds AS INT NO-UNDO.
   DEF VAR lcTime AS CHAR NO-UNDO.
   DEF VAR ldaDate AS DATE NO-UNDO.
   DEF BUFFER bMobCdr FOR MobCdr.
   DEF BUFFER bInvSeq FOR InvSeq.
   DEF BUFFER bInvoice FOR Invoice.
   ASSIGN
      ldaDate = DATE(INT(SUBSTR(icTimeInfo,6,2)),
                     INT(SUBSTR(icTimeInfo,9,2)),
                     INT(SUBSTR(icTimeInfo,1,4)))
      lcTime  = SUBSTR(icTimeInfo,12,8)
      liSeconds  = INT(ENTRY(1,lcTime,":")) * 3600 +
                INT(ENTRY(2,lcTime,":")) * 60   +
                INT(ENTRY(3,lcTime,":"))NO-ERROR.

   FIND FIRST bMobCdr WHERE
              bMobCdr.CLI EQ icCLI AND
              bMobCdr.DateSt EQ ldaDate AND
              bMobCDR.TimeStart EQ liSeconds NO-ERROR.
   IF AVAIL bMobCDR THEN DO:
      FIND FIRST bInvSeq NO-LOCK WHERE
                 bInvSeq.InvSeq EQ bMobCdr.InvSeq AND
                 bInvSeq.Billed EQ TRUE NO-ERROR.
      IF AVAIL bInvSeq THEN DO:
         FIND FIRST bInvoice NO-LOCK WHERE
                    bInvoice.InvNum EQ bInvSeq.InvNum AND
                    bInvoice.InvType NE {&INV_TYPE_TEST} NO-ERROR.
         IF AVAIL bInvoice THEN DO:
            oiInvoiceNum = bInvoice.InvNum.
            RETURN TRUE.
         END.   
      END.
   END.

   RETURN FALSE.
END.


FUNCTION fProcessPostpaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    icTimeInfo AS CHAR, /*period info*/
    icCurrentPeriod AS CHAR, /*Period of program starting moment*/
    ideAmount AS DECIMAL,
    icRefId AS CHAR,
    BUFFER bMobSub FOR MobSub, /*Amount*/
    OUTPUT ocErrInfo AS CHAR):

   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcPeriod AS CHAR NO-UNDO.
   DEF VAR llgBilled AS LOGICAL NO-UNDO.
   DEF VAR liInvNum AS INT NO-UNDO.
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR lcMemoText AS CHAR NO-UNDO.

   lcPeriod = REPLACE(SUBSTRING(icTimeInfo,1,7),"-","").  
     /*different perios, needs special handling:*/
     /*If billed -> Credit Note*/
     /*If not billed -> FAT*/
   llgBilled = fIsBilled(icMSISDN, icTimeInfo, OUTPUT liInvNum).
   IF lcPeriod EQ icCurrentPeriod OR llgBilled EQ FALSE THEN DO:
     lcMemoText = "Google refund FAT, id: " + icRefId.
     ocErrInfo =  fCreateFatRow(
                             "GOOGLEVASFAT",
                             bMobSub.CustNum,
                             bMobSub.MsSeq,
                             icMSISDN,
                             "", /*host table*/
                             "", /*key value*/
                             ideAmount,
                             0, /* percentage  */
                             ?, /* VAT included */
                             INT(lcPeriod), /*from period*/
                             999999, /*to period*/
                             lcMemoText).

      ocErrInfo = TRIM(ocErrInfo).             
      IF ocErrInfo NE "" THEN DO:
         lcResponse = {&GB_POSTPAID_FAT_FAILURE}.
      END.
      ELSE
         lcResponse = {&GB_RESP_OK}.
      RETURN lcResponse.
   END.
   ELSE DO:
      /*If billed -> credit note*/
      /*FIND FIRST invoice...*/
      liRequest = 0.
      ocErrInfo = "Credit Note not allowed".

      FOR FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum EQ liInvNum,                   
          FIRST SubInvoice NO-LOCK WHERE
                Subinvoice.InvNum EQ Invoice.InvNum AND
                SubInvoice.CLI EQ icMSISDN,
           EACH InvRow NO-LOCK WHERE
                InvRow.InvNum EQ Invoice.InvNum AND
                InvRow.SubInvNum EQ SubInvoice.SubInvNum AND
                InvRow.BillCode EQ "GOOGLEVAS" AND
                InvRow.CreditInvNum = 0 AND /*No CN already done*/
                InvRow.Amt >= ideAmount:


         liRequest = fFullCreditNote(Invoice.InvNum,
                                STRING(SubInvoice.SubInvNum),
                                "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                "InvRowAmt=" + 
                                   REPLACE(STRING(ideAmount),",","."),
                                "Correct", /*reason group*/
                                "2013", /*reason*/
                                "", /*reason note*/
                                OUTPUT ocErrInfo).
         IF liRequest = 0 THEN DO:
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "MobSub",
                             STRING(bMobSub.MsSeq),
                             bMobSub.Custnum,
                             "CREDIT NOTE CREATION FAILED",
                             "ERROR:" + ocErrInfo).
         END.
 
         LEAVE.
      END.
      IF ocErrInfo NE ""  OR liRequest EQ 0 THEN DO:
         lcResponse = {&GB_POSTPAID_CN_FAILURE}.
      END.
      ELSE
         lcResponse = {&GB_RESP_OK}.
      RETURN lcResponse.

   END.

END.

FUNCTION fProcessPrepaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    iiMsSeq AS INT, /*MsSeq*/
    icCorrId AS CHAR, /*Correlation ID*/
    ideAmount AS DECIMAL, /*Amount*/
    icRefId AS CHAR, /*TransactionID*/
    BUFFER bMsOwner FOR MsOwner):

   DEF BUFFER bCustomer FOR Customer.

   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR lcTaxZone AS CHAR NO-UNDO.

   lcResponse = {&GB_RESP_OK}.
   
   FIND FIRST bCustomer WHERE 
              bCustomer.Custnum EQ bMsOwner.InvCust NO-LOCK NO-ERROR.
   IF AVAIL bCustomer THEN lcTaxZone = fRegionTaxZone(bCustomer.Region).
   ELSE lcTaxZone = "1".
   
   liRequest = fCreateTopUpRequest(iiMsSeq,          /* msseq */
                                   icMSISDN,         /* cli */
                                   "RefillTRequest", /* function */
                                   "GBRefund",       /* source*/ 
                                   "RefillTRequest", /* request*/
                                   "990",            /* prefix */
                                   icRefId,          /* reference */
                                   lcTaxzone,        /* taxzone */
                                   0,                /* actstamp */
                                   ideAmount * 100,  /* topupamount*/
                                   0.0).             /* vatamount*/

   IF liRequest EQ 0 OR liRequest EQ ? THEN RETURN {&GB_RESP_REQUEST_ERROR}.
   ELSE RETURN {&GB_RESP_OK}.
END.

FUNCTION fProcessGBEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    icTimeInfo AS CHAR, /*Purchase date*/
    icCurrentPeriod AS CHAR, /*Period of "NOW"*/
    ideAmount AS DECIMAL, /*Amount*/
    ilgPayType AS LOGICAL, /*paytype*/
    icRefId AS CHAR, /*Reference ID (transactionid)*/
    OUTPUT ocErrInfo AS CHAR):

   DEF BUFFER bMobsub FOR MobSub.
   DEF BUFFER bMsOwner FOR MsOwner.
   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR lcErr AS CHAR NO-UNDO.

   lcErr = {&GB_RESP_OK}.

   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.Brand EQ "1" AND
              bMobSub.CLI EQ icMSISDN NO-ERROR.

   IF NOT AVAIL bMobSub THEN RETURN {&GB_RESP_NO_SUBS}.

   FIND FIRST bMsOwner WHERE
              bMsOwner.CLI EQ icMSISDN NO-LOCK NO-ERROR.

   IF NOT AVAIL bMsOwner THEN RETURN {&GB_RESP_NO_SUBS}.
   IF bMobsub.PayType NE ilgPayType THEN 
      lcErr = {&GB_INCORRECT_PAYTYPE}.

   IF bMobSub.paytype THEN DO:
      lcResponse = fProcessPrepaidEntry(icMSISDN,
                                        bMobSub.MsSeq,
                                        icCorrId, 
                                        ideAmount,
                                        icRefId,
                                        BUFFER bMsOwner).
   END.
   ELSE DO:
      lcResponse = fProcessPostpaidEntry(icMSISDN, 
                                         icCorrId, 
                                         icTimeInfo, 
                                         icCurrentPeriod,
                                         ideAmount,
                                         icRefId,
                                         BUFFER bMobSub,
                                         OUTPUT ocErrInfo).                                   
      
   END.
   IF lcErr NE {&GB_RESP_OK} THEN lcResponse = lcResponse + ";" + lcErr.
   RETURN lcResponse.
END.

