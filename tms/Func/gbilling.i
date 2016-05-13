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

FUNCTION fProcessPostpaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    icPeriod AS CHAR, /*period info*/
    ideAmount AS DECIMAL,
    BUFFER bMobSub FOR MobSub, /*Amount*/
    OUTPUT ocErrInfo AS CHAR):

   DEF VAR lcResponse AS CHAR NO-UNDO.
   
   RUN creafat (bMobSub.CustNum, /* custnum */
                bMobSub.MsSeq, /* msseq */
                "GOOGLEVASFAT", /*NOK*/
                ideAmount,   /* amount */ 
                0,   /* percentage  */
                ?,   /* vat included already */
                icPeriod, /*period*/
                999999, /*tp period, no limoit now*/
                OUTPUT lcResponse). /* error */
   lcresponse = TRIM(lcResponse).             
   IF lcResponse NE "" THEN DO:
      ocErrInfo = lcResponse.
      lcResponse = {&GB_POSTPAID_FAT_FAILURE}.
   END.
   ELSE
      lcResponse = {&GB_RESP_OK}.
   RETURN lcResponse.
END.


FUNCTION fProcessPrepaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    ideAmount AS DECIMAL, /*Amount*/
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

   liRequest = fCreateTopUpRequest(MobSub.MsSeq,     /* msseq */
                                   MobSub.Cli,       /* cli */
                                   "RefillTRequest", /* function */
                                   "GBRefund",       /* source*/ 
                                   "RefillTRequest", /* request*/
                                   "990",            /* prefix */
                                   "Cron script",    /* reference */
                                   lcTaxzone,        /* taxzone */
                                   0,                /* actstamp */
                                   ideAmount,        /* topupamount*/
                                   0.0).             /* vatamount*/

   IF liRequest EQ 0 OR liRequest EQ ? THEN RETURN {&GB_RESP_REQUEST_ERROR}.
   ELSE RETURN {&GB_RESP_OK}.
END.

FUNCTION fProcessGBEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    icPeriod AS CHAR, /*Purchase date*/
    ideAmount AS DECIMAL, /*Amount*/
    ilgPayType AS LOGICAL, /*p*/
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
   IF bMsOwner.PayType NE ilgPayType THEN 
      lcErr = {&GB_INCORRECT_PAYTYPE}.

   IF bMsOwner.paytype THEN DO:
      lcResponse = fProcessPrepaidEntry(icMSISDN, 
                                        icCorrId, 
                                        ideAmount,
                                        BUFFER bMsOwner).
   END.
   ELSE DO:
      lcResponse = fProcessPostpaidEntry(icMSISDN, 
                                         icCorrId, 
                                         icPeriod, 
                                         ideAmount,
                                         BUFFER bMobSub,
                                         OUTPUT ocErrInfo).                                   
      
   END.
   IF lcErr NE {&GB_RESP_OK} THEN lcResponse = lcResponse + ";" + lcErr.
   RETURN lcResponse.
END.
