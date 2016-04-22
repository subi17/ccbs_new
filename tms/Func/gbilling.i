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



DEF STREAM Sout.
DEF STREAM SHRLP.




FUNCTION fInitGBParameters RETURNS CHAR
   ():
   ASSIGN

      lcGBOutDir = fCParam("GBILLING","GBOutDir")
      lcGBInDir = fCParam("GBILLING","GBListInDir")
      lcGBLogDir = fCParam("GBILLING","GBLogDir")
      lcGBSpoolDir = fCParam("GBILLING","GBSpoolDir").

 
   IF lcGBOutDir  EQ "" OR lcGBOutDir EQ ? THEN lcGBOutDir = "/tmp/".
   IF lcGBInDir  EQ "" OR lcGBInDir EQ ? THEN lcGBInDir = "/tmp/".
   IF lcGBLogDir  EQ "" OR lcGBLogDir EQ ? THEN lcGBLogDir = "/tmp/".
   IF lcGBSpoolDir  EQ "" OR lcGBSpoolDir EQ ? THEN lcGBSpoolDir = "/tmp/".

END.   


FUNCTION fGetPeriod RETURNS INT
   (idtDate AS DATETIME):
   DEF VAR liPeriod AS INT NO-UNDO.
   liPeriod = YEAR(idtDate) * 100 + MONTH(idtDate).

   RETURN liPeriod.
END.


FUNCTION fProcessPostpaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    idtPDate AS DATETIME, /*Purchase date*/
    ideAmount AS DECIMAL): /*Amount*/

   DEF BUFFER bMobsub FOR MobSub.
   DEF VAR lcResponse AS CHAR NO-UNDO.
   lcResponse = {&GB_RESP_OK}.
   
   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.Brand EQ "1" AND
              bMobSub.CLI EQ icMSISDN NO-ERROR.
   IF NOT AVAIL bMobSub THEN RETURN {&GB_RESP_NO_SUBS}.           

   RUN creafat (MobSub.CustNum, /* custnum */
                MobSub.MsSeq, /* msseq */
                "DRAFT_GBFAT", /*NOK*/
                ideAmount,   /* amount */ 
                0,   /* percentage  */
                ?,   /* vat included already */
                fGetPeriod(idtPDate), /*period*/
                999999, /*tp period, no limoit now*/
                OUTPUT lcResponse). /* error */

   RETURN lcResponse.
END.


FUNCTION fProcessPrepaidEntry RETURNS CHAR
   (icMSISDN AS CHAR, /*MSISDN*/
    icCorrId AS CHAR, /*Correlation ID*/
    idtPDate AS DATETIME, /*Purchase date*/
    ideAmount AS DECIMAL): /*Amount*/

   DEF BUFFER bMobsub FOR MobSub.
   DEF BUFFER bMsOwner FOR MsOwner.
   DEF BUFFER bCustomer FOR Customer.

   DEF VAR lcResponse AS CHAR NO-UNDO.
   DEF VAR liRequest AS INT NO-UNDO.
   DEF VAR lcTaxZone AS CHAR NO-UNDO.

   lcResponse = {&GB_RESP_OK}.
   
   FIND FIRST bMobSub NO-LOCK WHERE
              bMobSub.Brand EQ "1" AND
              bMobSub.CLI EQ icMSISDN NO-ERROR.

   IF NOT AVAIL bMobSub THEN RETURN {&GB_RESP_NO_SUBS}.           

   FIND FIRST bMsOwner WHERE 
              bMsOwner.CLI EQ icMSISDN AND 
              bMsOwner.paytype EQ TRUE NO-LOCK NO-ERROR.

   IF NOT AVAIL bMsOwner THEN RETURN {&GB_RESP_NO_SUBS}.

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


END.


