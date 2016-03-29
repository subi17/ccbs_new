/* ----------------------------------------------------------------------
  MODULE .......: smsinvoice.p 
  TASK .........: Handles sms invoice request (type 79), YDR-104 
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 04/2010 
  CHANGE .......: 07/2010 - Different SM text for invoices with many
                            subscriptions. YOT-825
  CHANGE .......: 09/2015 - SMS sending throttling added with pausing YOT-3986
  Version ......: Yoigo
----------------------------------------------------------------------- */
/* Description: smsinvoice is called from newton side where SMS invoice button
   pressed for any customers after billing run. This will generate request 
   that will call this program to create SMS to all invoiced customers.   */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/cparam2.i}
{Func/femailinvoice.i}
{Func/email.i}
{Func/heartbeat.i}

&SCOPED-DEFINE MIDNIGHT-SECONDS 86400

DEF INPUT PARAMETER iiMSrequest AS INT  NO-UNDO.

DEF VAR ldaDateFrom             AS DATE NO-UNDO. 
DEF VAR liMonth                 AS INT  NO-UNDO. 
DEF VAR lcSep                   AS CHAR NO-UNDO. 
DEF VAR lcAddrConfDir           AS CHAR NO-UNDO.
DEF VAR lcContConFile           AS CHAR NO-UNDO.
DEF VAR lcMailContent           AS CHAR NO-UNDO.
DEF VAR liBillPeriod            AS INT  NO-UNDO.
DEF VAR lcSMSTextOriginal       AS CHAR NO-UNDO.
DEF VAR lcSMSReplacedText       AS CHAR NO-UNDO.
DEF VAR liLoop AS INT NO-UNDO. 
DEF VAR lcMonitor AS CHAR NO-UNDO. 

DEF VAR liSMSCntValue AS INT  NO-UNDO. 
DEF VAR liStartTime   AS INT  NO-UNDO. 
DEF VAR liStopTime    AS INT  NO-UNDO. 
DEF VAR liPauseTime   AS INT  NO-UNDO.
DEF VAR PauseFlag     AS LOG  NO-UNDO.
DEF VAR lcSMSSchedule AS CHARACTER NO-UNDO.
DEF VAR liTime2Pause  AS INTEGER   NO-UNDO.
DEF VAR lEndSeconds   AS INTEGER   NO-UNDO.
DEF VAR lIniSeconds   AS INTEGER   NO-UNDO.
DEF VAR lNowSeconds   AS INTEGER   NO-UNDO.

DEF STREAM sEmail.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest
NO-LOCK NO-ERROR.

IF NOT AVAILABLE MsRequest OR 
                 MsRequest.ReqType NE ({&REQTYPE_SMS_INVOICE}) THEN 
   RETURN "ERROR".

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

lcMonitor = fGetRequestNagiosToken(MsRequest.Reqtype).

/* Email Address Conf File */
ASSIGN lcAddrConfDir = fCParamC("RepConfDir")
       lcContConFile = fCParamC("SMSInvContFile")
       liSMSCntValue = fCParamI("SMSCountValue")
       /* ie. "32400-79200" Send between 9:00-22:00 */
       lcSMSSchedule = fCParamC("SMSSchedule")
       liTime2Pause  = fCParamI("Time2Pause")
       ldaDateFrom   = MsRequest.ReqDtParam1
       liMonth       = MONTH(ldaDateFrom)
       liLoop        = 0
       liStartTime   = TIME
       liStopTime    = 0
       liPauseTime   = 0
       PauseFlag     = FALSE.

IF liTime2Pause < 0 THEN liTime2Pause = 0.
IF liTime2Pause > 3599 THEN liTime2Pause = 3599.  /* 1 Hour */

lIniSeconds = INTEGER(ENTRY(1,lcSMSSchedule,"-")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN lIniSeconds = 0.
lEndSeconds = INTEGER(ENTRY(2,lcSMSSchedule,"-")) NO-ERROR.
IF ERROR-STATUS:ERROR THEN lEndSeconds = 0.

IF lIniSeconds <= 0 THEN lIniSeconds = 1.
IF lIniSeconds > 86399 THEN lIniSeconds = 86399. /* 23:59:59 */

IF lEndSeconds <= 0 THEN lEndSeconds = 1.
IF lEndSeconds > 86399 THEN lEndSeconds = 86399. /* 23:59:59 */

IF lIniSeconds >= lEndSeconds THEN
ASSIGN /* 9:00-22:00 */
   lIniSeconds = 32400
   lEndSeconds = 86399.

INVOICE_LOOP:
FOR EACH Invoice WHERE
         Invoice.Brand    = gcBrand AND
         Invoice.InvType  = 1 AND
         Invoice.InvDate >= ldaDateFrom AND
         Invoice.InvAmt  >= 0 NO-LOCK:

   IF MONTH(Invoice.InvDate) NE liMonth THEN NEXT INVOICE_LOOP.

   liBillPeriod = YEAR(Invoice.ToDate) * 100 + MONTH(Invoice.ToDate).

   FIND FIRST Customer WHERE
              Customer.Custnum = Invoice.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN NEXT INVOICE_LOOP.

   lcSep = (IF Customer.Language = 5 THEN "." ELSE ",").

   IF Invoice.DelType <> {&INV_DEL_TYPE_EMAIL_PENDING} AND
      Invoice.DelType <> {&INV_DEL_TYPE_SMS} THEN NEXT INVOICE_LOOP.

   IF liLoop = 0 THEN 
      liStartTime = TIME.

   /* Pause can be passed with liSMSCntValue 0 from cparam */
   IF liLoop > 0 AND liSMSCntValue > 0 AND PauseFlag THEN
   DO:            
      ASSIGN liStopTime  = TIME
             lNowSeconds = liStopTime.
             
      liPauseTime = liTime2Pause - (liStopTime - liStartTime).
      IF liPauseTime > liTime2Pause THEN liPauseTime = liTime2Pause.       
      
      /* If is too late (or early after midnight), create message now 
         but assing sending time later according to pause time */
      IF (lNowSeconds > lEndSeconds) OR (lNowSeconds < lIniSeconds) THEN
      DO:
         lIniSeconds = lIniSeconds + liPauseTime.
         IF lIniSeconds >= lEndSeconds THEN
         ASSIGN /* 9:00-22:00 */
            lIniSeconds = 32400
            lEndSeconds = 86399.
      END.
      
      PAUSE liPauseTime NO-MESSAGE.
      ASSIGN 
        liStartTime = TIME
        PauseFlag   = FALSE.
   END.

   lcSMSTextOriginal = fGetSMSText("SMS",
                                   "SMSInvoice",
                                   Customer.Language).

   IF lcSMSTextOriginal = "" THEN NEXT INVOICE_LOOP.

   /* Sending SMS Invoice to customers */
   SUBINVOICE_LOOP:
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      FIND FIRST MobSub WHERE
                 MobSub.MsSeq = SubInvoice.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAIL MobSub THEN NEXT SUBINVOICE_LOOP.

      lcSMSReplacedText = REPLACE(lcSMSTextOriginal,"#AMOUNT", 
         REPLACE(TRIM(STRING(Invoice.InvAmt, "->>>>>>9.99")),".", lcSep)).
   
      lcSMSReplacedText = REPLACE(lcSMSReplacedText,"#DATE",
                          STRING(Invoice.DueDate,"99/99/99")).
      
      DO TRANS:
         fMakeSchedSMS2(MobSub.CustNum,
                        MobSub.CLI,
                        44,
                        lcSMSReplacedText,
                        fMakeTS(),
                        "Fact. Yoigo",
                        STRING(lIniSeconds) + "-" + STRING(lEndSeconds)).

         IF AVAIL CallAlarm THEN RELEASE CallAlarm.
         liLoop = liLoop + 1. /* Count SMS scheduled */
         IF liLoop MOD 5000 EQ 0 AND
         lcMonitor > "" THEN fKeepAlive(lcMonitor).
         IF NOT PauseFlag THEN
            PauseFlag = (liLoop MOD liSMSCntValue) EQ 0.
      END. /* DO TRANS: */
   END. /* FOR EACH SubInvoice OF Invoice NO-LOCK: */
END. /* FOR EACH Invoice WHERE */

/* Send an email to configure list*/
IF lcAddrConfDir > "" THEN
   lcAddrConfDir = lcAddrConfDir + "smsinvoice.email".

IF lcContConFile > "" AND SEARCH(lcAddrConfDir) <> ? THEN DO:
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand            AND
             InvText.Target    = "General"          AND
             InvText.KeyValue  = "EmailConfSMSInv"  AND
             InvText.Language  = 5                  AND 
             InvText.FromDate <= today              AND
             InvText.ToDate   >= today:
       lcMailContent = InvText.InvText.
   END. /* FOR FIRST InvText NO-LOCK WHERE */

   OUTPUT TO VALUE(lcContConFile).
   PUT UNFORMATTED lcMailContent skip.
   OUTPUT CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   SendMail(lcContConFile,"").
END. /* IF SEARCH(lcAddrConfDir) <> ? THEN DO: */

fReqStatus(2,""). /* request handled succesfully */
