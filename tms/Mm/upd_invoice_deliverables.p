/* ----------------------------------------------------------------------
  module .......: Mm/upd_invoice_deliverables.p
  task .........: Update Invoice Deliverables from Backdoor tool
  application ..: tms
  author .......: vikas
  created ......: 31.08.11
  version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
katun = "Cron".
gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}
{Func/femailinvoice.i}

/* files and dirs */
DEF VAR lcLine           AS CHAR NO-UNDO.
DEF VAR lcLogFile        AS CHAR NO-UNDO. 
DEF VAR lcFileName       AS CHAR NO-UNDO. 
DEF VAR lcIncDir         AS CHAR NO-UNDO. 
DEF VAR lcInputFile      AS CHAR NO-UNDO. 
DEF VAR lcProcDir        AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile  AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir       AS CHAR NO-UNDO. 
DEF VAR lcReportFileOut  AS CHAR NO-UNDO. 
DEF VAR lcOutDir         AS CHAR NO-UNDO. 
DEF VAR lcToday          AS CHAR NO-UNDO.
DEF VAR lcTime           AS CHAR NO-UNDO.
DEF VAR lcSep            AS CHAR NO-UNDO INIT ";".
DEF VAR lhCustomer      AS HANDLE NO-UNDO.

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

ASSIGN
   lcIncDir    = fCParam("InvDeliverable","IncDir") 
   lcProcDir   = fCParam("InvDeliverable","IncProcDir")
   lcSpoolDir  = fCParam("InvDeliverable","OutSpoolDir")
   lcOutDir    = fCParam("InvDeliverable","OutDir")
   lcToday     = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99")  +
                 STRING(DAY(TODAY),"99")
   lcTime      = REPLACE(STRING(TIME,"hh:mm:ss"),":","").

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}
   lhCustomer = BUFFER Customer:HANDLE.
END. /* IF llDoEvent THEN DO: */

FUNCTION fLogLine RETURNS LOG(icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.

END FUNCTION.


FUNCTION fLocalMemo RETURNS LOG(icHostTable AS CHAR,
                                icKey       AS CHAR,
                                icTitle     AS CHAR,
                                icText      AS CHAR,
                                icMemoType  AS CHAR,
                                icUserId    AS CHAR):
   CREATE Memo.
   ASSIGN
      Memo.Brand     = gcBrand
      Memo.CreStamp  = fMakeTS()
      Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
      Memo.Custnum   = (IF AVAILABLE MobSub THEN MobSub.CustNum ELSE 0)
      Memo.HostTable = icHostTable
      Memo.KeyValue  = icKey
      Memo.CreUser   = icUserId
      Memo.MemoTitle = icTitle
      Memo.Memotext  = icText
      Memo.MemoType  = icMemoType.
      
END FUNCTION.


FUNCTION fSetInvDelType RETURNS CHAR(INPUT icDelType AS CHAR,
                                     INPUT icAction  AS CHAR,
                                     INPUT icEmail   AS CHAR):

   DEF VAR liRequest      AS INT  NO-UNDO.
   DEF VAR lcResult       AS CHAR NO-UNDO.
   DEF VAR llEmailChange  AS LOG  NO-UNDO.

   CASE icDelType:
      WHEN "Paper" THEN DO:
         IF icAction = "0" THEN DO:
            IF Customer.DelType <> {&INV_DEL_TYPE_PAPER} THEN
               RETURN "Paper invoice is already turned off".
         END. /* IF icAction = "0" THEN DO: */
         ELSE IF icAction = "1" THEN DO:
            IF Customer.DelType = {&INV_DEL_TYPE_PAPER} THEN
               RETURN "Paper invoice is already turned on".
         END. /* ELSE  IF icAction = "0" THEN DO: */
      END. /* WHEN "Paper" THEN DO: */

      WHEN "Email" THEN DO:
         IF icAction = "0" THEN DO:
            IF Customer.DelType <> {&INV_DEL_TYPE_EMAIL} AND
               Customer.DelType <> {&INV_DEL_TYPE_EMAIL_PENDING} THEN
               RETURN "Email invoice is already turned off".
         END. /* IF icAction = "0" THEN DO: */
         ELSE IF icAction = "1" THEN DO:
            IF icEmail = "" OR Customer.Email = icEmail THEN DO:
               IF Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING} THEN
                  RETURN "Email has already been sent to customer to activate the Email Invoice service".
               ELSE IF Customer.DelType = {&INV_DEL_TYPE_EMAIL} THEN
                  RETURN "Email invoice is already turned on".
            END. /* IF icEmail = "" OR Customer.Email = icEmail THEN DO: */
            IF icEmail = "" AND Customer.Email = "" THEN
               RETURN "Customer email is blank. Invoice Delivery Type to Email can not be changed".
         END. /* ELSE  IF icAction = "0" THEN DO: */
      END. /* WHEN "Email" THEN DO: */

      WHEN "SMS" THEN DO:
         IF icAction = "0" THEN DO:
            IF Customer.DelType <> {&INV_DEL_TYPE_SMS} THEN
               RETURN "SMS invoice is already turned off".
         END. /* IF icAction = "0" THEN DO: */
         ELSE IF icAction = "1" THEN DO:
            IF Customer.DelType = {&INV_DEL_TYPE_SMS} THEN
               RETURN "SMS invoice is already turned on".
         END. /* ELSE  IF icAction = "0" THEN DO: */
      END. /* WHEN "SMS" THEN DO: */

      WHEN "No Delivery" THEN DO:
         IF icAction = "0" THEN DO:
            IF Customer.DelType <> {&INV_DEL_TYPE_NO_DELIVERY} THEN
               RETURN "No Delivery is already turned off".
         END. /* IF icAction = "0" THEN DO: */
         ELSE IF icAction = "1" THEN DO:
            IF Customer.DelType = {&INV_DEL_TYPE_NO_DELIVERY} THEN
               RETURN "No Delivery is already turned on".
         END. /* ELSE  IF icAction = "0" THEN DO: */
      END. /* WHEN "No Delivery" THEN DO: */
   END CASE. /* CASE icDelType: */

   DO TRANSACTION:
      FIND CURRENT Customer EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF NOT AVAIL Customer THEN
         RETURN "ERROR:Customer is locked or not available".

      RUN StarEventInitialize(lhCustomer).
      RUN StarEventSetOldBuffer(lhCustomer).

      /* If DelType is Email then set to Email Pending first and send 
         an email to customer to activate the email service */
      IF icAction = "1" THEN DO:
         IF icDelType = "EMAIL" THEN DO:
            llEmailChange = FALSE.
            /* validate customer email address */
            IF icEmail > "" AND Customer.Email <> icEmail THEN DO:
               ASSIGN Customer.Email = icEmail
                      llEmailChange  = TRUE.

               /* Cancel Ongoing Email Activation Request (if any) */
               IF fPendingEmailActRequest(INPUT Mobsub.Custnum) THEN
                  fCancelPendingEmailActRequest(INPUT Mobsub.Custnum,
                                        INPUT "Customer email is changed.").
            END. /* IF icEmail > "" AND Customer.Email <> icEmail THEN DO: */

            liRequest = fEmailInvoiceRequest(INPUT fMakeTS(),
                                             INPUT TODAY,
                                             INPUT katun,
                                             INPUT MobSub.MsSeq,
                                             INPUT MobSub.CLI,
                                             INPUT Mobsub.Custnum,
                                             INPUT {&REQUEST_SOURCE_SCRIPT},
                                             INPUT Customer.Email,
                                             INPUT 0, /*orderid*/
                                             OUTPUT lcResult).
            IF liRequest = 0 THEN DO:
               IF lcResult = "Customer already has an active request" THEN .
               ELSE UNDO, RETURN "Invoice Delivery Type to Email can not be changed".
            END. /* IF liRequest = 0 THEN DO: */

            /* If Email already validated then mark DelType EMAIL */
            IF liRequest = 1 THEN
               Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
            ELSE
               Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.

            IF llEmailChange = TRUE THEN
               FIND FIRST InvoiceTargetGroup WHERE
                          InvoiceTargetGroup.CustNum = Customer.CustNum AND
                          InvoiceTargetGroup.ToDate >= TODAY AND
                         (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                          InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                    EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL InvoiceTargetGroup THEN DO:
                  IF liRequest = 1 THEN
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
                  ELSE
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.
               END. /* IF AVAIL InvoiceTargetGroup THEN DO: */

         END. /* IF icDelType = "EMAIL" THEN DO: */
         ELSE DO:
            CASE icDelType:
               WHEN "Paper" THEN Customer.DelType = {&INV_DEL_TYPE_PAPER}.
               WHEN "SMS"   THEN Customer.DelType = {&INV_DEL_TYPE_SMS}.
               WHEN "No Delivery" THEN Customer.DelType = {&INV_DEL_TYPE_NO_DELIVERY}.
            END CASE. /* CASE icDelType: */
            
            /* Cancel Ongoing Email Activation Request (if any) */
            FIND FIRST InvoiceTargetGroup WHERE
                       InvoiceTargetGroup.CustNum = Customer.CustNum AND
                       InvoiceTargetGroup.ToDate >= TODAY AND
                      (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                       InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                 NO-LOCK NO-ERROR.
            IF NOT AVAIL InvoiceTargetGroup AND
               fPendingEmailActRequest(INPUT Mobsub.Custnum) THEN
               fCancelPendingEmailActRequest(INPUT Mobsub.Custnum,
                                             INPUT "Invoice Delivery Type is" +
                                    " changed to " + STRING(Customer.DelType)).
         END. /* ELSE DO: */
      END. /* IF icAction = "1" THEN DO: */
      ELSE Customer.DelType = {&INV_DEL_TYPE_NO_DELIVERY}.

      RUN StarEventMakeModifyEvent(lhCustomer).
      fCleanEventObjects().
   END. /* DO TRANSACTION: */

   FIND CURRENT Customer NO-LOCK NO-ERROR.

   RETURN "OK".
   
END FUNCTION.

FUNCTION fSetDetail RETURNS CHAR(INPUT icAction AS CHAR):

   DEF VAR liAction    AS INT  NO-UNDO.
   DEF VAR liReq       AS INT  NO-UNDO.
   DEF VAR lcError     AS CHAR NO-UNDO.
   DEF VAR lcAction    AS CHAR NO-UNDO.

   liAction = INT(icAction).

   IF liAction = 0 THEN lcAction = "off".
   ELSE lcAction = "on".

   FIND FIRST SubSer WHERE
              SubSer.MsSeq   = MobSub.MsSeq AND
              SubSer.ServCom = "CALLSPEC"   AND
              SubSer.SSDate <= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAIL SubSer THEN
      RETURN "ERROR:Call itemization deliverable service not found".

   /* Check ongoing service requests */
   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq      = MobSub.MsSeq AND
                     MsRequest.ReqType    = ({&REQTYPE_SERVICE_CHANGE}) AND
                     MsRequest.ReqCParam1 = SubSer.ServCom AND
               LOOKUP(STRING(MsRequest.ReqStat),{&REQ_INACTIVE_STATUSES}) = 0)
   THEN RETURN "ERROR:Service can not be changed due to " +
               "ongoing network command".
     
   IF liAction EQ SubSer.SSStat THEN
      RETURN "Call itemization is already turned " + lcAction.

   liReq = fServiceRequest(SubSer.MsSeq,
                           Subser.ServCom,
                           liAction,
                           (IF liAction = 1 THEN SubSer.SSParam
                            ELSE ""),
                           fMakeTS(),
                           "",
                           FALSE,      /* fees */
                           FALSE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_SCRIPT},
                           0, /* father request */
                           false, /* mandatory for father request */
                           OUTPUT lcError).
         
   IF liReq = 0 THEN
      RETURN "ERROR:Change request was not accepted for service " +
             SubSer.ServCom + ". " + lcError.

   RETURN "OK".
   
END FUNCTION.


/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName. 
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.

   lcLogFile = lcSpoolDir + "invoice_deliverables_" +
               lcToday + "_" + lcTime + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile).
   fBatchLog("START", lcLogFile).

   LINE_LOOP:
   REPEAT:
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine = "" OR lcLine = ? THEN NEXT LINE_LOOP.

      RUN pInvoiceDeliverables(INPUT lcLine).
      
      fLogLine(RETURN-VALUE).

   END. /* REPEAT: LINE_LOOP: */
  
   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

END.

INPUT STREAM sFile CLOSE.


PROCEDURE pInvoiceDeliverables: 

   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO. 

   /* local variables */
   DEF VAR lcCLI              AS CHAR NO-UNDO.
   DEF VAR lcChannel          AS CHAR NO-UNDO.
   DEF VAR lcDate             AS CHAR NO-UNDO.
   DEF VAR lcDeliverable      AS CHAR NO-UNDO.
   DEF VAR lcInputAction      AS CHAR NO-UNDO.
   DEF VAR lcMemoContent      AS CHAR NO-UNDO.
   DEF VAR lcMemoTitle        AS CHAR NO-UNDO.
   DEF VAR lcError            AS CHAR NO-UNDO.
   DEF VAR lcEmail            AS CHAR NO-UNDO.

   IF NUM-ENTRIES(pcLine,lcSep) <> 6 THEN
      RETURN "ERROR:Wrong file format".

   ASSIGN
      lcCLI         = ENTRY(1,pcLine,lcSep)
      lcDate        = ENTRY(2,pcLine,lcSep)
      lcChannel     = ENTRY(3,pcLine,lcSep)
      lcDeliverable = ENTRY(4,pcLine,lcSep)
      lcInputAction = ENTRY(5,pcLine,lcSep)
      lcEmail       = ENTRY(6,pcLine,lcSep).

   IF lcInputAction = "" OR lcInputAction = ? OR
      LOOKUP(lcInputAction,"0,1") = 0 THEN
      RETURN "ERROR:Invalid action value".

   /* check invoice */
   FIND MobSub WHERE 
        MobSub.Brand = gcBrand AND
        MobSub.CLI   = lcCLI NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN "ERROR:Invalid MSISDN".
   ELSE IF MobSub.PayType THEN RETURN "ERROR:Not a postpaid subscription".

   FIND Customer WHERE
        Customer.CustNum = MobSub.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "ERROR:Customer not found".

   CASE lcDeliverable:
      WHEN "Paper" THEN lcError = fSetInvDelType(lcDeliverable,lcInputAction,lcEmail).
      WHEN "SMS"   THEN lcError = fSetInvDelType(lcDeliverable,lcInputAction,lcEmail).
      WHEN "EMAIL" THEN lcError = fSetInvDelType(lcDeliverable,lcInputAction,lcEmail).
      WHEN "No Delivery" THEN lcError = fSetInvDelType(lcDeliverable,lcInputAction,lcEmail).
      WHEN "Detail"      THEN lcError = fSetDetail(INPUT lcInputAction).
      OTHERWISE RETURN "ERROR:Invalid deliverable value".
   END CASE. /* CASE lcDeliverable: */

   IF lcError <> "OK" THEN RETURN lcError.

   lcMemoContent = MobSub.CLI + " Se ha " +
                   (IF lcInputAction = "0" THEN "desactivado" ELSE
                    "activado") + " la factura " + lcDeliverable.

   IF LOOKUP(lcDeliverable,"Detail") > 0 THEN
      fLocalMemo("MobSub",
                 STRING(MobSub.MsSeq),
                 lcMemoContent,
                 lcMemoContent,
                 "Service",
                 (IF lcChannel > "" THEN lcChannel ELSE katun)).
   ELSE
      fLocalMemo("Invoice",
                 STRING(Customer.CustNum),
                 lcMemoContent,
                 lcMemoContent,
                 "",
                 (IF lcChannel > "" THEN lcChannel ELSE katun)).

   RETURN "OK".

END PROCEDURE.


