/* ----------------------------------------------------------------------
  MODULE .......: smsmessage.i
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 20.04.15
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Func/timestamp.i}
{Syst/tmsconst.i}

&IF "{&SMSMESSAGE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE SMSMESSAGE_I YES

&GLOBAL-DEFINE SMS_DELITYPE_OUT 1
&GLOBAL-DEFINE SMS_DELITYPE_IN 2

&GLOBAL-DEFINE SMS_TYPE_OFFER 1
&GLOBAL-DEFINE SMS_TYPE_Q25 2
&GLOBAL-DEFINE SMS_TYPE_CONSULT 3

&GLOBAL-DEFINE SMS_DELISTATUS_NEW 1
&GLOBAL-DEFINE SMS_DELISTATUS_SENT 2
&GLOBAL-DEFINE SMS_DELISTATUS_RECEIVED 3

FUNCTION fCreateSMS RETURNS INTEGER
   (iiCustNum  AS INT,
    icCLI      AS CHAR,
    iiMsSeq    AS INT,
    iiOrdeRId  AS INT,
    icMessage  AS CHAR,
    icSender   AS CHAR,
    iiSMSType  AS INT):
   
   DEF VAR ldeCreStamp AS DEC NO-UNDO. 
   ldeCreStamp = fMakeTS().

   CREATE SMSMessage.
   ASSIGN SMSMessage.SMSSeq      = NEXT-VALUE(SMSSeq)
          SMSMessage.CreStamp    = ldeCreStamp
          SMSMessage.ActStamp    = ldeCreStamp
          SMSMessage.MsSeq       = iiMsSeq
          SMSMessage.MSISDN      = icCLI
          SMSMessage.CustNum     = iiCustnum
          SMSMessage.OrderID     = iiOrderId
          SMSMessage.DeliType    = {&SMS_DELITYPE_OUT}
          SMSMessage.DeliMsg     = icMessage
          SMSMessage.OrigAddress = icSender
          SMSMessage.SMSType     = iiSMSType
          SMSMessage.DeliStatus  = {&SMS_DELISTATUS_NEW}.

   RETURN SMSMessage.SMSSeq.
END.

FUNCTION fCreateResponseSMS RETURNS INTEGER
   (iiCustNum  AS INT,
    iiMsSeq     AS INT,
    iiOrdeRId   AS INT,
    icMessage  AS CHAR,
    icSender   AS CHAR,
    iiSMSSource AS INT,
    ideDeliStamp AS DEC):
   
   DEF VAR ldeCreStamp AS DEC NO-UNDO. 
   ldeCreStamp = fMakeTS().

   CREATE SMSMessage.
   ASSIGN SMSMessage.SMSSeq      = NEXT-VALUE(SMSSeq)
          SMSMessage.CreStamp    = ldeCreStamp
          SMSMessage.ActStamp    = ldeCreStamp
          SMSMessage.DeliStamp   = ideDeliStamp
          SMSMessage.MsSeq       = iiMsSeq
          SMSMessage.MSISDN      = icSender
          SMSMessage.CustNum     = iiCustnum
          SMSMessage.OrderID     = iiOrderId
          SMSMessage.DeliType    = {&SMS_DELITYPE_IN}
          SMSMessage.DeliMsg     = icMessage
          SMSMessage.SMSType     = {&SMS_TYPE_OFFER}
          SMSMessage.DeliStatus  = {&SMS_DELISTATUS_RECEIVED}.

   RETURN SMSMessage.SMSSeq.
END.

&ENDIF
