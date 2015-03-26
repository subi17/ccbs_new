/* ----------------------------------------------------------------------
  MODULE .......: telefonica_file_not_processed.p
  TASK .........: Notify customer if telefonica is not received
  APPLICATION ..: tms
  AUTHOR .......: Vikas 
  CREATED ......: 9.12.13
  Version ......: yoigo
---------------------------------------------------------------------- */
&GLOBAL-DEFINE MailTitleSpaces Allow

{commpaa.i}
gcBrand = "1".
Katun = "Qvantel".
{timestamp.i}
{cparam2.i}
{email.i}
{tmsconst.i}

DEF VAR lcAddrConfDir AS CHAR NO-UNDO.
DEF VAR liPeriod      AS INT  NO-UNDO.
DEF VAR lcEmailText   AS CHAR NO-UNDO.

liPeriod = YEAR(TODAY) * 100 + MONTH(TODAY).

FIND FIRST ActionLog WHERE
           ActionLog.Brand        = gcBrand        AND
           ActionLog.ActionID     = "TELEFONICA"   AND
           ActionLog.ActionPeriod = liPeriod NO-LOCK NO-ERROR.
IF AVAIL ActionLog THEN RETURN.

lcAddrConfDir = fCParamC("RepConfDir").

lcEmailText = "Telefonica file has not been arrived for billing period " +
              STRING(liPeriod) + " in TMS till now " + fTS2HMS(fMakeTS()) +
              " .".

/* Send an email to configure list*/
IF lcAddrConfDir > "" THEN DO:
   lcAddrConfDir = lcAddrConfDir + "telefonica_file_not_received.email".
   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   /* Send via mail */
   IF LOOKUP(lcMailHost,{&HOSTNAME_STAGING}) > 0 THEN
      SendMaileInvoice(lcEmailText,"").
   ELSE
      SendMail(lcEmailText,"").
END.
