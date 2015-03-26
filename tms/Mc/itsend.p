/* ------------------------------------------------------
  MODULE .......: itsend.p
  FUNCTION .....: send information texts via eMail
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.05.03
  MODIFIED .....: 10.06.03/aam InfoType vrs. Customer.DirMark
                  12.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{email.i}
{tmsparam2.i}
{timestamp.i}

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum AS INT. 

DEF TEMP-TABLE ttText NO-UNDO
   FIELD Target AS CHAR
   FIELD KeyVal AS CHAR
   FIELD ITNum  AS INT.

DEF INPUT  PARAMETER TABLE FOR ttCust.
DEF INPUT  PARAMETER idtDate1  AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2  AS DATE NO-UNDO.
DEF OUTPUT PARAMETER oiCount   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiErrors  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocErrFile AS CHAR NO-UNDO. 

DEF VAR lcFile AS CHAR NO-UNDO.
DEF VAR liCnt  AS INT  NO-UNDO. 

DEF STREAM sLog.


FUNCTION fErrorTxt RETURNS LOGICAL
   (icText AS CHAR).

   PUT STREAM sLog UNFORMATTED
      "Cust: " 
      Customer.CustNum
      " " 
      icText 
      SKIP.

   oiErrors = oiErrors + 1.
END.


FUNCTION fSendText RETURNS LOGICAL.

   /* has direct marketing through eMail been banned */
   IF InvText.InfoType = "M" AND 
      (LOOKUP("e",Customer.DirMark) > 0 OR
       LOOKUP("a",Customer.DirMark) > 0)       
   THEN RETURN FALSE. 

   /* check if text has already been sent to this customer */
   IF CAN-FIND(ITSendLog WHERE
               ITSendLog.TxtType = 1                AND
               ITSendLog.ITNum   = InvText.ITNum    AND
               ITSendLog.CustNum = Customer.CustNum)
   THEN RETURN FALSE. 

   /* no eMail address */
   IF Customer.eMail = "" THEN DO:
      fErrorTxt("No eMail address").
      RETURN FALSE.
   END.

           /* mail recipients */
   ASSIGN  xMailAddr = Customer.eMail
           /* subject */
           xMailSubj = '"' + InvText.TxtTitle + '"'
           xMailFrom = fCParamC("ITMailSender").

   /* message text */
   lcFile = "/tmp/itmail_" + STRING(TIME) + ".txt".
   OUTPUT TO VALUE(lcFile).
   PUT UNFORMATTED InvText.InvText SKIP.
   OUTPUT CLOSE.

   /* if there are no attachments then attach the text itself */
   IF InvText.Attachment = "" THEN xMailAttach = lcFile.

   /* attachments */
   ELSE xMailAttach = InvText.Attachment.

   IF SEARCH(xMailAttach) = ? THEN DO:
      fErrorTxt("Attachment not found (" + xMailAttach + ")").
      RETURN FALSE. 
   END.

   /* actual sending */
   IF NOT SendMail(lcFile,
                   xMailAttach)
   THEN DO:
      fErrorTxt("eMail sending failed (" + 
                lcFile + " / " + xMailAttach + ")").
      RETURN FALSE.
   END.

   ELSE DO:
      oiCount = oiCount + 1.

      /* mark text as sent */
      CREATE ITSendLog.
      ASSIGN ITSendLog.Brand      = Customer.Brand 
             ITSendLog.TxtType    = 1
             ITSendLog.ITNum      = InvText.ITNum
             ITSendLog.SendMethod = 1
             ITSendLog.CustNum    = Customer.CustNum
             ITSendLog.EMail      = xMailAddr
             ITSendLog.RepType    = "IT"
             ITSendLog.UserCode   = katun.
             ITSendLog.SendStamp  = fMakeTS().
             
      OS-DELETE VALUE(lcFile).

      RETURN TRUE.
   END.                              

END FUNCTION.


ocErrFile = fCParamC("ITMailErrDir").

IF ocErrFile = ? OR ocErrFile = "" 
THEN ocErrFile = "/tmp/".
ELSE IF SUBSTRING(ocErrFile,LENGTH(ocErrFile),1) NE "/"
THEN ocErrFile = ocErrFile + "/".

/* error message log */
ocErrFile = ocErrFile + "itmailerr_" + 
            STRING(YEAR(TODAY),"9999") + 
            STRING(MONTH(TODAY),"99")  + 
            STRING(DAY(TODAY),"99")    +
            "_" + 
            STRING(TIME) + ".txt".

OUTPUT STREAM sLog TO VALUE(ocErrFile).

FOR EACH ttCust,
FIRST Customer NO-LOCK WHERE
      Customer.CustNum = ttCust.CustNum:

   /* text for customer (try to send even if customer has no email address 
      -> get error to log) */
   FOR EACH InvText NO-LOCK WHERE   
            InvText.Brand    = gcBrand                  AND
            InvText.Target   = "Customer"               AND
            InvText.KeyValue = STRING(Customer.CustNum) AND
            InvText.FromDate <= idtDate2                AND
            InvText.ToDate   >= idtDate1                AND
            InvText.Language = Customer.Language        AND
            InvText.Report   = 2:

      fSendText().         

   END.

   /* send group texts only to those who have an email address */
   IF Customer.eMail = "" THEN NEXT.

   /* text for invoice group */
   FOR EACH InvText NO-LOCK WHERE
            InvText.Brand    = gcBrand           AND
            InvText.Target   = "InvGroup"        AND
            InvText.KeyValue = Customer.InvGroup AND
            InvText.FromDate <= idtDate2         AND
            InvText.ToDate   >= idtDate1         AND
            InvText.Language = Customer.Language AND
            InvText.Report   = 2:

      fSendText().         
   END.

   /* text for customer group */
   FOR EACH CGMember NO-LOCK WHERE
            CGMember.Brand   = gcBrand AND
            CGMember.CustNum = Customer.CustNum,
       EACH InvText NO-LOCK WHERE
            InvText.Brand    = gcBrand            AND
            InvText.Target   = "CustGroup"        AND
            InvText.KeyValue = CGMember.CustGroup AND
            InvText.FromDate <= idtDate2          AND
            InvText.ToDate   >= idtDate1          AND
            InvText.Language = Customer.Language  AND
            InvText.Report   = 2:

      fSendText().         

   END.

   /* text for salesman */
   FOR EACH InvText NO-LOCK WHERE
            InvText.Brand    = gcBrand           AND
            InvText.Target   = "Salesman"        AND
            InvText.KeyValue = Customer.Salesman AND
            InvText.FromDate <= idtDate2         AND
            InvText.ToDate   >= idtDate1         AND
            InvText.Language = Customer.Language AND
            InvText.Report   = 2:

      fSendText().         

   END.

END.


