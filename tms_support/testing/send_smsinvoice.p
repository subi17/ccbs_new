{Syst/commpaa.i}
assign gcbrand = "1"
       katun = "Qvantel".
{Func/fmakemsreq.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/femailinvoice.i}
{Func/email.i}

def stream sin.
def stream sout.
def var lcline        as char no-undo.
def var lccli         as char no-undo.
def var lcdel         as char no-undo.
DEF VAR lcSMSReplacedText as char no-undo.
DEF VAR lcSMSTextOriginal as char no-undo.
DEF VAR lcSep             as char no-undo.
def var i                 as int no-undo.
DEFINE VARIABLE liInvnum AS INTEGER NO-UNDO. 

lcdel = ";".

input stream sin from "/apps/yoigo/tms_support/testing/send_smsinvoice_201211.csv".
output stream sout to "/apps/yoigo/tms_support/testing/send_smsinvoice_201211.log".

repeat:
   import stream sin unformatted lcline.
   if lcline = "" or lcline = ? or lcline begins "Cli" then next.

   if num-entries(lcline,lcdel) <> 4 then do:
      put stream sout unformatted lcline lcdel "ERROR:Wrong file format" skip.
      next.
   end.
   assign lccli = trim(entry(1,lcline,lcdel)).

   FIND mobsub NO-LOCK WHERE
        mobsub.cli = lccli NO-ERROR.
   IF NOT AVAILABLE mobsub THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:MobSub not found" skip.
      next.
   END.

   FIND customer NO-LOCK WHERE
        customer.custnum = mobsub.custnum NO-ERROR.
   IF NOT AVAILABLE customer THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:customer not found" skip.
      next.
   END.

   lcSep = (IF Customer.Language = 5 THEN "." ELSE ",").

   liInvnum = 0.
   FOR EACH Invoice WHERE
            Invoice.Brand    = gcBrand AND
            Invoice.CustNum  = Customer.CustNum AND
            Invoice.InvDate  = 11/01/2012 AND
            Invoice.InvType  = 1 AND
            Invoice.InvAmt   > 0 AND
            Invoice.DDState  = 1 NO-LOCK,
      FIRST SubInvoice OF Invoice NO-LOCK WHERE
            SubInvoice.MsSeq = mobsub.msseq USE-INDEX Invnum:
      liInvnum = invoice.invnum.
      LEAVE.
   END.

   FIND FIRST invoice NO-LOCK where
              invoice.invnum = liInvnum NO-ERROR.
   IF NOT AVAIL Invoice THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:Invoice not found" skip.
      next.
   END.

   IF Invoice.DelType = {&INV_DEL_TYPE_EMAIL_PENDING} OR
      Invoice.DelType = {&INV_DEL_TYPE_SMS} THEN DO:
      put stream sout unformatted lcline lcdel "ERROR:SMS will be sent with normal process" skip.
      next.
   END.

   lcSMSTextOriginal = fGetSMSText("SMS",
                                   "SMSInvoice",
                                   Customer.Language).

   IF lcSMSTextOriginal = "" THEN NEXT.

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
                     "622",
                     "36000-75600").
      IF AVAIL CallAlarm THEN RELEASE CallAlarm. 
   END. /* DO TRANS: */

   i = i + 1.
   if i mod 100 = 0 then do:
      disp i.
      pause 0.
   end.
   
   put stream sout unformatted lcline lcdel "Sent" skip.
end.

input stream sin close.
output stream sout close.
