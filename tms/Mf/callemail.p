/* ----------------------------------------------------------------------
  MODULE .......: callemail.p
  TASK .........: send one invoice OR ALL unbilled call spesification
                  VIA email from customer
  APPLICATION ..: Master
  AUTHOR .......: jr
  CREATED ......: 13-06-02
  CHANGED ......: 18-11-02 jr Table names jne..
                  12.09.03/aam brand
  VERSION ......: M15

-------------------------------------------------------------------------- */
{Syst/commali.i}
{Func/excel.i}
{Func/email.i}

DEF INPUT PARAMETER asno  AS INT  NO-UNDO.
DEF INPUT PARAMETER invno AS INT  NO-UNDO.
DEF INPUT PARAMETER email AS CHAR NO-UNDO.

DEF VAR logfile     AS CHAR NO-UNDO.
DEF VAR emailsender AS CHAR NO-UNDO.

DEF TEMP-TABLE tinvseq LIKE invseq.

{Func/cparam.i DefEmailSender    RETURN}.  emailsender = TMSParam.CharVal .

FIND customer NO-LOCK WHERE customer.custnum = asno NO-ERROR.

IF invno NE 0 THEN
FIND invseq where 
      invseq.custnum = asno AND
      InvSeq.InvNum  = invno NO-LOCK NO-ERROR.
IF AVAIL invseq THEN
BUFFER-COPY invseq TO tinvseq NO-ERROR.   

ELSE 
FOR EACH invseq NO-LOCK WHERE
        invseq.custnum = asno AND
        invseq.billed = FALSE:
    BUFFER-COPY invseq TO Tinvseq NO-ERROR.
END.

logfile = "/tmp/" + STRING(asno).
OUTPUT STREAM excel TO VALUE(logfile).

IF invno NE 0 THEN
PUT STREAM excel 
"INVOICE SPECIFICATION FOR " Customer.CustName AT 27 SKIP
"INVOICE NUMBER " STRING(invno)                AT 17 SKIP
.
ELSE 
PUT STREAM excel
"UNBILLED CALLS SPECIFICATION FOR " Customer.CustName AT 34 SKIP
.
FOR EACH tinvseq NO-LOCK:
    FOR EACH fixcdr WHERE
             fixcdr.invseq = tinvseq.invseq NO-LOCK
            BREAK BY fixcdr.cli
                  BY fixcdr.date
                  BY fixcdr.timestart:

        IF FIRST-OF(fixcdr.cli) THEN
        PUT STREAM excel SKIP(1) 
        "From number "
        fixcdr.cli AT 13 SKIP
        "DATE"        AT 2
        "FROM"        AT 12
        "TO NUMBER"   AT 22
        "DESTINATION" AT 38
        "DURATION"    AT 60
        "PRICE"       TO 80.

        FIND FIRST ccn WHERE
                   CCN.Brand = gcBrand AND
                   ccn.ccn   = fixcdr.ccn NO-LOCK NO-ERROR.

        PUT STREAM excel UNFORMATTED
        STRING(FixCDR.Date,"99-99-99")          AT 2
        string(FixCDR.TimeStart,"hh:mm:ss") format "x(8)" AT 12
        FixCDR.BSub format "x(14)" AT 22       /* B-subscriber number */
        CCN.CCNName  at 38 format "x(20)" 
        FixCDR.Duration to 67 format "zzz9"
        STRING(FixCDR.GrossPrice - FixCDR.DiscValue)  TO 80  /* when pu-htyyp   */ ~         SKIP.
    END.
END.   

OUTPUT STREAM excel CLOSE.
RUN SEND-LOG.

PROCEDURE SEND-LOG:
      ASSIGN 
      xMailSubj = "Invoice_specification"
      xMailAddr = email
      xMailFrom = emailsender.
      SendMail(logfile,logfile).
END PROCEDURE.

