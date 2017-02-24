{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/timestamp.i}

def var i as int no-undo.
def var k as int no-undo.
def var liCount as int no-undo.
def var liMsSeq as int no-undo.
DEF VAR liInvSeq AS INT NO-UNDO. 
def var lcline as char no-undo.
def var lcinputfile as char no-undo.
def var lcoutputfile as char no-undo.
def var liperiod as int no-undo.

def temp-table ttInvSeq no-undo
    FIELD MsSeq AS INT
    FIELD InvSeq AS INT.

def stream sin.
def stream sout.

pause 0.
update lcinputfile label "Enter Input File Path:" 
   FORMAT "X(256)" VIEW-AS FILL-IN SIZE 45 BY 1 skip
   with overlay row 10 centered title " Recalculate COUNTER " 
        side-labels frame fCheck.
hide frame fCheck no-pause.

IF lcinputfile = "" THEN DO:
   MESSAGE "Please Enter Input File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcinputfile = "" THEN DO: */

liperiod = year(today) * 100 + month(today).
lcoutputfile = "/apps/yoigo/tms_support/billing/log/recalc_invrowcounter_" + string(fMakeTS()) + ".log".

input stream sin from value(lcinputfile).
output stream sout to value(lcoutputfile).
   
repeat:
   import stream sin unformatted lcline.

   if lcline = "" or lcline begins "MSISDN" then next.
   ASSIGN
      liMsSeq  = int(entry(2,lcline, chr(9))) 
      liInvSeq = int(entry(3,lcline, chr(9))) no-error.

   if error-status:error then do:
      MESSAGE "incorrect line format" skip.
      return.
   end.

   if not can-find (first ttInvSeq WHERE ttInvSeq.invSeq = liInvSeq) then do:
      create ttInvSeq.
      assign
          ttInvSeq.InvSeq = liInvSeq
          ttInvSeq.MsSeq  = liMsseq.
   end.
end.

put stream sout unformatted
   "MsSeq" chr(9)
   "InvSeq" chr(9)
   "CDRs" skip.

for each ttInvSeq no-lock:

   assign i = i + 1
          k = 0
          liCount = 0.

   disp ttInvSeq.msseq string(i).
   pause 0.

   for each invseq no-lock where
            invseq.InvSeq = ttInvSeq.InvSeq and
            invseq.MsSeq  = ttInvSeq.MsSeq and
            invseq.billed = false:

      RUN billing/conv_invrowcounter.p(InvSeq.InvSeq,
                               OUTPUT liCount).
      k = k + liCount.
   end.

   put stream sout unformatted ttInvSeq.MsSeq CHR(9) 
                               ttInvSeq.Invseq CHR(9)
                               k skip.
end.

output stream sout close.
input stream sin close.
