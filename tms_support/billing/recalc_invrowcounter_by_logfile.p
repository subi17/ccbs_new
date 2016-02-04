{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/timestamp.i}

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.
def var liCount as int no-undo.
def var lccli as char no-undo.
def var lcline as char no-undo.
def var lcinputfile as char no-undo.
def var lcoutputfile as char no-undo.
def var liperiod as int no-undo.

def temp-table ttCLI no-undo
    FIELD CLI AS CHAR.

def stream sin.
def stream sout.

pause 0.
update lcinputfile label "Enter Iutput File Path:" 
   FORMAT "X(256)" VIEW-AS FILL-IN SIZE 45 BY 1 skip
   with overlay row 10 centered title " Recalculate COUNTER " 
        side-labels frame fCheck.
hide frame fCheck no-pause.

IF lcinputfile = "" THEN DO:
   MESSAGE "Please Enter Iutput File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcinputfile = "" THEN DO: */

liperiod = year(today) * 100 + month(today).
lcoutputfile = "/apps/yoigo/tms_support/billing/log/recalc_invrowcounter_" + string(fMakeTS()) + ".log".

input stream sin from value(lcinputfile).
output stream sout to value(lcoutputfile).
   
repeat:
   import stream sin unformatted lcline.

   if lcline = "" or lcline begins "MSISDN" then next.
   lccli = entry(1,lcline, chr(9)).

   if not can-find (first ttCLI WHERE ttCLI.CLI = lccli) then do:
      create ttCLI.
             ttCLI.CLI = lccli.
   end.
end.

for each ttCLI no-lock,
    first msowner no-lock use-index cli_s where
          msowner.cli = ttCLI.CLI:

   assign i = i + 1
          j = 0
          k = 0
          liCount = 0.

   disp msowner.cli  string(i).
   pause 0.

   for each invseq no-lock use-index msseq where
            invseq.msseq = msowner.msseq and
            invseq.custnum = msowner.invcust and
            invseq.billed = false:

      j = j + 1.

      RUN tms_support/billing/conv_invrowcounter.p(InvSeq.InvSeq,
                               OUTPUT liCount).

      k = k + liCount.
   end.

   put stream sout unformatted ttCLI.CLI + CHR(9) +
                               "unbilled InvSeqs: " + STRING(j) + CHR(9) +
                               "containing CDRs: "  + STRING(k) skip.
end.

output stream sout close.
input stream sin close.
