def var lccli as char no-undo.

pause 0.
update lccli
    format "x(12)"
    label "MSISDN"
    help "MSISDN"
    with overlay row 10 centered side-labels title " COUNTER " frame fcount.
hide frame fcount no-pause.

if lccli = "" then return.

for first mobsub no-lock where
          mobsub.cli = lccli,
     each saldocounter no-lock where
          saldocounter.msseq = mobsub.msseq:

   display saldocounter.period
           saldocounter.msseq
           saldocounter.qty
           saldocounter.amt.
end.


