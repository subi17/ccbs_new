def stream slog.
output stream slog to /apps/snet/200809/aam_yob89.log append.

def var i as int no-undo.

for each msrequest no-lock where
         brand = "1" and
         reqtype = 18 and
         reqstat = 2 and
         actstamp > 20080800,
   first termmobsub no-lock where
         termmobsub.msseq = msrequest.msseq and
         termmobsub.clitype = "contrd1",
   first fixedfee no-lock where
         fixedfee.brand = "1" and
         fixedfee.hosttable = "mobsub" and
         fixedfee.keyvalue = string(msrequest.msseq) and
         fixedfee.billcode = "contdatamf",
    each ffitem of fixedfee exclusive-lock where
         ffitem.billed = false and
         ffitem.amt < 25:

   i = i + 1.
   pause 0.
   disp i with 1 down.

   
   put stream slog unformatted
      termmobsub.cli chr(9)
      termmobsub.msseq chr(9)
      fixedfee.ffnum chr(9)
      ffitem.billper chr(9)
      ffitem.amt  chr(9)
      ffitem.concerns[2] skip.

   assign
      ffitem.amt = 25
      ffitem.concerns[2] = if ffitem.concerns[1] = 20080801
                           then 20080831
                           else 20080930.
   /*
   disp termmobsub.cli
        msrequest.actstamp
        ffitem.billper
        ffitem.billed
        ffitem.amt
        ffitem.concerns format "99999999".
   */     
end.

output stream slog close.

