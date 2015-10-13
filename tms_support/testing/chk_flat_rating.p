def var lccli as char no-undo.
def var liperiod as int no-undo.
def var lilongperiod as int no-undo.
def var ldadate as date no-undo.

liperiod = year(today) * 100 + month(today).

pause 0.
update lccli format "x(12)" 
          label "MSISDN"  
          help "MSISDN" skip
       liperiod format "999999"
          label "Period" 
          help "YYYYMM"
with side-labels overlay row 10 centered frame fcli.
hide frame fcli no-pause.        
if lccli = "" then return.
        
ldadate = date(liperiod mod 100,1,int(truncate(liperiod / 100,0))).
lilongperiod = liperiod * 100 + 1.

find first msowner where msowner.cli = lccli no-lock no-error.
if not available msowner then do:
   message "Unknown MSISDN"
   view-as alert-box.
   return.
end.
disp msowner.msseq msowner.cli msowner.clitype msowner.invcust.

for each mservicelimit no-lock where
         mservicelimit.msseq = msowner.msseq and
         mservicelimit.endts > lilongperiod,
   first servicelimit no-lock where
         servicelimit.slseq = mservicelimit.slseq:
   
   disp servicelimit.groupcode with 3 down.
   disp mservicelimit.
end.

for each servicelcounter no-lock where
         servicelcounter.msseq = msowner.msseq and
         servicelcounter.period = liperiod:
    disp servicelcounter with 3 down. 
end.

def var ldtot as int no-undo.

for each mobcdr no-lock where
         mobcdr.cli = msowner.cli and
         mobcdr.datest >= ldadate and
         (mobcdr.dialtype = 4 or mobcdr.dialtype = 12):

   disp datest string(timest,"hh:mm:ss")
        billcode  format "x(10)"
        dctype  format "x(1)" column-label "T"
        dcevent format "x(8)" column-label "Package"
        string(mobcdr.errorcode > 0,"1/0") column-label "E"
        mobcdr.billdur format ">>>9" column-label "Dur"  
        amount format ">>9.999" column-label "Price"  . 
        
   if dctype = "1" and mobcdr.errorcode = 0 then 
      ldtot = ldtot + mobcdr.billdur.
   
   disp ldtot column-label "Sec" format ">>>>>9" 
        ldtot / 60 format ">>>>9.99" column-label "Min".
end.
