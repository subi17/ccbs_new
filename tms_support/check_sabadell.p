{Func/date.i}
input from check_sabadell_payterm.log.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def stream sout.
output stream sout to check_sabadell_payterm_2.log.

DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE k AS INTEGER NO-UNDO. 
DEFINE VARIABLE l AS INTEGER NO-UNDO. 

def buffer binvrow for invrow.
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liMsseq AS INTEGER NO-UNDO. 
DEFINE VARIABLE liInvnum AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCustnum AS INTEGER NO-UNDO. 

DEFINE VARIABLE ldeMaxLoss AS DECIMAL NO-UNDO. 

DEFINE TEMP-TABLE ttsub
FIELD i AS INT
INDEX i IS PRIMARY UNIQUE i. 

def buffer bmsowner for msowner. 
DEFINE VARIABLE ldeMaxMC AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeMC AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeSubAmt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeOther AS DECIMAL NO-UNDO. 

DEFINE VARIABLE ldaISTCDate AS DATE NO-UNDO. 
DEFINE VARIABLE ldaEnddate AS DATE NO-UNDO. 
DEFINE VARIABLE ldeMinCons AS DECIMAL NO-UNDO. 

DEFINE VARIABLE ldeInvRow AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcBillCodes AS CHAR NO-UNDO. 

DEFINE VARIABLE lcNote AS CHARACTER NO-UNDO. 
   
put stream sout unformatted
   "EXT.INV.ID;CUSTNUM;SUBSCR.ID;SABADEL_CODES;SABADELL_AMT;OTHER_AMT;MC_AMT;MCCONTR1_AMT;CLITYPE;ISTC_DATE;UNDER_CHARGE" skip.


DEFINE VARIABLE ldeError AS DECIMAL NO-UNDO. 
repeat trans:

   import unformatted lcLine.
   if i mod 100 = 0 THEN DO:
      disp i k with frame a.
      pause 0.
   END.

   liMsseq = int(entry(3,lcLine,";")).
   liMsseq = int(entry(3,lcLine,";")).
   liInvNum = int(entry(1,lcLine,";")).

   FIND FIRST msowner NO-LOCK where
              msowner.msseq = liMsseq and
              msowner.tsend > 20140601 and
              lookup(msowner.clitype,"cont,cont4,cont5,cont6") > 0 no-error.
   IF NOT AVAIL msowner then next.

   i = i + 1.
   ldaISTCDate = ?.
   FIND FIRST bmsowner NO-LOCK where
              bmsowner.msseq = msowner.msseq and
              bmsowner.tsend > msowner.tsend and
              bmsowner.tsbegin < 20140701 and
              bmsowner.clievent begins "is" no-error.
  IF AVAIL bmsowner then do:
      fTS2Date(bmsowner.tsbegin, output ldaISTCDate).
      k = k + 1.
  end.
   
  ldaEnddate = ?.
  if msowner.tsend < 99999999.99999 then do:
     FIND FIRST mobsub NO-LOCK where
      mobsub.msseq = msowner.msseq no-error.
      IF NOT AVAIL mobsub then  do:
         fTs2Date(msowner.tsend, output ldaEnddate). 
         MESSAGE "here" VIEW-AS ALERT-BOX.
      end.
  end.
   
   case msowner.clitype:
      when "cont" then ldeMC = 6.
      when "cont4" then ldeMC = 6.
      when "cont5" then ldeMC = 19.
      when "cont6" then ldeMC = 39.
   end.
   if ldaISTCDate ne ? then ldeMc = (day(ldaISTCDate) - 1) / 30 * ldeMc.

/*
   if (msowner.tsbegin >= 20140601 and msowner.clievent ne "icc") then
      MESSAGE "wtf" msowner.cli msowner.clievent
      VIEW-AS ALERT-BOX. */

/*
   FIND FIRST ttsub NO-LOCK where   
      ttsub.i = msowner.msseq no-error.
   IF AVAIL ttsub then MESSAGE "wtf" VIEW-AS ALERT-BOX.
   create ttsub.
   assign ttsub.i = msowner.msseq.
*/

   find invoice NO-LOCK where
        invoice.invnum = liinvnum.
      
   FIND FIRST subinvoice NO-LOCK where
              subinvoice.invnum = liinvnum and
              subinvoice.msseq = limsseq no-error.

   ldeSubAmt = 0.
   ldeOther = 0.
   lcBillCodes = "".
   ldeMinCons = 0.

   for each invrow NO-LOCK where
            invrow.invnum = liInvNum and
            invrow.subinvnum = subinvoice.subinvnum:

      if ldaISTCDate NE ? AND 
         invrow.todate >= ldaISTCDate then next.

     if lookup(invrow.billcode,
        "PAYTERMBS,PAYTERMENDBS,RVTERMBSF,PAYTERMCGBS") > 0 then do:
         lcBillCodes = lcBillCodes + "," + invrow.billcode.
         ldeMaxLoss = ldeMaxLoss + invrow.amt.
         ldeSubAmt = ldeSubAmt + invrow.amt.
      end.
      else do:
         if invrow.billcode begins "mccontr"
         then ldeMinCons = ldeMinCons + invrow.amt.
         else ldeOther = ldeOther + invrow.amt.
      end. 
   end.

   if lcBillCodes eq "" then lcNote = "SKIPPED: payterm fees after iSTC".
   else if ldeOther >= ldeMC then lcNote = "SKIPPED: other cons. exceeds MC".
   else do:
      if ldeOther + ldeSubAmt > ldeMC THEN
         ldeError = ldeMc - ldeOther.
      else if ldeOther + ldeSubAmt <= ldeMc then
         ldeError = ldeSubAmt.

      if ldeerror < 0 then MESSAGE "huu" VIEW-AS ALERT-BOX.

      lcNote = string(ldeError,">9.99").
      ldeMaxMC = ldeMaxMC + ldeError.
   end.

   put stream sout unformatted 
      invoice.extinvid ";"
      invoice.custnum ";" 
      subinvoice.msseq ";"
      trim(lcbillcodes,",") ";"
      ldeSubAmt ";"
      ldeOther ";"
      ldeMC ";"
      ldeMinCons ";"
      msowner.clitype ";"
      ldaISTCDate ";"
      lcNote
      skip. 
  
end.

MESSAGE i k ldeMaxLoss ldeMaxMC VIEW-AS ALERT-BOX. 
