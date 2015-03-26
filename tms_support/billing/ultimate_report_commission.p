DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 

def buffer bsinglefee for singlefee.

DEFINE VARIABLE ldtInputDate AS DATE NO-UNDO. 
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcFile1 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDir AS CHARACTER NO-UNDO. 
lcDir = "/apps/yoigo/tms_support/billing/log/".
lcFile1 = "tf_commission_check_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + ".txt".

def frame fcontrolrep 
   ldtInputDate FORMAT "99-99-9999" LABEL "Period" SKIP
   lcDir      FORMAT "X(60)"   LABEL "Directory" SKIP
   lcFile1    FORMAT "X(60)"   LABEL "File" SKIP
   i label "Processed"  skip
   j label "Errors" 
with overlay NO-labels 1 column row 4 title "TF Commission Fee Check".

ldtInputDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).
UPDATE
   ldtInputDate 
   lcDir
   lcFile1 with
frame fcontrolrep.

IF ldtInputDate = ? OR
   NOT lcFile1 > "" THEN RETURN.

pause 0.

liPeriod = YEAR(ldtInputDate) * 100 + MONTH(ldtInputDate).

def stream sout.
output stream sout to VALUE(lcDir + lcFile1).

put stream sout unformatted
   "MSSEQ|CUSTNUM|SINGLE_FEE_ID|PERIOD|BILLED|ERROR" skip.

def stream sout2.
output stream sout2 to created_commissions.txt.

FOR EACH singlefee NO-LOCK where
         singlefee.brand = "1" and
        (singlefee.billcode = "PAYTERMCG1E" OR
         singlefee.billCode = "PAYTERMCGBS"):

   if singlefee.billperiod ne liPeriod then next.
/*
   if lookup(singlefee.memo[1],"Created 09.10.2013,Created 04.10.2013") = 0 
      then MESSAGE singlefee.memo[1] VIEW-AS ALERT-BOX.
*/
   put stream sout2 unformatted
      singlefee.keyvalue "|" 
      singlefee.custnum "|"
      singlefee.billperiod "|"
      singlefee.amt "|"
      singlefee.memo[1] skip.

   i = i + 1.
   if i mod 1000 = 0 then do:
      disp i  j with frame fcontrolrep.
      pause 0.
   end.

   if singlefee.sourcetable eq "fixedfee" then
   FIND FIRST fixedfee NO-LOCK where
              fixedfee.ffnum = int(singlefee.sourcekey) and
              fixedfee.custnum = singlefee.custnum and
              fixedfee.hosttable = "mobsub" and
              fixedfee.keyvalue  = singlefee.keyvalue and
              fixedfee.billcode eq "PAYTERM" and
              fixedfee.financedresult = "00" no-error.

   ELSE   
   FIND FIRST fixedfee NO-LOCK where
              fixedfee.brand  = "1" and
              fixedfee.custnum = singlefee.custnum and
              fixedfee.hosttable = "mobsub" and
              fixedfee.keyvalue  = singlefee.keyvalue and
              fixedfee.billcode eq "PAYTERM" and
              fixedfee.financedresult = "00" no-error.

      IF NOT AVAIL fixedfee then do:
         put stream sout unformatted
            singlefee.keyvalue "|" 
            singlefee.custnum "|"
            singlefee.fmitemid "|"
            singlefee.billperiod "|"
            singlefee.billed "|"
            singlefee.sourcekey "|"
            "fixed fee missing" skip.
            j = j + 1.
         next.
   end.

   FIND FIRST bsinglefee NO-LOCK where
              bsinglefee.brand = "1" and
              bsinglefee.hosttable = singlefee.hosttable and
              bsinglefee.keyvalue = singlefee.keyvalue and
              bsinglefee.billcode = singlefee.billcode  and
              bsinglefee.billperiod = singlefee.billperiod and
              bsinglefee.amt = singlefee.amt and
              rowid(bsinglefee) ne rowid(singlefee) no-error.
   IF AVAIL bsinglefee then do:
         put stream sout unformatted
            singlefee.keyvalue "|" 
            singlefee.custnum "|"
            singlefee.fmitemid "|"
            singlefee.billperiod "|"
            singlefee.billed "|"
            singlefee.sourcekey "|"
            "Duplicate commission fee" skip.
            j = j + 1.
         next.
    end.

   if fixedfee.calcobj ne singlefee.calcobj then do:
         put stream sout unformatted
            singlefee.keyvalue "|" 
            singlefee.custnum "|"
            singlefee.fmitemid "|"
            singlefee.billperiod "|"
            singlefee.billed "|"
            singlefee.sourcekey "|"
            "PAYTERM differs SF:" singlefee.calcobj ", FF:" fixedfee.calcobj skip.
            j = j + 1.

   end.
end.

MESSAGE lcDir + lcFile1  VIEW-AS ALERT-BOX TITLE "Done".

hide frame freading no-pause.
