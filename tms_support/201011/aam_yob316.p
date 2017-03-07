{Func/timestamp.i}

session:numeric-format = "european".

def var i as int no-undo.
def var j as int no-undo.

def temp-table ttcli no-undo
   field msseq as int 
   field dcevent as char
   index msseq msseq.

for each msrequest no-lock where
         msrequest.brand = "1" and
         msrequest.reqtype = 9 and
         msrequest.reqstat = 2 and
         msrequest.actstamp >= 20101031.86300 and
         msrequest.actstamp < 20101101.00100,
   first msowner no-lock where
         msowner.msseq = msrequest.msseq,
    each invoice no-lock use-index custnum where
         invoice.brand = "1" and
         invoice.custnum = msowner.invcust and
         invoice.invdate = 11/1/10 and
         invoice.invtype = 1,
   first subinvoice of invoice no-lock where
         subinvoice.msseq = msowner.msseq:

   if msrequest.donestamp > invoice.chgstamp then do:
   
      find first ttcli where ttcli.msseq = msowner.msseq no-error.
      if not available ttcli then do:
         create ttcli.
         ttcli.msseq = msowner.msseq.
      end.
      ttcli.dcevent = ttcli.dcevent + (if ttcli.dcevent > "" then "," else "")
                      + msrequest.reqcparam3.
                      
      /*                
      disp msowner.cli
           msowner.invcust
        fts2hms(msrequest.donestamp) format "x(19)"
        fts2hms(invoice.chgstamp) format "x(19)"
        msrequest.reqcparam3 format "x(12)".
      pause.
      */
      j = j + 1.
   end.
   
   i = i + 1.
   pause 0.
   disp i j with 1 down row 1.
   
end.

def stream slog.
output stream slog to /apps/yoigo/tms_support/201011/aam_yob316.log.

def var ldpenalty as dec no-undo.
def var lddata    as dec no-undo.
def var ldastart as date no-undo.
def var listart as int no-undo.

put stream slog unformatted
   "MSISDN"  chr(9)
   "Subscr.ID" chr(9)
   "Customer"  chr(9)
   "Contract"  chr(9)
   "Data usage" chr(9)
   "Penalty"   skip.

i = 0.
   
for each ttcli,
   first msowner where
         msowner.msseq = ttcli.msseq:

   assign 
      ldpenalty = 0
      lddata    = 0.
      
   if index(ttcli.dcevent,"act") = 0 and
      index(ttcli.dcevent,"term") = 0 
      then next.
          
   if index(ttcli.dcevent,"act") > 0 then do:
   
      assign
         ldastart = 10/1/10
         listart = 0.
      for first servicelimit no-lock where
                servicelimit.groupcode = "mdubact",
          first mservicelimit no-lock where
                mservicelimit.msseq = msowner.msseq and
                mservicelimit.slseq = servicelimit.slseq and
                mservicelimit.fromts >= 20101001 and
                mservicelimit.fromts < 20101101:
         fsplitts(mservicelimit.fromts,
                  output ldastart,
                  output listart).
      end.
        
      for each mobcdr no-lock use-index cli where
               mobcdr.cli = msowner.cli and
               mobcdr.datest >= ldastart and
               mobcdr.datest <= 10/31/10 and
               mobcdr.spocmt = 93:
         if mobcdr.datest = ldastart and 
            mobcdr.timest < listart then next.
            
         lddata = lddata + mobcdr.amount.      
      end.
   end.
      
   if index(ttcli.dcevent,"term") > 0 then
   for first singlefee no-lock use-index hosttable where
             singlefee.brand = "1" and 
             singlefee.hosttable = "mobsub" and
             singlefee.keyvalue = string(msowner.msseq) and
             singlefee.billed = false and
             singlefee.billperiod = 201010 and
             singlefee.feemodel begins "term":
      ldpenalty = singlefee.amt.
   end.
   

   put stream slog unformatted
      msowner.cli  chr(9)
      msowner.msseq chr(9)
      msowner.invcust chr(9)
      ttcli.dcevent chr(9)
      trim(string(lddata,"->>>>>>9.999")) chr(9)
      ldpenalty skip.

   i = i + 1.
   pause 0.
   disp i with 1 down row 10.
end.         

output stream slog close.

session:numeric-format = "american".


