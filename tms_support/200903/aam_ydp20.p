{Syst/testpaa.i}
katun = "ari".
{Func/timestamp.i}

def var lcres as char no-undo.
def var ldtactdate as date no-undo.
def var litime as int no-undo.
def var i as int no-undo.

def stream slog.
output stream slog to /apps/snet/200903/aam_ydp20.log append.

for each msrequest no-lock where
         brand = "1" and
         reqtype = 9 and
         reqstat = 2 and
         actstamp > 20090301 and
         reqcparam3 = "contdataact" and
         createfees = true:

   /*
   find first fixedfee no-lock where
              fixedfee.brand = "1" and
              fixedfee.hosttable = "mobsub" and
              fixedfee.keyvalue = string(msrequest.msseq) no-error.

   if available fixedfee then do:
      disp fixedfee.begperiod fixedfee.endperiod
           can-find(first ffitem of fixedfee).
   end.
   */

   find first singlefee no-lock where
              singlefee.brand = "1" and
              singlefee.hosttable = "mobsub" and
              singlefee.keyvalue = string(msrequest.msseq) and
              singlefee.billcode = "contdatamf" and
              singlefee.billed = false no-error.

   if available singlefee then do:
   
      disp msrequest.cli msseq actstamp 
           msrequest.custnum.
      
      disp singlefee.billperiod
           singlefee.amt
           singlefee.calcobj
           singlefee.memo[1].
           
      i = i + 1.

      find current singlefee exclusive-lock.
      export stream slog singlefee.
      delete singlefee.
       
      fsplitts(msrequest.actstamp,
               output ldtactdate,
               output litime).
               
      RUN Mc/creasfee (Msrequest.CustNum,
                    MsRequest.MsSeq,
                    ldtActDate,
                    "FeeModel",
                    "TERMCONTDATA",
                    9,
                    "CONTDATAACT" + " terminated " + 
                            STRING(ldtActDate,"99.99.9999"),
                    FALSE,              /* no messages to screen */
                    OUTPUT lcRes).
   end.
end.


disp i.
