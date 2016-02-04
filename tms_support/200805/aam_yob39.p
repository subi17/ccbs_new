{Syst/testpaa.i}
katun = "ari".
{Func/timestamp.i}

def buffer breq    for msrequest.
def buffer bupdreq for msrequest.

def var i          as int  no-undo.
def var ldtactdate as date no-undo.
def var litime     as int  no-undo.
def var lcresult   as char no-undo.

DEF VAR liDaysPassed  AS INT  NO-UNDO.
DEF VAR ldCoefficient AS DECI NO-UNDO.

def stream slog.
output stream slog to /apps/snet/200805/aam_yob39.log append.

for each msrequest no-lock where
         brand = "1" and
         reqtype = 0 and
         actstamp >= 20080501 and
         reqcparam1 ne "tarj3",
   first breq no-lock where
         breq.origrequest = msrequest.msrequest and
         breq.reqtype = 9 and
         breq.reqcparam3 = "term18":

         i = i + 1.   
         disp i format ">>9"
              msrequest.cli        format "x(12)" 
              msrequest.custnum 
              msrequest.actstamp
              msrequest.reqcparam1 format "x(8)" column-label "From"
              msrequest.reqcparam2 format "x(8)" column-label "To"
              breq.createfees
              can-find(first singlefee where
                           singlefee.brand = "1" and
                           singlefee.hosttable = "mobsub" and
                           singlefee.keyvalue = string(msrequest.msseq) and
                           singlefee.billed = false and
                           singlefee.billcode = "termperiod"). 
          
         if msrequest.reqcparam1 begins "cont" then do:
            find bupdreq where recid(bupdreq) = recid(breq)
                exclusive-lock.
            bupdreq.createfees = true.

            fsplitts(breq.actstamp,
                     output ldtactdate,
                     output litime).
                     
            find first dccli no-lock where
                       dccli.brand = "1" and
                       dccli.msseq = breq.msseq and
                       dccli.dcevent = "term18" no-error.
            if not available dccli then do:
               message "missing dccli:" breq.cli 
               view-as alert-box.
               next.
            end.
            
            liDaysPassed = ldtActDate - DCCLI.ValidFrom.
            
            ldCoefficient = 1 - ((liDaysPassed + 1) / 547).
            IF ldCoefficient < 0 THEN ldCoefficient = 0.

            RUN Mc/creasfee (MsRequest.CustNum,
                          MsRequest.MsSeq,
                          ldtActDate,
                          "MobSub",
                          "TERM_PERIOD",
                          1,
                          /* memo   */
                          DCCLI.DCEvent + " terminated " + 
                             STRING(ldtActDate,"99.99.9999") +
                             "¤¤¤" + STRING(ldCoefficient),
                          FALSE,          /* no messages to screen */
                          OUTPUT lcResult).
         end.
         
         else do:

            find bupdreq where recid(bupdreq) = recid(breq)
                exclusive-lock.
            bupdreq.createfees = false.

            for each singlefee exclusive-lock where
                     singlefee.brand = "1" and
                     singlefee.hosttable = "mobsub" and
                     singlefee.keyvalue = string(msrequest.msseq) and
                     singlefee.billed = false and
                     singlefee.billcode = "termperiod":
               export stream slog singlefee.
               delete singlefee.
            end.
        end.      

end.