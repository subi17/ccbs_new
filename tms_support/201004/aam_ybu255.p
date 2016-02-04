{Syst/testpaa.i}
katun = "Qvantel".

{Func/timestamp.i}
DEF VAR ldaActDate    AS DATE NO-UNDO.
DEF VAR liTime        AS INT  NO-UNDO.
def var j as int no-undo.
def var k as int no-undo.
def var lcresult as char no-undo.

def stream slog.
output stream slog to /apps/snet/201004/aam_ybu255.log append.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand     = "1" AND
         MsRequest.ReqType   = 9       AND
         MsRequest.ReqStatus = 2       AND
         MsRequest.ActStamp >= 20100331 AND
         msrequest.actstamp < 20100401 and 
         MsRequest.DoneStamp >= 20100301 AND
         MsRequest.DoneStamp <= 20100401.86399 and
         msrequest.reqcparam3 = "mdubact",
   FIRST DayCampaign NO-LOCK WHERE
         DayCampaign.Brand = "1" AND
         DayCampaign.DCEvent = MsRequest.ReqCParam3,
   first mobsub where mobsub.msseq = msrequest.msseq no-lock:

   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldaActDate,
            OUTPUT liTime).


   def var i as int no-undo.
   i = I + 1.            
            
   pause 0.
   disp i msrequest.cli msrequest.msseq msrequest.actstamp
        createfees
        msrequest.custnum.
  
   find first fixedfee where
                       fixedfee.brand = "1" and
                       fixedfee.hosttable = "mobsub" and
                       fixedfee.keyvalue = string(msrequest.msseq) and
                       fixedfee.billcode = "MDUBMF" and
                       fixedfee.begperiod = 201003 no-lock no-error.
   pause 0.
   disp available fixedfee column-label "fee".
                       
   if not available fixedfee then do:
      k = k + 1. 

      find first invoice use-index custnum where
            invoice.brand = "1" and
            invoice.custnum = msrequest.custnum and
            invoice.invdate = 4/1/10 and
            invoice.invtype = 1 no-lock no-error.
      if not available invoice then do:

            RUN Mc/creasfee (MsRequest.CustNum,
                          MsRequest.MsSeq,
                          ldaActDate,
                          "FeeModel",
                          "TERMMDUB",
                          9,
                          ?,
                          "MDUBACT terminated " + 
                           STRING(ldaActDate,"99.99.9999") +
                           "¤¤¤1",
                          FALSE,              /* no messages to screen */
                          OUTPUT lcResult).

         put stream slog unformatted
            msrequest.msseq chr(9)
            msrequest.cli  chr(9)
            msrequest.custnum chr(9)
            lcresult skip.
         j = j + 1.
      end.

   end.
                       
end.         

disp i k j.

