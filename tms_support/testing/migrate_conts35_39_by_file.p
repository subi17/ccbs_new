{Syst/commpaa.i}
assign gcbrand = "1"
       katun = "Qvantel2".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/matrix.i}

def var lcInfo as char no-undo.
def var liRequest as int no-undo.
def var lcdel as char no-undo init "|".
def var lcBONOContracts as char no-undo.
def var licount as int no-undo.
def var lcline as char no-undo.

def stream sout.

lcBONOContracts = fCParamC("BONO_CONTRACTS").

{Func/fbtc.i}

output stream sout to "/apps/yoigo/tms_support/testing/mig_conts35_39_to_CONTS32_20140604_by_file.log" append.
input from "/apps/yoigo/tms_support/testing/rehandle_mig.txt".

repeat :

import unformatted lcline.

FOR FIRST MobSub WHERE
          MobSub.Brand = gcBrand AND
          MobSub.CLI = entry(1,lcline,"|") NO-LOCK:

   assign liRequest = 0
          lcInfo = "".
          licount = licount + 1.

   status default string(licount).

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.Reqtype = 0 AND
               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel "ERROR:Already pending STC request" skip.
      next.
   END.

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.Reqtype = 81 AND
               LOOKUP(STRING(MsRequest.ReqStatus),"2,4,9,99") = 0 AND
               LOOKUP(MsRequest.ReqCparam2,lcBONOContracts) = 0 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel "ERROR:Already pending STC request" skip.
      next.
   END.

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.Reqtype = 0 AND
                     MsRequest.ReqStatus = 2 AND
                     MsRequest.ActStamp > 20140601 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel "ERROR:Already iSTC handled" skip.
      next.
   END.

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.Reqtype = 81 AND
                     MsRequest.ReqStatus = 2 AND
                     MsRequest.ActStamp > 20140601 AND
                     LOOKUP(MsRequest.ReqCparam2,lcBONOContracts) = 0 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel "ERROR:Already iSTC handled" skip.
      next.
   END.

   liRequest = fBundleChangeRequest(MobSub.MsSeq,
                                    MobSub.TariffBundle, 
                                    "CONTS32",
                                    20140601,
                                    {&REQUEST_SOURCE_SCRIPT},
                                    katun,
                                    TRUE,
                                    0,
                                    FALSE,
                                    FALSE,
                                    FALSE, /* extend terminal contract */
                                    OUTPUT lcInfo).

   IF liRequest = 0 THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel "ERROR:Request creation failed: " + lcInfo skip.
      next.
   END.

   put stream sout unformatted mobsub.cli "|" mobsub.tariffbundle "|Done"  skip.

end.
end.
output stream sout close.
