{commpaa.i}
assign gcbrand = "1"
       katun = "Qvantel2".
{tmsconst.i}
{cparam2.i}
{matrix.i}

def var lcInfo as char no-undo.
def var liRequest as int no-undo.
def var lcdel as char no-undo init "|".
def var lcBONOContracts as char no-undo.
def var licount as int no-undo.
def var lliSTCflag as log no-undo.

def stream sout.

lcBONOContracts = fCParamC("BONO_CONTRACTS").

{fbtc.i}

output stream sout to "/apps/yoigo/tms_support/testing/mig_conts35_39_to_CONTS32_20140707.log".

FOR EACH MobSub WHERE
         MobSub.Brand = gcBrand AND
         MobSub.CLIType = "CONTS" AND
        (MobSub.TariffBundle = "CONTS39" OR MobSub.TariffBundle = "CONTS35") NO-LOCK:

   assign liRequest = 0
          lcInfo = ""
          licount = licount + 1
          lliSTCflag = FALSE.

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
                     MsRequest.ActStamp > 20140701 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel
          "ERROR:Already iSTC " skip.
      next.
   END.

   IF CAN-FIND(FIRST MsRequest WHERE
                     MsRequest.MsSeq = MobSub.MsSeq AND
                     MsRequest.Reqtype = 81 AND
                     MsRequest.ReqStatus = 2 AND
                     MsRequest.ActStamp > 20140701 AND
                     LOOKUP(MsRequest.ReqCparam2,lcBONOContracts) = 0 NO-LOCK) THEN DO:
      put stream sout unformatted MobSub.CLI lcdel MobSub.TariffBundle lcdel
          "ERROR:Already iSTC " skip.
      next.
   END.

   liRequest = fBundleChangeRequest(MobSub.MsSeq,
                                    MobSub.TariffBundle, 
                                    "CONTS32",
                                    (IF lliSTCflag THEN 20140801 ELSE 20140701),
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

output stream sout close.
