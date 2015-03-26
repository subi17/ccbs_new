{commpaa.i}
katun = "Qvantel".
gcBrand = "1".

{fbankdata.i}
{fctchange.i}
{fmakemsreq.i}
{fcharge_comp_loaded.i}
{tmsconst.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcCliType AS CHARACTER NO-UNDO init "TARJ".
DEFINE VARIABLE pcBankAcc AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pdActivation AS DECIMAL NO-UNDO. 
pdActivation = 20110101.10800.

DEFINE VARIABLE pdeCharge AS DECIMAL NO-UNDO. 
DEFINE VARIABLE liCreditCheck AS INTEGER NO-UNDO. 
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcInfo AS CHARACTER NO-UNDO. 

def stream sout.
output stream sout to as_yot1087.log append.
/*
put stream sout unformatted "SUBSC.ID|MSISDN|RESULT" skip. 
*/
LOOPPI:
FOR EACH mobsub where
         mobsub.brand = gcBrand AND
         mobsub.clitype = "TARJ2" NO-LOCK:
   
   i = i + 1.
   if i <= 25 then NEXT LOOPPI. 
 /*  if i > 25 then leave LOOPPI.  */
   disp i.
   pause 0.
   
   find first msrequest NO-LOCK where
        msrequest.msseq = mobsub.msseq and
        msrequest.reqtype = 18 and
        lookup(string(msrequest.reqstatus),"2,4,9") = 0 no-error.
   IF AVAIL msrequest then do:
      put stream sout unformatted mobsub.msseq "|" mobsub.cli
         "|SKIPPED: Ongoing termination request" skip.
      next LOOPPI.
   END.
   
   find first msrequest NO-LOCK where
        msrequest.msseq = mobsub.msseq and
        msrequest.reqtype = 0 and
        lookup(string(msrequest.reqstatus),"2,4,9") = 0 no-error.
   IF AVAIL msrequest then do:
      put stream sout unformatted mobsub.msseq "|" mobsub.cli
         "|SKIPPED: Ongoing STC request" skip.
      next LOOPPI.
   END.
   
   /* Various validations */
   IF fValidateMobTypeCh(output lcError) NE 0 THEN DO:
      put stream sout unformatted mobsub.msseq "|" mobsub.cli 
         "|ERROR:" lcError skip.
      next LOOPPI.
   END.

   IF fValidateNewCliType(input pcCliType, output lcError) NE 0 THEN DO:
      put stream sout unformatted mobsub.msseq "|" mobsub.cli
         "|ERROR:" lcError skip.
      next LOOPPI.
   END.

   /* Check if credit check is needed */
   liCreditcheck = 0.
   find customer where customer.custnum = mobsub.custnum NO-LOCK.


   liRequest = fCTChangeRequest(MobSub.msseq,
                   pcCliType,
                   "",  /* customer.BankAcct, */ 
                        /* validation is already done in newton */
                   pdActivation,
                   liCreditCheck,  /* 0 = Credit check ok */
                   "",
                   "" /* pcSalesman */,
                   (pdeCharge > 0),
                   TRUE, /* send sms */
                   "",
                   pdeCharge,
                   {&REQUEST_SOURCE_SCRIPT},
                   OUTPUT lcInfo).

   IF liRequest = 0 THEN DO:
      put stream sout unformatted mobsub.msseq "|" mobsub.cli
         "|ERROR: Request creation failed: " + lcInfo skip.
      next LOOPPI.
   END.
   
   put stream sout unformatted mobsub.msseq "|" mobsub.cli
      "|OK: " liRequest skip. 
END.

disp i.
