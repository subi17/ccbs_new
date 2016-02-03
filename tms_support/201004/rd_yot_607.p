DEFINE VARIABLE llSimulated AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cCLI AS CHARACTER NO-UNDO. 

{Syst/commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{Func/timestamp.i}
{Func/msisdn.i}

   llSimulated = FALSE.
   cCLI = "622188797".
   FIND FIRST msisdnnumber where
      msisdnnumber.cli = cClI NO-LOCK.
      
   FIND FIRST msisdn where msisdn.brand = gcBrand and 
      msisdn.cli = msisdnnumber.cli NO-LOCK NO-ERROR.

   /* check that no active subscription exists */
   find mobsub where
        mobsub.cli = msisdn.cli NO-LOCK no-error.
   IF AVAIL mobsub THEN DO:
       DISPLAY "active subscription found" skip.
       RETURN.
   END.
   /* change status */
   IF NOT llSimulated THEN DO:
          fMakeMSIDNHistory(recid(msisdn)).
          assign
             msisdn.statuscode = 2
             msisdn.CustNum = 268112.
   END.



