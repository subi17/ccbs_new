
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 

{commpaa.i}
katun = "rafaeldv".
gcBrand  = "1".
{timestamp.i}
{msisdn.i}
{tmsconst.i}

lcCLI = "622442316".

FIND FIRST msisdnnumber where
           msisdnnumber.cli = lcClI NO-LOCK NO-ERROR.

IF NOT AVAIL msisdnnumber THEN  RETURN.

FIND FIRST msisdn where msisdn.brand = gcBrand and 
      msisdn.cli = msisdnnumber.cli NO-LOCK NO-ERROR.
IF NOT AVAIL msisdn THEN RETURN.

/* check that no active subscription exists */
find mobsub where
     mobsub.cli = msisdn.cli NO-LOCK no-error.
IF AVAIL mobsub THEN RETURN.
      
/* change status  */
fMakeMSIDNHistory(recid(msisdn)).
assign msisdn.statuscode = 6.



