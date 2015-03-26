
{commpaa.i}
katun = "rafaeldv".
gcBrand = "1".

{timestamp.i}
{tmsconst.i}

 DEFINE VARIABLE pcSearch AS CHARACTER NO-UNDO.
 DEFINE VARIABLE pcFor AS CHARACTER NO-UNDO. 
 DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.

 DEFINE VARIABLE ldate2 AS DATE NO-UNDO.
 DEFINE VARIABLE ldTSLock AS DECIMAL NO-UNDO.

 ldate2 = DATE(07,25,2009).
 ldTSLock =  fHMS2TS(ldate2,"00:00:00").

 pcSearch = "63338".

 pcFor = {&MSISDN_STOCK_ONLINE}.
 ldTS =  fHMS2TS(TODAY,"00:00:00").

 pcSearch = "*" + pcSearch + "*".

 FIND FIRST msisdn NO-LOCK
      WHERE MSISDN.Brand EQ "1"
      AND msisdn.statuscode EQ 1
      AND msisdn.LockedTo LT ldTS
      AND MSISDN.ValidTo GE ldTS
      AND MSISDN.POS EQ pcFor
      AND msisdn.cli MATCHES pcSearch NO-ERROR.

  IF AVAIL MSISDN THEN DO:
   
     DISPLAY MSISDN.CLI.
    
    FIND CURRENT MSISDN EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN MSISDN.LockedTo = ldTSLock.
  
  END.

  RELEASE MSISDN.

 IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.
