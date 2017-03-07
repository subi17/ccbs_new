/* cdrstream_counter.i    20.08.09/aam
*/

{Syst/commali.i}
{Func/cparam2.i}

DEF VAR liKPILimit AS INT NO-UNDO.

/* unit is seconds */
liKPILimit = fCParamI("KPICDRHandleTime").
IF liKPILimit = ? THEN liKPILimit = 0.


/* compare mediator's handling time to online read in time */
FUNCTION fUpdateKPICounter RETURNS LOGIC
   (icMSCID        AS CHAR,
    iiStream       AS INT,
    idtTMSTime     AS DATETIME,
    icMediatorTime AS CHAR):
    
   DEF VAR lcCounterType AS CHAR NO-UNDO.
   DEF VAR ldtFrom   AS DATETIME NO-UNDO.
   
   IF liKPILimit = 0 OR 
      LENGTH(icMediatorTime) < 14 OR
      idtTMSTime = ? THEN RETURN FALSE.
   
   ldtFrom = DATETIME(INTEGER(SUBSTRING(icMediatorTime,5,2)),
                      INTEGER(SUBSTRING(icMediatorTime,7,2)),
                      INTEGER(SUBSTRING(icMediatorTime,1,4)),
                      INTEGER(SUBSTRING(icMediatorTime,9,2)),
                      INTEGER(SUBSTRING(icMediatorTime,11,2)),
                      INTEGER(SUBSTRING(icMediatorTime,13,2))) NO-ERROR.
      
   IF ERROR-STATUS:ERROR THEN RETURN FALSE.
   
   IF INTERVAL(idtTMSTime,ldtFrom,"seconds") > liKPILimit
   THEN lcCounterType = "KPIHTO".
   ELSE lcCounterType = "KPIHTU".
   
   FIND FIRST CDRStreamCounter WHERE
              CDRStreamCounter.Brand        = gcBrand AND
              CDRStreamCounter.ImportDate   = TODAY AND
              CDRStreamCounter.CounterType  = lcCounterType AND
              CDRStreamCounter.MSCID        = icMSCID AND
              CDRStreamCounter.OnlineStream = iiStream EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE CDRStreamCounter THEN DO:
      CREATE CDRStreamCounter.
      ASSIGN 
         CDRStreamCounter.Brand        = gcBrand
         CDRStreamCounter.ImportDate   = TODAY 
         CDRStreamCounter.CounterType  = lcCounterType
         CDRStreamCounter.MSCID        = icMSCID 
         CDRStreamCounter.OnlineStream = iiStream.
   END.
   
   CDRStreamCounter.CDRQty = CDRStreamCounter.CDRQty + 1.
   
   RETURN TRUE.

END FUNCTION.


