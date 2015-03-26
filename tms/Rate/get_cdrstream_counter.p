/* get_cdrstream_counter.p    09.09.09/aam 

   return counter values from one day 
*/

DEF VAR lcFlow          AS CHAR NO-UNDO.
DEF VAR lcCounterType   AS CHAR NO-UNDO.
DEF VAR ldaImportFrom   AS DATE NO-UNDO.
DEF VAR ldaImportTo     AS DATE NO-UNDO.
DEF VAR lcImportTime    AS CHAR NO-UNDO.
DEF VAR ldaImportDate   AS DATE NO-UNDO.
DEF VAR lcFlowList      AS CHAR NO-UNDO.
DEF VAR liFlowCount     AS INT  NO-UNDO.
DEF VAR lcTypeList      AS CHAR NO-UNDO.
DEF VAR liTypeCount     AS INT  NO-UNDO.
DEF VAR liCDRQty        AS INT  NO-UNDO.
DEF VAR liYear          AS INT  NO-UNDO.
DEF VAR liMonth         AS INT  NO-UNDO.

/* Parse values from -param "param1,param2,param3"

   param1 : cdr import date, DD-MM-YYYY
            OR
            cdr import month, YYYYMM
   param2 : under (within) kpi limit = 'UNDER' 
            over limit = 'OVER'
            both = 'ALL'
   param3 : which flow (POST,POSTD,PRE,TAP3,CCGW,ROAM,NRTRDE), also 'ALL'
   
*/

SESSION:DATE-FORMAT = "dmy".

IF NUM-ENTRIES(SESSION:PARAMETER) >= 3 THEN DO:
   
   /* is the 1. parameter day or month */
   lcImportTime = ENTRY(1,SESSION:PARAMETER).
   IF NUM-ENTRIES(lcImportTime,"-") = 3 THEN 
      ldaImportFrom = DATE(lcImportTime) NO-ERROR.
   ELSE DO:
      ASSIGN 
         liYear  = INTEGER(SUBSTRING(lcImportTime,1,4))
         liMonth = INTEGER(SUBSTRING(lcImportTime,5,2)) 
         NO-ERROR.
      IF liYear > 0 AND liMonth > 0 THEN DO:
         ldaImportFrom = DATE(liMonth,1,liYear) NO-ERROR.
         IF liMonth = 12 THEN 
            ldaImportTo = DATE(12,31,liYear) NO-ERROR.
         ELSE ldaImportTo = DATE(liMonth + 1,1,liYear) - 1 NO-ERROR.
      END.
   END. 
   
   ASSIGN
      lcCounterType = ENTRY(2,SESSION:PARAMETER)
      lcFlow        = ENTRY(3,SESSION:PARAMETER)
      NO-ERROR. 
END.

IF ERROR-STATUS:ERROR OR lcCounterType = "" OR 
   lcFlow = "" OR ldaImportFrom = ? THEN DO:
   PUT UNFORMATTED "Invalid input parameters" SKIP.
   QUIT.
END. 

IF ldaImportTo = ? THEN ldaImportTo = ldaImportFrom.

IF lcFlow = "ALL" THEN lcFlowList = "POST,POSTD,PRE,TAP3,CCGW,ROAM,NRTRDE".
ELSE lcFlowList = lcFlow.

CASE lcCounterType:
WHEN "UNDER" THEN lcTypeList = "KPIHTU".
WHEN "OVER"  THEN lcTypeList = "KPIHTO".
WHEN "ALL"   THEN lcTypeList = "KPIHTU,KPIHTO".
OTHERWISE lcTypeList = "".
END CASE. 

DO liTypeCount = 1 TO NUM-ENTRIES(lcTypeList):

   DO liFlowCount = 1 TO NUM-ENTRIES(lcFlowList):

      DO ldaImportDate = ldaImportFrom TO ldaImportTo:

         FOR EACH CDRStreamCounter NO-LOCK WHERE
            CDRStreamCounter.Brand       = "1" AND
            CDRStreamCounter.ImportDate  = ldaImportDate AND
            CDRStreamCounter.CounterType = ENTRY(liTypeCount,lcTypeList) AND
            CDRStreamCounter.MSCID       = ENTRY(liFlowCount,lcFlowList):
            liCDRQty = liCDRQty + CDRStreamCounter.CDRQty.
         END.
      END.
      
   END.

END.
      
PUT UNFORMATTED
   "Period: "   STRING(ldaImportFrom,"99.99.9999") "-" 
                STRING(ldaImportTo,"99.99.9999")
   " Limit: " lcCounterType 
   " Flow: "  lcFlow
   " Qty: "   liCDRQty 
   SKIP.

QUIT.


