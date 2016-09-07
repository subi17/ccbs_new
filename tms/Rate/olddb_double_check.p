/* olddb_double_check.p     23.06.11/aam 
*/

{Syst/commali.i}
{Func/direct_dbconnect.i}
{Rate/error_codes.i}
{Rate/rate_roamzone.i}

DEF INPUT PARAMETER  icCLI       AS CHAR NO-UNDO.
DEF INPUT PARAMETER  idaDateSt   AS DATE NO-UNDO.
DEF INPUT PARAMETER  iiTimeSt    AS INT  NO-UNDO.
DEF INPUT PARAMETER  iiBillDur   AS INT  NO-UNDO.
DEF INPUT PARAMETER  icGSMBNr    AS CHAR NO-UNDO.
DEF INPUT PARAMETER  iiSpoCMT    AS INT  NO-UNDO.
DEF INPUT PARAMETER  idCCharge   AS DEC  NO-UNDO.
DEF INPUT PARAMETER  icMSCID     AS CHAR  NO-UNDO.
DEF INPUT PARAMETER  icEventType AS CHAR  NO-UNDO.
DEF INPUT PARAMETER  icCDRId     AS CHAR  NO-UNDO.
DEF INPUT PARAMETER  icCallIdNum AS CHAR  NO-UNDO.
DEF INPUT PARAMETER  icApn       AS CHAR  NO-UNDO.
DEF INPUT PARAMETER  icOldDB     AS CHAR NO-UNDO.
DEF INPUT PARAMETER  icOldCDR    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER olDouble    AS LOG  NO-UNDO.

DEF VAR ldaOldDb           AS DATE NO-UNDO.
DEF VAR liCurrent          AS INT  NO-UNDO.
DEF VAR lhOldCDR           AS HANDLE NO-UNDO.
DEF VAR lhFind             AS HANDLE NO-UNDO.
DEF VAR lcFind             AS CHAR NO-UNDO. 
DEF VAR llAlreadyConnected AS LOG  NO-UNDO.


olDouble = FALSE.

IF NOT CONNECTED(icOldDB) THEN DO:

   llAlreadyConnected = FALSE. 
   fInitializeConnectTables(icOldCDR,"old").
   RUN pDirectConnect2Dbs(gcBrand,
                          "old",
                          idaDateSt,
                          idaDateSt).
   IF RETURN-VALUE BEGINS "ERROR" THEN 
      RETURN RETURN-VALUE.
END.
ELSE llAlreadyConnected = TRUE.

IF NOT CONNECTED(icOldDB) THEN 
   RETURN "ERROR:Old DB not connected".

icOldCDR = icOldDB + "." + icOldCDR.

CREATE BUFFER lhOldCDR FOR TABLE icOldCDR.
    
CREATE QUERY lhFind.
lhFind:SET-BUFFERS(lhOldCDR).

lcFind = 'FOR EACH ' + icOldCDR + ' NO-LOCK USE-INDEX CLI WHERE ' +
          icOldCDR + '.CLI         = ' + '"' + icCLI + '"'                    + ' AND ' +
          icOldCDR + '.DateSt      = ' + STRING(idaDateSt)                    + ' AND ' +
          icOldCDR + '.TimeStart   = ' + STRING(iiTimeSt)                     + ' AND ' +
          icOldCDR + '.BillDur     = ' + STRING(iiBillDur)                    + ' AND ' +
          icOldCDR + '.GsmBnr      = ' + '"' + icGsmBnr + '"'                 + ' AND ' +
          icOldCDR + '.SpoCmt      = ' + STRING(iiSpoCMT)                     + ' AND ' +
          icOldCDR + '.CCharge     = ' + STRING(idCCharge)                    + ' AND ' +
          icOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_CALL})     + ' AND ' +
          icOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_CCGW_CDR}) + ' AND ' +
          icOldCDR + '.ErrorCode  NE ' + STRING({&CDR_ERROR_DOUBLE_DATA_CDR}) + '     ' .
          /* in separate db so no need to check recid */

lhFind:QUERY-PREPARE(lcFind).
lhFind:QUERY-OPEN.

REPEAT:
   lhFind:GET-NEXT.
   IF lhFind:QUERY-OFF-END THEN LEAVE.
   
   IF icMSCID EQ "CCGW" AND
      lhOldCDR::MSCID EQ "CCGW" AND
      icCDRId NE fGetMcdrDtlValue(lhOldCDR::Datest,
                                  lhOldCDR::Dtlseq,
                                  "External running index")
      THEN NEXT.
   ELSE IF icMSCID EQ  "POST" AND
           lhOldCDR::MSCID EQ "POST" AND
           icEventType EQ "SMS" AND
           lhOldCDR::EventType EQ "SMS" AND
           icCallIdNum NE fGetMcdrDtlValue(lhOldCDR::Datest,
                                           lhOldCDR::Dtlseq,
                                           "Call identification number")
      THEN NEXT.
   ELSE IF ((icMSCID EQ "POSTD" AND lhOldCDR::MSCID EQ "POSTD") OR
            (icMSCID EQ "PRE" AND lhOldCDR::MSCID EQ "PRE" AND
             icEventType EQ "GPRS" AND lhOldCDR::EventType EQ "GPRS")) AND
             icApn NE fGetMcdrDtlValue(lhOldCDR::Datest,
                                       lhOldCDR::Dtlseq,
                                       "Access point name NI")
      THEN NEXT.

   olDouble = TRUE.
   LEAVE.
END.   
      
lhFind:QUERY-CLOSE.
  
DELETE OBJECT lhFind NO-ERROR.
DELETE OBJECT lhOldCDR NO-ERROR.

IF NOT llAlreadyConnected AND CONNECTED(icOldDB) THEN 
   DISCONNECT VALUE(icOldDB) NO-ERROR.

RETURN "".

