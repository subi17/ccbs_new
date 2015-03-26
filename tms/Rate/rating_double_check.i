/* rating_double_check.i    23.06.11/aam 
   do a double call check for a single cdr
*/

{commali.i}
{error_codes.i}

/* post/pre dbs are renewed at the same time -> one variable is enough */
DEF VAR ldaYesterday AS DATE NO-UNDO INIT ?.

DEF TEMP-TABLE ttDBConfig NO-UNDO
   FIELD TableName AS CHAR
   FIELD FromDate  AS DATE
   FIELD ToDate    AS DATE
   INDEX TableName TableName ToDate.

FOR EACH DBConfig NO-LOCK WHERE
         DBConfig.Brand = gcBrand AND
         DBConfig.TableName = "MobCDR" AND
         DBConfig.DBState <= 1:
   CREATE ttDBConfig.
   BUFFER-COPY DBConfig TO ttDBConfig.
END.
FOR EACH DBConfig NO-LOCK WHERE
         DBConfig.Brand = gcBrand AND
         DBConfig.TableName = "PrepCDR" AND
         DBConfig.DBState <= 1:
   CREATE ttDBConfig.
   BUFFER-COPY DBConfig TO ttDBConfig.
END.


FUNCTION fIsDoubleCall RETURNS LOGICAL
   (icCaller  AS CHAR,
    irChecked AS RECID):

   DEF VAR lcOldDB      AS CHAR   NO-UNDO.
   DEF VAR llDouble     AS LOG    NO-UNDO.
   DEF VAR lcTableName  AS CHAR   NO-UNDO.
   DEF VAR llCheckOld   AS LOG    NO-UNDO.
   
   DEF BUFFER bPostDouble FOR MobCDR.
   DEF BUFFER bPreDouble  FOR PrepCDR.
   
   ASSIGN
      llDouble   = FALSE
      llCheckOld = FALSE.
   
   IF ttCall.MSCID = "NRTRDE" THEN RETURN FALSE.

   IF ttCall.MSCID = "CCGW" THEN DO:
      IF ttCall.PPFlag = 0 THEN
         FOR EACH bPostDouble NO-LOCK USE-INDEX CLI WHERE
                  bPostDouble.CLI       = ttCall.CLI AND
                  bPostDouble.DateSt    = ttCall.DateSt AND
                  bPostDouble.TimeStart = ttCall.TimeStart AND
                  bPostDouble.BillDur   = ttCall.BillDur AND
                  bPostDouble.GsmBnr    = ttCall.GsmBnr AND
                  bPostDouble.SpoCMT    = ttCall.SpoCMT AND
                  bPostDouble.CCharge   = ttCall.CCharge AND
                  bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
                  bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
                  bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR} AND
                  bPostDouble.ErrorCode NE 8040 AND
                  RECID(bPostDouble)    NE irChecked:
            IF bPostDouble.MSCID = "CCGW" THEN DO:
               IF icCaller = "rerate" AND ttCall.CdrId = "" THEN
                  ttCall.CdrId = fGetMcdrDtlValue(ttCall.Datest,
                                                  ttCall.Dtlseq,
                                                  "External running index").
               IF ttCall.CdrId = fGetMcdrDtlValue(bPostDouble.Datest,
                                                  bPostDouble.Dtlseq,
                                                  "External running index")
                  THEN llDouble = TRUE.
            END.
            ELSE llDouble = TRUE.
            IF llDouble THEN LEAVE.
         END. /* FOR EACH bPostDouble USE-INDEX CLI WHERE */
      ELSE
         FOR EACH bPreDouble NO-LOCK USE-INDEX CLI WHERE
                  bPreDouble.CLI       = ttCall.CLI AND
                  bPreDouble.DateSt    = ttCall.DateSt AND
                  bPreDouble.TimeStart = ttCall.TimeStart AND
                  bPreDouble.BillDur   = ttCall.BillDur AND
                  bPreDouble.GsmBnr    = ttCall.GsmBnr AND
                  bPreDouble.SpoCMT    = ttCall.SpoCMT AND
                  bPreDouble.CCharge   = ttCall.CCharge AND
                  bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
                  bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
                  bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR} AND
                  RECID(bPreDouble)    NE irChecked:
            IF bPreDouble.MSCID = "CCGW" THEN DO:
               IF icCaller = "rerate" AND ttCall.CdrId = "" THEN
                  ttCall.CdrId = fGetMcdrDtlValue(ttCall.Datest,
                                                  ttCall.Dtlseq,
                                                  "External running index").
               IF ttCall.CdrId = fGetMcdrDtlValue(bPreDouble.Datest,
                                                  bPreDouble.Dtlseq,
                                                  "External running index")
                  THEN llDouble = TRUE.
            END.
            ELSE llDouble = TRUE.
            IF llDouble THEN LEAVE.
         END. /* FOR EACH bPreDouble USE-INDEX CLI WHERE */
   END. /* IF ttCall.MSCID = "CCGW" THEN DO: */
   ELSE DO:
      IF ttCall.PPFlag = 0 THEN DO:

         IF ttCall.MSCID = "POST" AND
            ttCall.EventType EQ "SMS" THEN
            FOR EACH bPostDouble USE-INDEX CLI WHERE
                     bPostDouble.CLI       = ttCall.CLI AND
                     bPostDouble.DateSt    = ttCall.DateSt AND
                     bPostDouble.TimeStart = ttCall.TimeStart AND
                     bPostDouble.BillDur   = ttCall.BillDur AND
                     bPostDouble.GsmBnr    = ttCall.GsmBnr AND
                     bPostDouble.SpoCMT    = ttCall.SpoCMT AND
                     bPostDouble.CCharge   = ttCall.CCharge AND
                     bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
                     bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
                     bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR} AND
                     bPostDouble.ErrorCode NE 8040 AND
                     RECID(bPostDouble) NE irChecked:
               IF bPostDouble.MSCID = "POST" AND
                  bPostDouble.EventType EQ "SMS" THEN DO:
                  IF icCaller = "rerate" AND ttCall.CallIdNum = "" THEN
                     ttCall.CallIdNum = fGetMcdrDtlValue(ttCall.Datest,
                                                     ttCall.Dtlseq,
                                                   "Call identification number").
                  IF ttCall.CallIdNum = fGetMcdrDtlValue(bPostDouble.Datest,
                                                     bPostDouble.Dtlseq,
                                                   "Call identification number")
                     THEN llDouble = TRUE.
               END.
               ELSE llDouble = TRUE.
               IF llDouble THEN LEAVE.
            END.
         ELSE
         llDouble = CAN-FIND(FIRST bPostDouble USE-INDEX CLI WHERE
            bPostDouble.CLI       = ttCall.CLI AND
            bPostDouble.DateSt    = ttCall.DateSt AND
            bPostDouble.TimeStart = ttCall.TimeStart AND
            bPostDouble.BillDur   = ttCall.BillDur AND
            bPostDouble.GsmBnr    = ttCall.GsmBnr AND
            bPostDouble.SpoCMT    = ttCall.SpoCMT AND
            bPostDouble.CCharge   = ttCall.CCharge AND
            bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
            bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
            bPostDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR} AND
            bPostDouble.ErrorCode NE 8040 AND
            RECID(bPostDouble) NE irChecked).
      END.   
      ELSE
      llDouble = CAN-FIND(FIRST bPreDouble USE-INDEX CLI WHERE
         bPreDouble.CLI       = ttCall.CLI AND
         bPreDouble.DateSt    = ttCall.DateSt AND
         bPreDouble.TimeStart = ttCall.TimeStart AND
         bPreDouble.BillDur   = ttCall.BillDur AND
         bPreDouble.GsmBnr    = ttCall.GsmBnr AND
         bPreDouble.SpoCMT    = ttCall.SpoCMT AND
         bPreDouble.CCharge   = ttCall.CCharge AND
         bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CALL} AND
         bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_CCGW_CDR} AND
         bPreDouble.ErrorCode NE {&CDR_ERROR_DOUBLE_DATA_CDR} AND
         RECID(bPreDouble) NE irChecked).
   END. /* ELSE DO: */

   /* check from old dbs also */
   IF NOT llDouble AND ttCall.DateSt < TODAY AND ttCall.PPFlag = 0 and
      LOOKUP(ttCall.MSCID,"tap3,ccgw,nrtrde") = 0
   THEN DO:
      IF ttCall.PPFlag = 0 THEN ASSIGN
         lcOldDb     = "OldMCDR"
         lcTableName = "MobCDR".
      ELSE ASSIGN
         lcOldDb     = "OldPrepCDR"
         lcTableName = "PrepCDR".

      IF CONNECTED(lcOldDb) THEN llCheckOld = TRUE.

      ELSE DO:
         /* most common case, reading yesterday's tickets after midnight */
         IF icCaller = "online" AND 
            ttCall.DateSt = TODAY - 1 AND ldaYesterday = TODAY - 1 THEN
            llCheckOld = FALSE.
            
         /* when have dbs been renewed, is there cause to check older dbs */
         ELSE DO:
            FIND FIRST ttDBConfig WHERE
                       ttDBConfig.TableName = lcTableName AND
                       ttDBConfig.ToDate >= ttCall.DateSt AND
                       ttDBConfig.FromDate <= ttCall.DateSt NO-LOCK NO-ERROR.
            IF AVAILABLE ttDBConfig AND ttDBConfig.ToDate < TODAY THEN 
               llCheckOld = TRUE.
            ELSE IF ttCall.DateSt = TODAY - 1 THEN 
               ldaYesterday = TODAY - 1.
         END.      
      END.
             
      IF llCheckOld THEN DO:
           
         IF icCaller = "rerate" AND ttCall.CdrId = "" THEN
            ttCall.CdrId = fGetMcdrDtlValue(ttCall.Datest,
                                            ttCall.Dtlseq,
                                            "External running index").

         IF ttCall.MSCID = "POST" AND
            ttCall.EventType EQ "SMS" AND
            icCaller = "rerate" AND
            ttCall.CallIdNum = "" THEN
            ttCall.CallIdNum = fGetMcdrDtlValue(ttCall.Datest,
                                               ttCall.Dtlseq,
                                               "Call identification number").

         RUN olddb_double_check.p(ttCall.CLI,
                                  ttCall.DateSt,
                                  ttCall.TimeStart,
                                  ttCall.BillDur,
                                  ttCall.GSMBnr,
                                  ttCall.SpoCMT,
                                  ttCall.CCharge,
                                  ttCall.MSCID,
                                  ttCall.EventType,
                                  ttCall.CDRId,
                                  ttCall.CallIdNum,
                                  lcOldDB,
                                  lcTableName,
                                  OUTPUT llDouble).
         IF RETURN-VALUE BEGINS "ERROR" THEN ASSIGN 
            llDouble = TRUE
            ttCall.ErrorCode = {&CDR_ERROR_DOUBLE_CHECK_FAILED}.
      END.                            
   END.

   IF llDouble THEN DO: 
      CASE ttCall.MSCID:
         WHEN "CCGW" THEN ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_DOUBLE_CCGW_CDR} 
                                    WHEN ttCall.ErrorCode = 0 OR
                                    ttCall.ErrorCode = 9999
            ttCall.InvSeq    = 0.
         WHEN "POSTD" THEN ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_DOUBLE_DATA_CDR} 
                                    WHEN ttCall.ErrorCode = 0 OR
                                    ttCall.ErrorCode = 9999
            ttCall.InvSeq    = 0.
         OTHERWISE ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_DOUBLE_CALL} 
                                    WHEN ttCall.ErrorCode = 0 OR
                                    ttCall.ErrorCode = 9999
            ttCall.InvSeq    = 0.
      END CASE.
   END.
   
   RETURN llDouble.
   
END FUNCTION.

