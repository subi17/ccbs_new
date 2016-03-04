USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpTimeBased.
USING HPD.HPDTime.
USING HPD.HPDCommon.
USING HPD.DumpBase.

{Syst/tmsconst.i}
{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.RerateDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE VARIABLE lhDBConfig    AS HANDLE             NO-UNDO.
   DEFINE PRIVATE VARIABLE lhDBConfQuery AS HANDLE             NO-UNDO.
   DEFINE PRIVATE VARIABLE lcBufferDB    AS CHARACTER EXTENT 2 NO-UNDO.
   DEFINE PRIVATE VARIABLE lhBuffer      AS HANDLE    EXTENT 2 NO-UNDO.
   DEFINE PRIVATE VARIABLE objDumpBase2  AS CLASS HPD.DumpBase NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTableName   AS CHARACTER          NO-UNDO.
   DEFINE PRIVATE VARIABLE liCurrentDB   AS INTEGER            NO-UNDO.

   DEFINE PRIVATE BUFFER lbEDRHistory     FOR EDRHistory.


   CONSTRUCTOR PUBLIC RerateDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      lcTableName = "MobCDR".

      mConnectToTwoLatest().

      lhMainTable = BUFFER lbEDRHistory:HANDLE.
      
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mUseTable(lhBuffer[1]).

      objDumpBase:mUpdateDumpHeader(3,"",lcTableName).
      objDumpBase:mUpdateDumpHeader(4,"","MODIFY").

      objDumpBase:mVerifyDumpFileData().

      IF VALID-HANDLE(lhBuffer[2])
      THEN DO:
         objDumpBase2 = NEW DumpBase(iiDumpID, THIS-OBJECT).
         objDumpBase2:mUseTable(lhMainTable).
         objDumpBase2:mUseTable(lhBuffer[2]).

         objDumpBase2:mUpdateDumpHeader(1,"","").
         objDumpBase2:mUpdateDumpHeader(2,"","").
         objDumpBase2:mUpdateDumpHeader(3,"",lcTableName).
         objDumpBase2:mUpdateDumpHeader(4,"","MODIFY").

         &IF DEFINED(OLD_HEADER_VERSION) &THEN
            /* RECID contains an empty value */
            objDumpBase2:mUpdateDumpHeader(5,"","").
            /* Change keyvalue from position 11 to position 6 */
            objDumpBase2:mModifyDumpHeaderPosOrder(11, 6).
            /* Convert datetime-tz to datetime and put it to position 7 */
            objDumpBase2:mUpdateDumpHeader(7,"",STRING(DATETIME(DATE(CAST(objDumpBase, DumpTimeBased):objHPDTime:CurrLocalTZTime)),MTIME(CAST(objDumpBase, DumpTimeBased):objHPDTime:CurrLocalTZTime))).
         &ELSE
            objDumpBase2:mUpdateDumpHeader(5,"", ISO-DATE(CAST(objDumpBase, DumpTimeBased):objHPDTime:CurrLocalTZTime)).
         &ENDIF

         objDumpBase2:mVerifyDumpFileData().
      END.

      mProcessCDR(INPUT CAST(objDumpBase, DumpTimeBased)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.
   

   DESTRUCTOR PUBLIC RerateDump( ):

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      IF VALID-OBJECT(objDumpBase2)
      THEN DELETE OBJECT objDumpBase2.

      DO lii = 1 TO 2:

         IF VALID-HANDLE(lhBuffer[lii])
         THEN DELETE OBJECT lhBuffer[lii].

         IF CONNECTED(lcBufferDB[lii])
         THEN DISCONNECT VALUE(lcBufferDB[lii]).

      END.  

   END DESTRUCTOR.   


   METHOD PRIVATE VOID mConnectToTwoLatest():

      DEFINE VARIABLE lii             AS INTEGER   NO-UNDO.
      DEFINE VARIABLE llOK            AS LOGICAL   NO-UNDO.   
      DEFINE VARIABLE lcDBConfigTable AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcQuery         AS CHARACTER NO-UNDO.
      
      ASSIGN
         lcDBConfigTable = "DBConfig"
         lcQuery = "FOR EACH &1 NO-LOCK WHERE " +
                   "&1.Brand     = '1' AND " +
                   "&1.TableName = &2  AND " +
                   "&1.DBState   < 2 " +
                   "BY &1.ToDate DESC"
         .

      CREATE BUFFER lhDBConfig FOR TABLE lcDBConfigTable.
      CREATE QUERY lhDBConfQuery.
      lhDBConfQuery:QUERY-PREPARE(SUBSTITUTE(lcQuery, lcDBConfigTable, lcTableName)).
      
      lhDBConfQuery:QUERY-OPEN().
      
      DO WHILE TRUE:
         
         llOK = lhDBConfQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process dbconfig query", 1).
         
         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.
         
         lii = lii + 1.

         lcBufferDB[lii] = lhDBConfig::DBConnName.

         IF CONNECTED(lcBufferDB[lii])
         THEN DISCONNECT VALUE(lcBufferDB[lii]).

         CONNECT VALUE(lhDBConfig::DirectConnect + " -ld " + lcBufferDB[lii]) NO-ERROR.

         IF ERROR-STATUS:ERROR = TRUE
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to " + lcBufferDB[lii] + " database located in " + lhDBConfig::DirectConnect, 1).
         
         CREATE BUFFER lhBuffer[lii] FOR TABLE lcBufferDB[lii] + "." + lcTableName.

         IF lii >= 2
         THEN LEAVE.

      END.

      FINALLY:

         IF VALID-HANDLE(lhDBConfQuery)
         THEN DO:
            lhDBConfQuery:QUERY-CLOSE().
            DELETE OBJECT lhDBConfQuery.
         END.

         IF VALID-HANDLE(lhDBConfig)
         THEN DELETE OBJECT lhDBConfig.            

      END FINALLY.

   END METHOD.       
      

   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN lhBuffer[liCurrentDB]::MSCID + objDumpBase:lcKeyDelimiter +
                          STRING(lhBuffer[liCurrentDB]::DtlSeq) + objDumpBase:lcKeyDelimiter +
                          ISO-DATE(lhBuffer[liCurrentDB]::DateSt).
         WHEN "DataInDataOut"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhBuffer[liCurrentDB]::DataIn + lhBuffer[liCurrentDB]::DataOut).
         OTHERWISE RETURN "". 
         
      END CASE.
   
   END METHOD.

   
   METHOD PRIVATE VOID mProcessCDR
      (INPUT objDumpTBC AS CLASS DumpTimeBased):

      DEFINE VARIABLE ldaFromDate AS DATE      NO-UNDO.
      DEFINE VARIABLE ldaFromTime AS INTEGER   NO-UNDO.
      DEFINE VARIABLE ldaToDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE ldaToTime   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE llFound     AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lcQuery     AS CHARACTER NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDumpTBC:ldeFromTS)
         ldaFromTime = HPDTime:mMake2Time(objDumpTBC:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDumpTBC:ldeToTS)
         ldaToTime   = HPDTime:mMake2Time(objDumpTBC:ldeToTS)
         lcQuery     = "WHERE CLI       = &1 AND " +
                             "DateSt    = &2 AND " +
                             "TimeStart = &3 AND " +
                             "DtlSeq    = &4"
         .

      FOR EACH lbEDRHistory FIELDS (Brand UpdateDate UpdateTime CLI DateSt TimeStart DtlSeq) NO-LOCK WHERE
         lbEDRHistory.Brand       = "1"         AND
         lbEDRHistory.UpdateDate >= ldaFromDate AND
         lbEDRHistory.UpdateDate <= ldaToDate   AND
         lbEDRHistory.UpdateTime >= ldaFromTime AND
         lbEDRHistory.UpdateTime <= ldaToTime
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.
         
         DO liCurrentDB = 1 TO 2:
            llFound = lhBuffer[liCurrentDB]:FIND-FIRST(SUBSTITUTE(lcQuery,
                                                          QUOTER(lbEDRHistory.CLI),
                                                          lbEDRHistory.DateSt,
                                                          lbEDRHistory.TimeStart,
                                                          lbEDRHistory.DtlSeq), NO-LOCK) NO-ERROR.
            IF llFound
            THEN DO:
               liEvents = liEvents + 1.

               IF liCurrentDB = 1
               THEN objDumpTBC:mWriteALine().
               ELSE objDumpTBC:mWriteALine(objDumpBase2:mGetData()).
               
               LEAVE.
            END.
         END.
         
         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY lcTableName FORMAT "X(20)" NO-LABEL liEvents NO-LABEL 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.
         
      END.

      IF NOT SESSION:BATCH
      THEN HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.