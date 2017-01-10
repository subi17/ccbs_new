USING Progress.Lang.*.
USING HPD.DumpHandlerWithQuery.
USING HPD.DumpTimeBased.
USING HPD.HPDTime.
USING HPD.HPDCommon.
USING HPD.DumpBase.

{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.RerateDump FINAL INHERITS DumpHandlerWithQuery:

   DEFINE PROTECTED OVERRIDE PROPERTY lcQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX UpdateDate WHERE " +
                "&1.Brand       = '1' AND " +
                "&1.UpdateDate >= &2 AND " +
                "&1.UpdateDate <= &3".
      END GET.


   DEFINE PRIVATE PROPERTY lcFirstDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX UpdateDate WHERE " +
                "&1.Brand       = '1' AND " +
                "&1.UpdateDate = &2 AND " +
                "&1.UpdateTime >= &3".
      END GET.


   DEFINE PRIVATE PROPERTY lcLastDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX UpdateDate WHERE " +
                "&1.Brand       = '1' AND " +
                "&1.UpdateDate = &2 AND " +
                "&1.UpdateTime < &3".
      END GET.


   DEFINE PRIVATE PROPERTY lcOneDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX UpdateDate WHERE " +
                "&1.Brand       = '1' AND " +
                "&1.UpdateDate = &2 AND " +
                "&1.UpdateTime >= &3 AND " +
                "&1.UpdateTime <  &4".
      END GET.


   DEFINE PRIVATE PROPERTY lcMobCDRQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "WHERE " +
                "CLI       = &1 AND " +
                "DateSt    = &2 AND " +
                "TimeStart = &3 AND " +
                "DtlSeq    = &4".
      END GET.


   DEFINE PRIVATE VARIABLE lhDBConfig    AS HANDLE              NO-UNDO.
   DEFINE PRIVATE VARIABLE lhDBConfQuery AS HANDLE              NO-UNDO.
   DEFINE PRIVATE VARIABLE lcBufferDB    AS CHARACTER EXTENT 2  NO-UNDO.
   DEFINE PRIVATE VARIABLE lhBuffer      AS HANDLE    EXTENT 2  NO-UNDO.
   DEFINE PRIVATE VARIABLE objDumpBase2  AS CLASS DumpBase      NO-UNDO.
   DEFINE PRIVATE VARIABLE lcMobCDRTable AS CHARACTER           NO-UNDO.
   DEFINE PRIVATE VARIABLE lcHeader      AS CHARACTER           NO-UNDO.
   DEFINE PRIVATE VARIABLE liCurrentDB   AS INTEGER             NO-UNDO.
   DEFINE PRIVATE VARIABLE liBufferCount AS INTEGER             NO-UNDO.
   DEFINE PRIVATE VARIABLE objDTB        AS CLASS DumpTimeBased NO-UNDO.


   CONSTRUCTOR PUBLIC RerateDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      SUPER(iiDumpID).

      /* Note: DumpHandler destructor is deleting the lhMainTable object */
      CREATE BUFFER lhMainTable FOR TABLE lcTableName.

      lcMobCDRTable = "MobCDR".

      mConnectToTwoLatest().

      /* If no db could found then we might have test or stage env.
         Lets try to connect test or stage mobcdr database... */
      IF NOT VALID-HANDLE(lhBuffer[1])
      THEN DO:
         lhBuffer[1] = mConnectDBAndGiveBuffer("/db1/mcdr",
                                               "mcdr",
                                               lcMobCDRTable).
         IF NOT VALID-HANDLE(lhBuffer[1])
         THEN RETURN ERROR NEW Progress.Lang.AppError("Couldn't found any suitable MobCDR database", 1).

         lcBufferDB[1] = "mcdr".
      END.
      
      liBufferCount = liBufferCount + 1.

      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).

      lcHeader = lcMobCDRTable + objDumpBase:lcDelimiter +
                 "&1" + objDumpBase:lcDelimiter.

      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mUseTable(lhBuffer[1]).

      /* DumpFile.cls will initially populate tablename and
         event type to field positions 1 and 2. We
         need to remove them because we are using lcHeader instead */
      objDumpBase:mDeleteDumpHeader(1).
      objDumpBase:mDeleteDumpHeader(2).

      objDumpBase:mVerifyDumpFileData().

      IF VALID-HANDLE(lhBuffer[2])
      THEN DO:
         liBufferCount = liBufferCount + 1.
         objDumpBase2 = NEW DumpBase(iiDumpID, THIS-OBJECT).
         objDumpBase2:mUseTable(lhMainTable).
         objDumpBase2:mUseTable(lhBuffer[2]).

         /* DumpFile.cls will initially populate tablename and
            event type to field positions 1 and 2. We
            need to remove them because we are using lcHeader instead */
         objDumpBase2:mDeleteDumpHeader(1).
         objDumpBase2:mDeleteDumpHeader(2).

         /* Lets use the same timestamp value for the second buffer also */
         objDumpBase2:mUpdateDumpHeader(3,"", ISO-DATE(CAST(objDumpBase, DumpTimeBased):objHPDTime:CurrLocalTZTime)).

         objDumpBase2:mVerifyDumpFileData().
      END.

      objDTB = CAST(objDumpBase, DumpTimeBased).

      mProcessEDRHistory().
      
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

         IF lcBufferDB[lii] > "" AND CONNECTED(lcBufferDB[lii])
         THEN DISCONNECT VALUE(lcBufferDB[lii]).

      END.

   END DESTRUCTOR.   


   METHOD PRIVATE HANDLE mConnectDBAndGiveBuffer
      ( icDBPath        AS CHARACTER,
        icDBName        AS CHARACTER,
        icTableName     AS CHARACTER ):

      DEFINE VARIABLE lhBuffer AS HANDLE NO-UNDO.

      IF NOT HPDCommon:mDBExist(icDBPath + "/" + icDBName)
      THEN RETURN ?.

      IF CONNECTED(icDBName)
      THEN DISCONNECT VALUE(icDBName).

      CONNECT VALUE(icDBPath + "/" + icDBName) NO-ERROR.

      IF ERROR-STATUS:ERROR = TRUE
      THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot connect to database " + icDBName + " located on directory " + icDBPath, 1).

      CREATE BUFFER lhBuffer FOR TABLE icDBName + "." + icTableName.

      RETURN lhBuffer.

   END METHOD.


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

      lhDBConfQuery:SET-BUFFERS(lhDBConfig).

      lhDBConfQuery:QUERY-PREPARE(SUBSTITUTE(lcQuery, lcDBConfigTable, QUOTER(lcMobCDRTable))).

      lhDBConfQuery:QUERY-OPEN().

      DO WHILE TRUE ON ERROR UNDO, THROW:
         
         llOK = lhDBConfQuery:GET-NEXT(NO-LOCK).

         IF llOK = ?
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process dbconfig query", 1).
         
         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.
         
         ASSIGN
            lii           = lii + 1
            lhBuffer[lii] = mConnectDBAndGiveBuffer(lhDBConfig::DirectConnect,
                                                    lhDBConfig::DBConnName,
                                                    lcMobCDRTable).

         IF NOT VALID-HANDLE(lhBuffer[lii])
         THEN RETURN.

         lcBufferDB[lii] = lhDBConfig::DBConnName.

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
              ELSE RETURN lhBuffer[liCurrentDB]::MsSeq + objDumpBase:lcKeyDelimiter +
                          STRING(lhBuffer[liCurrentDB]::DtlSeq) + objDumpBase:lcKeyDelimiter +
                          STRING(lhBuffer[liCurrentDB]::DateSt).
         WHEN "DataInDataOut"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhBuffer[liCurrentDB]::DataIn + lhBuffer[liCurrentDB]::DataOut).
         WHEN "DCEvent"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE DO:

                IF lhBuffer[liCurrentDB]::DCEvent BEGINS "CONTDSL" OR
                   lhBuffer[liCurrentDB]::DCEvent BEGINS "CONTFH" THEN DO:

                   IF lhBuffer[liCurrentDB]::DialType EQ {&DIAL_TYPE_FIXED_VOICE} THEN
                     RETURN lhBuffer[liCurrentDB]::DCevent + "_MIN".
                   
                   IF lhBuffer[liCurrentDB]::DialType EQ {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN
                     RETURN lhBuffer[liCurrentDB]::DCevent + "_QTY".
                END.

                RETURN lhBuffer[liCurrentDB]::DCevent.
          END.
         OTHERWISE RETURN "". 
         
      END CASE.
   
   END METHOD.


   METHOD PRIVATE VOID mProcessEDRHistory():

      DEFINE VARIABLE ldaFromDate AS DATE    NO-UNDO.
      DEFINE VARIABLE liFromTime  AS INTEGER NO-UNDO.
      DEFINE VARIABLE ldaToDate   AS DATE    NO-UNDO.
      DEFINE VARIABLE liToTime    AS INTEGER NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDTB:ldeFromTS)
         liFromTime  = HPDTime:mMake2Time(objDTB:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDTB:ldeToTS)
         liToTime    = HPDTime:mMake2Time(objDTB:ldeToTS)
         .

      lhQuery:SET-BUFFERS(lhMainTable).

      IF ldaFromDate NE ldaToDate
      THEN DO:
         /* Process first the first day */
         mProcessQuery(SUBSTITUTE(lcFirstDayQuery,lcTableName,STRING(ldaFromDate),STRING(liFromTime))).

         /* Then process days in the middle if the days are not sequential */
         IF (NOT llInterrupted) AND ldaFromDate + 1 NE ldaToDate
         THEN mProcessQuery(SUBSTITUTE(lcQuery,lcTableName,STRING(ldaFromDate + 1),STRING(ldaToDate - 1))).

         /* At last process the last day */
         IF NOT llInterrupted
         THEN mProcessQuery(SUBSTITUTE(lcLastDayQuery,lcTableName,STRING(ldaToDate),STRING(liToTime))).

      END.

      /* We need to process only one day as from and to date are the same date */
      ELSE mProcessQuery(SUBSTITUTE(lcOneDayQuery,lcTableName,STRING(ldaFromDate),STRING(liFromTime),STRING(liToTime))).

   END METHOD.


   METHOD PRIVATE VOID mProcessQuery
      (INPUT icQuery AS CHARACTER):

      DEFINE VARIABLE llOK    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lcEvent AS CHARACTER NO-UNDO.

      lhQuery:QUERY-PREPARE(icQuery).

      lhQuery:QUERY-OPEN().

      REPEAT
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY
         ON ERROR UNDO, THROW:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.

         llOK = lhQuery:GET-NEXT().

         IF llOK = ?
         THEN DO:
            llInterrupted = TRUE.
            RETURN ERROR NEW Progress.Lang.AppError("Cannot process query", 1).
         END.

         /* No more records, or query is not open */
         IF NOT llOK
         THEN LEAVE.

         DO liCurrentDB = 1 TO liBufferCount:

            llOK = lhBuffer[liCurrentDB]:FIND-FIRST(SUBSTITUTE(lcMobCDRQuery,
                                                               QUOTER(lhMainTable::CLI),
                                                               lhMainTable::DateSt,
                                                               lhMainTable::TimeStart,
                                                               lhMainTable::DtlSeq), NO-LOCK) NO-ERROR.
            IF llOK
            THEN DO:
               ASSIGN
                  liEvents = liEvents + 1
                  lcEvent  = IF lhBuffer[liCurrentDB]::ErrorCode = 0
                             THEN IF lhMainTable::ErrorCode > 0
                                  THEN "CREATE"
                                  ELSE "MODIFY"
                             ELSE "DELETE".

               IF liCurrentDB = 1
               THEN objDTB:mWriteALineWithPrefix(SUBSTITUTE(lcHeader,lcEvent)).
               ELSE objDTB:mWriteALine(SUBSTITUTE(lcHeader,lcEvent) + objDumpBase2:mGetData()).

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

      FINALLY:
         lhQuery:QUERY-CLOSE().
      END FINALLY.

   END METHOD.

END CLASS.
