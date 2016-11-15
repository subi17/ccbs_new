/*------------------------------------------------------------------------
    File        : CDRDump.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : Can handle PrepCDR and MobCDR tables
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.DumpHandlerWithQuery.
USING HPD.DumpTimeBasedConnect.
USING HPD.HPDTime.
USING HPD.DumpBase.

{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.CDRDump FINAL INHERITS DumpHandlerWithQuery:

   DEFINE PROTECTED OVERRIDE PROPERTY lcQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX ReadDate WHERE " +
                "&1.ReadDate >= &2 AND " +
                "&1.ReadDate <= &3 AND " +
                "&1.ErrorCode = 0".
      END GET.


   DEFINE PRIVATE PROPERTY lcFirstDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX ReadDate WHERE " +
                "&1.ReadDate = &2 AND " +
                "&1.ReadTime >= &3 AND " +
                "&1.ErrorCode = 0".
      END GET.


   DEFINE PRIVATE PROPERTY lcLastDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX ReadDate WHERE " +
                "&1.ReadDate = &2 AND " +
                "&1.ReadTime < &3 AND " +
                "&1.ErrorCode = 0".
      END GET.


   DEFINE PRIVATE PROPERTY lcOneDayQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX ReadDate WHERE " +
                "&1.ReadDate = &2 AND " +
                "&1.ReadTime >= &3 AND " +
                "&1.ReadTime <  &4 AND " +
                "&1.ErrorCode = 0".
      END GET.


   DEFINE PRIVATE VARIABLE objDTBC AS CLASS DumpTimeBasedConnect NO-UNDO.

      
   CONSTRUCTOR PUBLIC CDRDump
      (iiDumpID AS INTEGER):
      
      /* Here is implicit SUPER() call... */
      
      mSetTableName(iiDumpID).
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).

      CREATE BUFFER lhMainTable FOR TABLE lcTableName.
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().   
            
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC CDRDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
          
      SUPER(iiDumpID).

      objDumpBase = NEW DumpTimeBasedConnect(iiDumpID, icFile, THIS-OBJECT).
      objDTBC     = CAST(objDumpBase, DumpTimeBasedConnect).

      DO WHILE objDTBC:mConnectMore() ON ERROR UNDO, THROW:

         CREATE BUFFER lhMainTable FOR TABLE lcTableName.

         objDumpBase:mUseTable(lhMainTable).
         objDumpBase:mVerifyDumpFileData().

         mProcessCDR().

         FINALLY:
            IF VALID-HANDLE(lhMainTable)
            THEN DELETE OBJECT lhMainTable.
         END FINALLY.

      END.
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.

   
   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):
   
      CASE icField:
         WHEN "KeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter MsSeq DtlSeq DateSt}.
         WHEN "EDRKeyValue"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN {HPD/keyvalue.i lhMainTable :: objDumpBase:lcKeyDelimiter MsSeq DtlSeq DateSt}.
         WHEN "DataInDataOut"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhMainTable::DataIn + lhMainTable::DataOut).
         /* MobCDR is using DCEvent field directly but PrepCDR is using this
            (because of DFField specifications) */
         WHEN "DCEvent"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE DO:

                IF lhMainTable::DCEvent BEGINS "CONTDSL" OR
                   lhMainTable::DCEvent BEGINS "CONTFH" THEN DO:

                   IF lhMainTable::DialType EQ {&DIAL_TYPE_FIXED_VOICE} THEN
                     RETURN lhMainTable::DCevent + "_MIN".
                   
                   IF lhMainTable::DialType EQ {&DIAL_TYPE_FIXED_VOICE_BDEST} THEN
                     RETURN lhMainTable::DCevent + "_QTY".
                END.

                RETURN IF lhMainTable::DCEvent > ""
                       THEN lhMainTable::DCEvent
                       ELSE IF lhMainTable::BillCode = "PREMDUB"
                       THEN "PMDUB"
                       ELSE IF lhMainTable::CLIType = "TARJ7"  AND
                               lhMainTable::Charge = 0
                       THEN "TARJ7"
                       ELSE IF lhMainTable::CLIType = "TARJ9"  AND
                               lhMainTable::EventType = "CALL" AND
                               lhMainTable::Accumulator > 0
                       THEN "TARJ9"
                       ELSE IF lhMainTable::CLIType = "TARJ9"  AND
                               lhMainTable::Charge = 0         AND
                               LOOKUP(lhMainTable::GsmBnr,{&YOIGO_FREE_NUMBERS}) = 0
                       THEN "TARJ9"
                       ELSE "".
         END.
         /*
         WHEN "DateStTimeStart"
         THEN IF il<F6><F6>CheckOnly
              THEN RETURN "OK".
              ELSE RETURN ISO-DATE(DATETIME-TZ(lhMainTable::DateSt, lhMainTable::TimeStart * 1000)).
         WHEN "ReadInTS"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN HPDTime:mTS2DateTimeTZ(lhMainTable::ReadInTS).
         WHEN "GsmBnr"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN QUOTER(lhMainTable::GsmBnr).
         */    
         OTHERWISE RETURN "". 
         
      END CASE.
   
   END METHOD.


   METHOD PRIVATE VOID mProcessCDR():

      DEFINE VARIABLE ldaFromDate AS DATE    NO-UNDO.
      DEFINE VARIABLE liFromTime  AS INTEGER NO-UNDO.
      DEFINE VARIABLE ldaToDate   AS DATE    NO-UNDO.
      DEFINE VARIABLE liToTime    AS INTEGER NO-UNDO.

      IF objDTBC:ldeToTS > HPDTime:mSecOffSet(HPDTime:mMake2DT(TODAY,TIME), -10)
      THEN objDTBC:ldeToTS = HPDTime:mSecOffSet(objDTBC:ldeToTS, -10).

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDTBC:ldeFromTS)
         liFromTime  = HPDTime:mMake2Time(objDTBC:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDTBC:ldeToTS)
         liToTime    = HPDTime:mMake2Time(objDTBC:ldeToTS)
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
      (icQuery AS CHARACTER):

      DEFINE VARIABLE llOK AS LOGICAL NO-UNDO.

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

         liEvents = liEvents + 1.
         
         objDTBC:mWriteALine().

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
