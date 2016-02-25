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

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.CDRDump FINAL INHERITS DumpHandlerWithQuery:

   DEFINE PROTECTED OVERRIDE PROPERTY lcQuery AS CHARACTER NO-UNDO
      GET:
         RETURN "FOR EACH &1 NO-LOCK USE-INDEX ReadDate WHERE " +
                "&1.ReadDate >= &2 AND " +
                "&1.ReadDate <= &3 AND " +
                "&1.ReadTime >= &4 AND " +
                "&1.ReadTime <  &5 AND " +
                "&1.ErrorCode = 0".   
      END GET.
      
      
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

      DO WHILE CAST(objDumpBase, DumpTimeBasedConnect):mConnectMore():

         CREATE BUFFER lhMainTable FOR TABLE lcTableName.
         
         objDumpBase:mUseTable(lhMainTable).
         objDumpBase:mVerifyDumpFileData().
         
         mProcessCDR(INPUT CAST(objDumpBase, DumpTimeBasedConnect)).
         
         IF VALID-HANDLE(lhMainTable)
         THEN DELETE OBJECT lhMainTable.        
         
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
              ELSE RETURN lhMainTable::MSCID + objDumpBase:lcKeyDelimiter +
                          STRING(lhMainTable::DtlSeq) + objDumpBase:lcKeyDelimiter +
                          ISO-DATE(lhMainTable::DateSt).
         WHEN "DataInDataOut"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN STRING(lhMainTable::DataIn + lhMainTable::DataOut).
         /* MobCDR is using DCEvent field directly but PrepCDR is using this
            (because of DFField specifications) */
         WHEN "DCEvent"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE RETURN IF lhMainTable::DCEvent > ""
                          THEN lhMainTable::DCEvent
                          ELSE IF lhMainTable::BillCode = "PREMDUB"
                          THEN "PMDUB"
                          ELSE IF lhMainTable::CLIType = "TARJ7" AND lhMainTable::Charge = 0
                          THEN "TARJ7"
                          ELSE "".
         /*
         WHEN "DateStTimeStart"
         THEN IF ilCheckOnly
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

   
   METHOD PRIVATE VOID mProcessCDR
      (INPUT objDumpTBC AS CLASS DumpTimeBasedConnect):

      DEFINE VARIABLE ldaFromDate AS DATE    NO-UNDO.
      DEFINE VARIABLE ldaFromTime AS INTEGER NO-UNDO.
      DEFINE VARIABLE ldaToDate   AS DATE    NO-UNDO.
      DEFINE VARIABLE ldaToTime   AS INTEGER NO-UNDO.

      ASSIGN
         ldaFromDate = HPDTime:mMake2Date(objDumpTBC:ldeFromTS)
         ldaFromTime = HPDTime:mMake2Time(objDumpTBC:ldeFromTS)
         ldaToDate   = HPDTime:mMake2Date(objDumpTBC:ldeToTS)
         ldaToTime   = HPDTime:mMake2Time(objDumpTBC:ldeToTS)
         .
                
      lhQuery:SET-BUFFERS(lhMainTable).
      lhQuery:QUERY-PREPARE(SUBSTITUTE(lcQuery,
                                       lcTableName,
                                       STRING(ldaFromDate),
                                       STRING(ldaToDate),
                                       ldaFromTime,
                                       ldaToTime)).

      lhQuery:QUERY-OPEN().   
   
      REPEAT
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.           
         
         lhQuery:GET-NEXT().
         IF lhQuery:QUERY-OFF-END THEN LEAVE.

         liEvents = liEvents + 1.
         
         objDumpTBC:mWriteALine().

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