/*------------------------------------------------------------------------
    File        : ServiceLCounterDump
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 15:19:10 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpTimeBased.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.ServiceLCounterDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbServiceLCounter FOR ServiceLCounter.


   CONSTRUCTOR PUBLIC ServiceLCounterDump
      (iiDumpID AS INTEGER):
      
      lhMainTable = BUFFER lbServiceLCounter:HANDLE.
         
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC ServiceLCounterDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
         
      objDumpBase = NEW DumpTimeBased(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbServiceLCounter:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessTimeBased(INPUT CAST(objDumpBase, DumpTimeBased)).
   
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
              ELSE RETURN IF lbServiceLCounter.MsSeq = 0
                          THEN {HPD/keyvalue.i lbServiceLCounter . objDumpBase:lcKeyDelimiter CustNum SlSeq Period}
                          ELSE {HPD/keyvalue.i lbServiceLCounter . objDumpBase:lcKeyDelimiter MsSeq SlSeq Period}.
         OTHERWISE RETURN "". 

      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mProcessTimeBased
      (INPUT objDumpTB AS CLASS DumpTimeBased):

      DEFINE VARIABLE liEndPeriod   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liBeginPeriod AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liPeriod      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcBundles     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liCount       AS INTEGER   NO-UNDO.
      
      ASSIGN
         liBeginPeriod = INTEGER(TRUNCATE(objDumpTB:ldeFromTS / 100, 0))
         liEndPeriod   = INTEGER(TRUNCATE(objDumpTB:ldeToTS / 100, 0))
         lcBundles     = objDumpTB:mGetConfigParam()
         .

      DO WITH FRAME fQty:

         /* Dump ServiceLCounter for mentioned bundles */
         BUNDLELOOP:
         DO liCount = 1 TO NUM-ENTRIES(lcBundles):

            FOR
               FIRST ServiceLimit FIELDS (GroupCode DialType SlSeq) NO-LOCK WHERE
                  ServiceLimit.GroupCode = ENTRY(liCount,lcBundles) AND
                  ServiceLimit.DialType  = 7:

               liPeriod = liBeginPeriod.

               DO WHILE liPeriod <= liEndPeriod:

                  FOR
                     EACH lbServiceLCounter NO-LOCK WHERE
                        lbServiceLCounter.CustNum = 0                   AND
                        lbServiceLCounter.Period  = liPeriod            AND
                        lbServiceLCounter.SlSeq   = ServiceLimit.SlSeq
                     ON QUIT UNDO, RETRY
                     ON STOP UNDO, RETRY:

                     IF RETRY THEN DO:
                        llInterrupted = TRUE.
                        LEAVE BUNDLELOOP.
                     END.

                     liEvents = liEvents + 1.

                     objDumpTB:mWriteALine().

                     IF NOT SESSION:BATCH AND liEvents MOD 100 = 0
                     THEN DO:
                        PAUSE 0.
                        DISPLAY liEvents LABEL "Consumption Counter"
                           WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                           TITLE " Collecting " FRAME fQty.
                     END.
                  END.

                  FOR
                     EACH lbServiceLCounter NO-LOCK WHERE
                        lbServiceLCounter.MsSeq   = 0                   AND
                        lbServiceLCounter.Period  = liPeriod            AND
                        lbServiceLCounter.SlSeq   = ServiceLimit.SlSeq
                     ON QUIT UNDO, RETRY
                     ON STOP UNDO, RETRY:

                     IF RETRY THEN DO:
                        llInterrupted = TRUE.
                        LEAVE BUNDLELOOP.
                     END.

                     liEvents = liEvents + 1.

                     objDumpTB:mWriteALine().

                     IF NOT SESSION:BATCH AND liEvents MOD 100 = 0
                     THEN DO:
                        PAUSE 0.
                        DISPLAY liEvents LABEL "Consumption Counter"
                           WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
                           TITLE " Collecting " FRAME fQty.
                     END.
                  END.

                  liPeriod = HPDTime:mNextPeriod(liPeriod).

               END.
            END.
         END.

         IF NOT SESSION:BATCH
         THEN HIDE FRAME fQty NO-PAUSE.

      END.

   END METHOD.  

END CLASS.