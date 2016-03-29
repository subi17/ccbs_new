USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DPMemberDump FINAL INHERITS DumpHandler:
  
   DEFINE PRIVATE BUFFER lbDPMember FOR DPMember.
  
   CONSTRUCTOR PUBLIC DPMemberDump
      (iiDumpID AS INTEGER):

      lhMainTable = BUFFER lbDPMember:HANDLE.
      
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).
      objDumpBase:mUseTable(lhMainTable).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC DPMemberDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):

      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbDPMember:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
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
              ELSE RETURN {HPD/keyvalue.i lbDPMember . objDumpBase:lcKeyDelimiter DPId HostTable KeyValue ValidFrom}.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE LOGICAL mSkipMsSeq
      (iiMsSeq AS INTEGER):

      FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
         MobSub.MsSeq = iiMsSeq:
         RETURN FALSE.
      END.

      FOR
         FIRST TermMobSub FIELDS (MsSeq) NO-LOCK WHERE
            TermMobSub.MsSeq = iiMsSeq,
         FIRST MSOwner FIELDS (MsSeq TsBegin TsEnd) USE-INDEX MsSeq NO-LOCK WHERE
            MSOwner.MsSeq = TermMobSub.MsSeq:

         IF HPDTime:mMake2Date(MSOwner.TsEnd) > TODAY - 60
         THEN RETURN FALSE.
            
      END.

      RETURN TRUE.
   
   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):

      DEFINE VARIABLE ldaValidTo AS DATE    NO-UNDO.
      DEFINE VARIABLE llSkip     AS LOGICAL NO-UNDO.
      
      ldaValidTo = ADD-INTERVAL(TODAY, -6, "MONTHS").

      FOR
         EACH lbDPMember NO-LOCK WHERE
            lbDPMember.HostTable = "MobSub" AND
            lbDPMember.ValidTo  >= ldaValidTo
         BREAK
            BY lbDBMember.KeyValue
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.
         
         IF FIRST-OF(lbDPMember.KeyValue)
         THEN llSKIP = mSkipMsSeq(INTEGER(lbDPMember.KeyValue)).

         IF llSkip
         THEN NEXT.
                  
         liEvents = liEvents + 1.

         objDumpFile:mWriteALine().

         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "DPMember Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.      
         
      END.

      IF NOT SESSION:BATCH THEN
         HIDE FRAME fQty NO-PAUSE.

   END METHOD.

END CLASS.