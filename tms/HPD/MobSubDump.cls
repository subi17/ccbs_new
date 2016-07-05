USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.DumpBase.
USING HPD.DumpFile.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.MobSubDump FINAL INHERITS DumpHandler:

   DEFINE PRIVATE BUFFER lbMobSub     FOR MobSub.
   DEFINE PRIVATE BUFFER lbMsOwner    FOR MsOwner.  
  
   CONSTRUCTOR PUBLIC MobSubDump
      (iiDumpID AS INTEGER):
               
      objDumpBase = NEW DumpBase(iiDumpID, THIS-OBJECT).      
      objDumpBase:mUseTable(BUFFER lbMobSub:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMsOwner:HANDLE).
      objDumpBase:mVerifyDumpFileData().
      
   END CONSTRUCTOR.


   CONSTRUCTOR PUBLIC MobSubDump
      (iiDumpID    AS INTEGER,
       icFile      AS CHARACTER):
               
      objDumpBase = NEW DumpFile(iiDumpID, icFile, THIS-OBJECT).
      objDumpBase:mUseTable(BUFFER lbMobSub:HANDLE).
      objDumpBase:mUseTable(BUFFER lbMsOwner:HANDLE).
      objDumpBase:mVerifyDumpFileData().

      mProcessFullDump(INPUT CAST(objDumpBase, DumpFile)).
      
      /* If there has been problems we won't be here... */ 
      IF llInterrupted = FALSE
      THEN llAllOK = TRUE.
      
   END CONSTRUCTOR.


   METHOD OVERRIDE PUBLIC CHARACTER mProcessRepLog
      (INPUT icRowID AS CHARACTER):
         
      FIND lbMobSub NO-LOCK WHERE
         ROWID(lbMobSub) = TO-ROWID(icRowID)
      NO-ERROR.
      
      IF AVAILABLE lbMobSub
      THEN DO:
         FIND CURRENT lbMobSub NO-LOCK.

         mFindMsOwner().
               
         RETURN objDumpBase:mGetData().
         
      END.
            
      RETURN "". 
         
   END METHOD.   


   METHOD PUBLIC OVERRIDE CHARACTER mSpecialHandling
      (icField     AS CHARACTER,
       ilCheckOnly AS LOGICAL):

      CASE icField:
         WHEN "TariffBundle"
         THEN IF ilCheckOnly
              THEN RETURN "OK".
              ELSE CASE lbMobSub.TariffBundle:
                      WHEN "CONTDATA" THEN RETURN "CONTRD1".
                      WHEN "CONTD2"   THEN RETURN "CONTRD2".
                      WHEN "CONTD3"   THEN RETURN "CONTRD3".
                      WHEN "CONTD4"   THEN RETURN "CONTRD4".
                      OTHERWISE            RETURN lbMobSub.TariffBundle.
                   END CASE.
         OTHERWISE RETURN "".         
      END CASE.

   END METHOD.


   METHOD PRIVATE VOID mFindMsOwner():

      FIND FIRST lbMsOwner NO-LOCK USE-INDEX MsSeq WHERE
         lbMsOwner.MsSeq        =  lbMobSub.MsSeq   AND
         ( lbMsOwner.CLIType      NE lbMobSub.CLIType OR
           lbMsOwner.TariffBundle NE lbMobSub.TariffBundle )
      NO-ERROR.      
   
   END METHOD.


   METHOD PRIVATE VOID mProcessFullDump
      (INPUT objDumpFile AS CLASS DumpFile):
   
      FOR EACH lbMobSub NO-LOCK
         ON QUIT UNDO, RETRY
         ON STOP UNDO, RETRY:
   
         IF RETRY THEN DO:
            llInterrupted = TRUE.
            LEAVE.
         END.            
            
         liEvents = liEvents + 1.
      
         mFindMsOwner().

         objDumpFile:mWriteALine().
         
         IF NOT SESSION:BATCH AND liEvents MOD 100 = 0 THEN 
         DO:
            PAUSE 0.
            DISPLAY liEvents LABEL "MobSub Counter" 
               WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
               TITLE " Collecting " FRAME fQty.
         END.         
   
      END.
      
      IF NOT SESSION:BATCH THEN 
         HIDE FRAME fQty NO-PAUSE.    

   END METHOD.

END CLASS.