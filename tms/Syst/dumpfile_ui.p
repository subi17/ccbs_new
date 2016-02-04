/* ----------------------------------------------------------------------
  MODULE .......: dumpfile_ui.p
  TASK .........: Run dump file creation
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 04.11.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DumpFile'}
{Syst/host.i}

DEF VAR liDumpID      AS INT  NO-UNDO.
DEF VAR liDumped      AS INT  NO-UNDO.
DEF VAR lcDumpMode    AS CHAR NO-UNDO.
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR lcFileNameTag AS CHAR NO-UNDO.

FORM 
   SKIP(2)
   "Create a dump file according to configuration." AT 11 SKIP(1)
   liDumpID COLON 20 
      LABEL "Dump File" 
      FORMAT ">>>9"
      HELP "File to be dumped"
   DumpFile.DumpName 
      NO-LABEL SKIP
   lcDumpMode COLON 20
      LABEL "Dump Mode"
      FORMAT "X(8)"
      HELP "Dump mode; Full, Modified or Test" SKIP
   lcFileNameTag COLON 20
      LABEL "File Name Tag"
      FORMAT "X(30)"
      HELP "Value for tag #RUN" SKIP(1)
   DumpFile.FileName COLON 20 SKIP
   DumpFile.SpoolDir COLON 20 SKIP
   DumpFile.TransDir COLON 20 
   SKIP(6)
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  DUMP FILE  " + STRING(pvm,"99-99-99") + " "
     FRAME fCrit.

FUNCTION fDispDumpFile RETURNS LOGIC
   (iiDumpID AS INT):

   FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
   IF AVAILABLE DumpFile THEN DO:
      DISPLAY
         DumpFile.DumpName
         DumpFile.FileName
         DumpFile.SpoolDir
         DumpFile.TransDir WITH FRAME fCrit.
      RETURN TRUE.
   END.
   
   ELSE RETURN FALSE.

END FUNCTION.


ASSIGN
   toimi      = -1
   lcDumpMode = "Full".

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   CLEAR FRAME fCrit ALL NO-PAUSE.
   DISPLAY liDumpID lcDumpMode lcFileNameTag WITH FRAME fCrit.
   
   IF liDumpID > 0 THEN DO:
      fDispDumpFile(liDumpID).
   END.

   IF toimi < 0 THEN toimi = 1.
   ELSE DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.
   END.
   
   IF toimi = 1 THEN 
   REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

      ehto = 9.
      RUN Syst/ufkey.
    
      UPDATE liDumpId lcDumpMode lcFileNameTag WITH FRAME fCrit 
      EDITING:
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "liDumpID" THEN DO:

            ASSIGN
               si-recid    = ?
               gcHelpParam = "dumpid".
            RUN Syst/dumpfile.
            gcHelpParam = "".
            
            IF si-recid NE ? THEN DO:
               FIND DumpFile WHERE RECID(DumpFile) = si-recid NO-LOCK NO-ERROR.
               IF AVAILABLE DumpFile THEN 
                  DISP DumpFile.DumpID @ liDumpID WITH FRAME fCrit.
            END.
            
            ehto = 9.
            RUN Syst/ufkey.

            NEXT. 
         END.
         
         ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
         DO WITH FRAME fCrit:
         
            PAUSE 0.

            IF FRAME-FIELD = "liDumpID" THEN DO:
               IF NOT fDispDumpFile(INPUT INPUT liDumpID) THEN DO:
                  MESSAGE "Unknown dump file"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "lcDumpMode" THEN DO:
               IF LOOKUP(INPUT lcDumpMode,"Test,Full,Modified") = 0
               THEN DO:
                  MESSAGE "Unknown mode"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
            END.
         END.
      
         ELSE IF FRAME-FIELD = "lcDumpMode" THEN DO WITH FRAME fCrit:
            APPLY LASTKEY.
            CASE SUBSTRING(INPUT lcDumpMode,1,1):
            WHEN "T" THEN DISPLAY "Test" @ lcDumpMode.
            WHEN "F" THEN DISPLAY "Full" @ lcDumpMode.
            WHEN "M" THEN DISPLAY "Modified" @ lcDumpMode.
            END CASE.
            NEXT.
         END.
         
         APPLY LASTKEY.
      END.
   
      LEAVE.
   
   END.
   
   ELSE IF toimi = 5 THEN DO:
      
      llOk = FALSE.
      MESSAGE "Start creating a dump file?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN NEXT. 

      RUN Syst/dumpfile_run(liDumpID,
                       lcDumpMode,
                       lcFileNameTag,
                       fIsThisReplica(),
                       OUTPUT liDumped).

      MESSAGE liDumped "rows were dumped." +
              (IF RETURN-VALUE > "" 
               THEN CHR(10) + RETURN-VALUE
               ELSE "")
      VIEW-AS ALERT-BOX 
      TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

