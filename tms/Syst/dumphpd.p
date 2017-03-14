/* ----------------------------------------------------------------------
  MODULE .......: DumpHPD
  TASK .........: browse table DumpHPD
  APPLICATION ..: TMS
  AUTHOR .......: jannetou
  CREATED ......: 01.02.2013
  CHANGED ......: 
  Version ......: M1
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DumpHPD

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DumpHPD'}
{Func/timestamp.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDumpHPD AS HANDLE NO-UNDO.
   lhDumpHPD = BUFFER DumpHPD:HANDLE.
   RUN StarEventInitialize(lhDumpHPD).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDumpHPD).
   END.

END.

DEFINE INPUT PARAMETER iiDumpID AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE siirto AS CHARACTER.

DEFINE VARIABLE ufkey  AS LOGICAL NO-UNDO INITIAL TRUE.

FORM
    DumpHPD.DumpID            COLON 20 
    DumpHPD.Active            COLON 20
    DumpHPD.Continuous        COLON 20
    DumpHPD.StartTime         COLON 20
    DumpHPD.FinalTime         COLON 20
    DumpHPD.UnitsToDump       COLON 20 
    DumpHPD.UnitType          COLON 20 
    DumpHPD.MaxRecordsPerFile COLON 20 
WITH  OVERLAY ROW 2 centered COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " HPD RELATED SETTINGS " SIDE-LABELS FRAME fHPD.

FIND DumpFile NO-LOCK WHERE DumpFile.DumpID = iiDumpID NO-ERROR.

IF NOT AVAILABLE DumpFile
THEN DO:
   MESSAGE "DumpID doesn't exist"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

IF NOT DumpFile.DumpName BEGINS "HPD_" 
THEN DO:
   MESSAGE "The dump is not HPD dump"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

FIND DumpHPD NO-LOCK WHERE DumpHPD.DumpID = iiDumpID NO-ERROR.

IF NOT AVAILABLE DumpHPD
THEN DO TRANSACTION:
   CREATE DumpHPD.
   FIND CURRENT DumpHPD NO-LOCK.
END.

REPEAT WITH FRAME fHPD:

   PAUSE 0.
   DISPLAY        
       DumpHPD.DumpID 
       DumpHPD.Active
       DumpHPD.Continuous
       DumpHPD.StartTime
       DumpHPD.FinalTime
       DumpHPD.UnitsToDump 
       DumpHPD.UnitType 
       DumpHPD.MaxRecordsPerFile 
   WITH FRAME fHPD.
   
   ASSIGN 
      ufk    = 0
      ufk[1] = 7    WHEN lcRight = "RW"
      ufk[8] = 8
      ehto   = 0.
         
   RUN Syst/ufkey.p.

   IF toimi = 1 THEN 
   REPEAT WITH FRAME fAddit ON ENDKEY UNDO, LEAVE MaintMenu:

      FIND CURRENT DumpHPD EXCLUSIVE-LOCK.

      ehto = 9.
      RUN Syst/ufkey.p.

      UPDATE
         DumpHPD.Active
         DumpHPD.Continuous
         DumpHPD.StartTime
         DumpHPD.FinalTime
         DumpHPD.UnitsToDump 
         DumpHPD.UnitType 
         DumpHPD.MaxRecordsPerFile       
      WITH FRAME fHPD EDITING:
 
      READKEY.

      IF KEYLABEL(LASTKEY) = "F9" AND 
         FRAME-FIELD = "UnitType"
      THEN DO:
         liField = FRAME-INDEX.
          
         RUN Help/h-tmscodes.p("DumpHPD",  /* TableName*/
                               "UnitType", /* FieldName */
                               "DumpHPD",  /* GroupCode */
                               OUTPUT siirto).

         DumpHPD.UnitType = siirto.
         DISPLAY DumpHPD.UnitType WITH FRAME fHPD.

         ehto = 9.
         RUN Syst/ufkey.p.

         NEXT. 
      END.

      ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
      DO WITH FRAME fHPD:
         PAUSE 0.
      END.
   
      APPLY LASTKEY.
   END.
   
   LEAVE.
END.
