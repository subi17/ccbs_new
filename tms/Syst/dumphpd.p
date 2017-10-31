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
WITH  OVERLAY ROW 2 centered COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " HPD RELATED SETTINGS " SIDE-LABELS FRAME fHPD.

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
   DumpHPD.DumpID = iiDumpID.
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
   WITH FRAME fHPD.
   
   ASSIGN 
      Syst.CUICommon:ufk    = 0
      Syst.CUICommon:ufk[1] = 7    WHEN lcRight = "RW"
      Syst.CUICommon:ufk[8] = 8
      Syst.CUICommon:ehto   = 0.
         
   RUN Syst/ufkey.p.

   IF Syst.CUICommon:toimi = 1
   THEN REPEAT WITH FRAME fHPD ON ENDKEY UNDO, LEAVE:

      FIND CURRENT DumpHPD EXCLUSIVE-LOCK.

      Syst.CUICommon:ehto = 9.
      RUN Syst/ufkey.p.

      UPDATE
         DumpHPD.Active
         DumpHPD.Continuous
         DumpHPD.StartTime
         DumpHPD.FinalTime
         DumpHPD.UnitsToDump 
         DumpHPD.UnitType 
      WITH FRAME fHPD EDITING:
 
         READKEY.

         IF KEYLABEL(LASTKEY) = "F9" AND
            FRAME-FIELD = "UnitType"
         THEN DO:
            RUN Help/h-tmscodes.p("DumpHPD",  /* TableName*/
                                  "UnitType", /* FieldName */
                                  "DumpHPD",  /* GroupCode */
                                  OUTPUT siirto).

            DumpHPD.UnitType = siirto.
            DISPLAY DumpHPD.UnitType WITH FRAME fHPD.

            Syst.CUICommon:ehto = 9.
            RUN Syst/ufkey.p.

            NEXT.
         END.

         ELSE IF LOOKUP(KEYLABEL(LASTKEY),Syst.CUICommon:poisnap) > 0 THEN
         DO WITH FRAME fHPD:

            IF FRAME-FIELD = "UnitType" THEN DO:
               FIND FIRST TMSCodes NO-LOCK WHERE
                  TMSCodes.TableName = "DumpHPD" AND
                  TMSCodes.FieldName = "UnitType" AND
                  TMSCodes.CodeGroup   = "DumpHPD" AND
                  TMSCodes.CodeValue = INPUT DumpHPD.UnitType
               NO-ERROR.

               IF NOT AVAILABLE TMSCodes
               THEN DO:
                   BELL.
                   MESSAGE "Unknown unit type" VIEW-AS ALERT-BOX.
                   NEXT-PROMPT UnitType. NEXT.
               END.
            END.

            IF FRAME-FIELD = "UnitsToDump" THEN DO:
               IF INPUT DumpHPD.UnitsToDump < 0 OR
                  INPUT DumpHPD.UnitsToDump = ?
               THEN DO:                   BELL.
                   MESSAGE "Units to dump cannot be less than zero" VIEW-AS ALERT-BOX.
                   NEXT-PROMPT UnitsToDump. NEXT.
               END.
            END.

            PAUSE 0.
         END.

         APPLY LASTKEY.
      END.

      LEAVE.
   END.
   
   ELSE IF Syst.CUICommon:toimi = 8 THEN LEAVE.

END.

FINALLY:
   fCleanEventObjects().
END FINALLY.