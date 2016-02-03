/* ----------------------------------------------------------------------
  MODULE .......: commission_ui.p
  TASK .........: Run commission calculation
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 31.10.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CoTarg'}

DEF VAR liChecked   AS INT  NO-UNDO.
DEF VAR liActivated AS INT  NO-UNDO.
DEF VAR llOk        AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Handle commission queue. Activate those that meet the criteria" AT 10 SKIP
   "in commission rule definitions." AT 10 
   SKIP(13)
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  COMMISSIONS  " + STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   VIEW FRAME fCrit.

   ASSIGN
      ufk    = 0
      ufk[5] = 795
      ufk[8] = 8 
      ehto   = 0.
   RUN ufkey.

   IF toimi = 5 THEN DO:
      
      llOk = FALSE.
      MESSAGE "Start handling commissions?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN NEXT. 
        
      RUN commission_run(OUTPUT liChecked,
                         OUTPUT liActivated).

      MESSAGE liChecked "rows in commission queue were checked," SKIP
              liActivated "of them were activated."
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

