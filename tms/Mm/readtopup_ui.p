/* ----------------------------------------------------------------------
  MODULE .......: readtopup_ui.p
  TASK .........: Read topups from files (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 15.10.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PrepaidRequest'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liRead        AS INT  NO-UNDO. 
DEF VAR liErrors      AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcDir         AS CHAR NO-UNDO.
DEF VAR lcChosen      AS CHAR NO-UNDO.
DEF VAR llOk          AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Read topups from a file." AT 5 
   SKIP(3)
                   
   "File Name:" AT 5 SKIP
   lcFile AT 5
      NO-LABEL
      HELP "File containing list of subscriptions (F9 -> choose)"
      FORMAT "X(72)"
   SKIP(9)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + "  TOPUP FILE  " + 
           STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey  = FALSE
   lcFile = fCParamC("ReadTopupFile").

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcFile WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7 
         ufk[5] = 795
         ufk[8] = 8 
         Syst.CUICommon:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN Syst.CUICommon:toimi = 1
               ufkey = TRUE.

   IF Syst.CUICommon:toimi = 1 THEN DO:

      Syst.CUICommon:ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcFile WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" THEN DO:

               lcDir = "".
               IF INDEX(INPUT lcFile,"*") = 0 AND
                  INDEX(INPUT lcFile,"?") = 0 AND
                  INDEX(INPUT lcFile,"/") GT 0
               THEN ASSIGN liCount = R-INDEX(INPUT lcFile,"/")
                           lcDir   = SUBSTRING(INPUT lcFile,1,liCount - 1).

               RUN Mc/choosefile.p (IF lcDir NE "" 
                               THEN lcDir
                               ELSE INPUT lcFile,
                               OUTPUT lcChosen).
               IF lcChosen NE "" THEN DO:
                  lcFile = (IF lcDir NE ""
                              THEN lcDir + "/"
                              ELSE "") + lcChosen.
                  DISPLAY lcFile.
               END. 

               Syst.CUICommon:ehto = 9.
               RUN Syst/ufkey.p.
            END. 

            ELSE APPLY LASTKEY. 

         END. 

         LEAVE. 
      END.

   END.

   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF lcFile = "" OR 
         INDEX(lcFile,"*") > 0 OR 
         INDEX(lcFile,"?") > 0 OR
         INDEX(lcFile,"<") > 0 
      THEN DO:
         MESSAGE "File has not been chosen."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      llOk = FALSE.
      MESSAGE "Start importing topups?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN NEXT.
      
      RUN Mm/readtopup.p (lcFile,
                     OUTPUT liRead,
                     OUTPUT liErrors).
      
      MESSAGE liRead "rows were read." SKIP
              liErrors "errors occurred."
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

