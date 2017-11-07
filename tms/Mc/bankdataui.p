/* ----------------------------------------------------------------------
  MODULE .......: bankdataui.p
  TASK .........: Read bank data from files (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 06.03.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Bank'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liRead        AS INT  NO-UNDO. 
DEF VAR liNew         AS INT  NO-UNDO. 
DEF VAR liUpdated     AS INT  NO-UNDO. 
DEF VAR liErrors      AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcLogFile     AS CHAR NO-UNDO.
DEF VAR lcDir         AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO.

FORM 
   SKIP(2)
   "Read bank data from a file." AT 5 
   SKIP(3)
                   
   "File Name:" AT 5 SKIP
   lcFile AT 5
      NO-LABEL
      HELP "File containing bank data (F9 -> choose)"
      FORMAT "X(72)"
   SKIP(9)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.Var:ynimi + "  READ BANK DATA  " + 
           STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN ufkey      = FALSE
       lcFile     = fCParamC("BankDataFiles")
       lcLogFile  = fCParamC("BankDataLog")
       lcTransDir = fCParamC("BankDataArc").

lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(YEAR(TODAY),"9999") +
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")). 
 
CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcFile WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 7 
         Syst.Var:ufk[5] = 795
         Syst.Var:ufk[8] = 8 
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN Syst.Var:toimi = 1
               ufkey = TRUE.

   IF Syst.Var:toimi = 1 THEN DO:

      Syst.Var:ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcFile WITH FRAME fCrit EDITING:

            READKEY.
            Syst.Var:nap = KEYLABEL(LASTKEY).

            IF Syst.Var:nap = "F9" THEN DO:

               lcDir = "".
               IF INDEX(INPUT lcFile,"*") = 0 AND
                  INDEX(INPUT lcFile,"?") = 0 AND
                  INDEX(INPUT lcFile,"/") GT 0
               THEN ASSIGN liCount = R-INDEX(INPUT lcFile,"/")
                           lcDir   = SUBSTRING(INPUT lcFile,1,liCount - 1).

               RUN Mc/choosefile.p (IF lcDir NE "" 
                               THEN lcDir
                               ELSE INPUT lcFile,
                               OUTPUT lcFile).
               IF lcFile NE "" THEN DO:
                  lcFile = (IF lcDir NE ""
                              THEN lcDir + "/"
                              ELSE "") + lcFile.
                  DISPLAY lcFile.
               END. 

               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
            END. 

            ELSE APPLY LASTKEY. 

         END. 

         LEAVE. 
      END.

   END.

   ELSE IF Syst.Var:toimi = 5 THEN DO:
      
      IF lcFile = "" OR 
         INDEX(lcFile,"*") > 0 OR 
         INDEX(lcFile,"?") > 0 OR
         INDEX(lcFile,"<") > 0 
      THEN DO:
         MESSAGE "File has not been chosen."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Mc/bankdata.p (lcFile,
                    lcLogFile,
                    OUTPUT liRead,
                    OUTPUT liNew,
                    OUTPUT liUpdated,
                    OUTPUT liErrors).
      
      /* move to archive */
      IF liRead > 0 AND lcTransDir > "" THEN DO:   
         fTransDir(lcFile,
                   "",
                   lcTransDir).
      END.
      
      MESSAGE liRead    "bank rows were read." SKIP
              liNew     "new banks were added," SKIP
              liUpdated "banks were updated," SKIP   
              liErrors  "errors occurred."
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.Var:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

