/* ----------------------------------------------------------------------
  MODULE .......: ddinfileui.p
  TASK .........: Read responses to a csb19 file for direct debiting (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.01.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR lcDir         AS CHAR NO-UNDO. 


FORM 
   SKIP(2)
   "Read responses to a direct debit file using CSB19 format." AT 10 
   SKIP(3)
                   
   lcFile AT 10
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(55)"
      SKIP
   SKIP(10)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + "  RESPONSES TO CSB19  " + 
           STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN ufkey  = FALSE
       lcFile = fCParamC("DDebitResponse").

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcFile WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         Syst.CUICommon:ufk    = 0
         Syst.CUICommon:ufk[1] = 132 
         Syst.CUICommon:ufk[5] = 795
         Syst.CUICommon:ufk[8] = 8 
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
            Syst.CUICommon:nap = KEYLABEL(LASTKEY).

            IF Syst.CUICommon:nap = "F9" THEN DO:

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

               Syst.CUICommon:ehto = 9.
               RUN Syst/ufkey.p.
            END. 

            ELSE APPLY LASTKEY. 

         END. 

         LEAVE. 
      END.

   END.

   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File has not been chosen."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Inv/ddinfile.p (lcFile,
                    OUTPUT liCount).
      
      MESSAGE liCount "responses were read." SKIP ""
              (IF RETURN-VALUE BEGINS "ERROR" 
               THEN "Error occurred: " + RETURN-VALUE
               ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

