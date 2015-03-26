/* ----------------------------------------------------------------------
  MODULE .......: ddinfileui.p
  TASK .........: Read responses to a csb19 file for direct debiting (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 25.01.07
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}

{lib/tokenlib.i}
{lib/tokenchk.i 'Invoice'}

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
     TITLE " " + ynimi + "  RESPONSES TO CSB19  " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


ASSIGN ufkey  = FALSE
       lcFile = fCParamC("DDebitResponse").

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcFile WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 132 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN ufkey.
   END.
   ELSE ASSIGN toimi = 1
               ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN ufkey.
      
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

               RUN choosefile (IF lcDir NE "" 
                               THEN lcDir
                               ELSE INPUT lcFile,
                               OUTPUT lcFile).
               IF lcFile NE "" THEN DO:
                  lcFile = (IF lcDir NE ""
                              THEN lcDir + "/"
                              ELSE "") + lcFile.
                  DISPLAY lcFile.
               END. 

               ehto = 9.
               RUN ufkey.
            END. 

            ELSE APPLY LASTKEY. 

         END. 

         LEAVE. 
      END.

   END.

   ELSE IF toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File has not been chosen."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN ddinfile.p (lcFile,
                    OUTPUT liCount).
      
      MESSAGE liCount "responses were read." SKIP ""
              (IF RETURN-VALUE BEGINS "ERROR" 
               THEN "Error occurred: " + RETURN-VALUE
               ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

