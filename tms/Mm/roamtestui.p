/* ----------------------------------------------------------------------
  MODULE .......: ROAMTESTUI.P
  TASK .........: Create test file for RoamOper
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 13.03.07
  CHANGED ......: 14.03.07 kl added Production choose
                  15.05.07 kl date format check
                  
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}

DEFINE VARIABLE lcPLMN     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldaDate1   AS DATE      NO-UNDO FORMAT "99-99-99".
DEFINE VARIABLE ldaDate2   AS DATE      NO-UNDO FORMAT "99-99-99".
DEFINE VARIABLE lcTestCode AS CHARACTER NO-UNDO INIT "V,G,A".
DEFINE VARIABLE lcTestDisp AS CHARACTER NO-UNDO FORMAT "X(1)".
DEFINE VARIABLE liTestCode AS INTEGER   NO-UNDO.
DEFINE VARIABLE llTest     AS LOGICAL   NO-UNDO FORMAT "Test/Production" INIT TRUE.

FORM
   SKIP(1)
   "  Production/Test:" llTest
      HELP "(P)roduction,(T)est"        SKIP
   "  PLMN Code .....:" lcPLMN          SKIP
   "  Time period ...:" ldaDate1 "-"
   ldaDate2                             SKIP
   "  Call type .....:" lcTestDisp 
      HELP "'V'=VOICE,'G'=GPRS,'A'=ALL" SKIP
WITH NO-LABEL CENTERED ROW 8 TITLE " Create RoamCDR TAP File "
   WIDTH 40 FRAME fUI.
   
ASSIGN
   ldaDate1   = TODAY
   ldaDate2   = TODAY
   lcTestDisp = "V".

MAIN:
REPEAT:
   
   ehto = 9.
   
   RUN ufkey.

   CRITERIA:
   REPEAT WITH FRAME fUI ON ENDKEY UNDO, RETURN:
   
      UPDATE
         llTest
         lcPLMN
         ldaDate1
         ldaDate2
         lcTestDisp
      WITH FRAME fUI EDITING:
   
         READKEY.
   
         nap = KEYLABEL(LASTKEY).

         IF LOOKUP(nap,poisnap) > 0 THEN DO:
   
            IF FRAME-FIELD = "llTest" THEN DO:

               ASSIGN llTest.

               IF NOT llTest THEN DO:

                  ASSIGN
                     ldaDate1   = TODAY - 1
                     ldaDate2   = TODAY - 1
                     lcTestDisp = "A".

                  DISP
                     ldaDate1
                     ldaDate2
                     lcTestDisp
                  WITH FRAME fUI.

               END.

            END.
            ELSE IF FRAME-FIELD = "lcPLMN" THEN DO:
         
               ASSIGN lcPLMN.

               IF lcPLMN = "" THEN RETURN.
               ELSE IF NOT CAN-FIND(FIRST RoamOper WHERE
                                          RoamOper.PLMN = lcPLMN) THEN DO:
                  MESSAGE
                     "Roaming operator" lcPLMN "does not exist!"
                  VIEW-AS ALERT-BOX MESSAGE.
                  NEXT.
       
               END.

            END.
            ELSE IF FRAME-FIELD = "ldaDate1" THEN DO:
               DATE(INPUT FRAME fUI ldaDate1) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE
                     " Error in date format: " FRAME-VALUE
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ASSIGN ldaDate1.
            END.
            ELSE IF FRAME-FIELD = "ldaDate2" THEN DO:
               DATE(INPUT FRAME fUI ldaDate2) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE
                     " Error in date format: " FRAME-VALUE
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ASSIGN ldaDate2.
               IF NOT llTest AND ldaDate2 >= TODAY THEN DO:
                  MESSAGE
                     "For Production file you can use only dates prior today!"
                  VIEW-AS ALERT-BOX MESSAGE.
                  NEXT.
               END.
               IF ldaDate2 < ldaDate1 THEN DO:
                  MESSAGE
                     "Time period is wrong!"
                  VIEW-AS ALERT-BOX MESSAGE.
                  NEXT.
               END.
               
            END.
            ELSE IF FRAME-FIELD = "lcTestDisp" THEN DO:
         
               ASSIGN lcTestDisp.
      
               IF NOT llTest AND lcTestDisp NE "A" THEN DO:
                  
                  lcTestDisp = "A".
                  
                  DISP "A" @ lcTestDisp WITH FRAME fUI.
               
                  MESSAGE
                     "With Production CDR types are taken from Operator definitions!"
                  VIEW-AS ALERT-BOX MESSAGE.

               END.
               
               IF LOOKUP(lcTestDisp,lcTestCode) = 0 THEN DO:
                  MESSAGE "Use one of these codes:" lcTestCode VIEW-AS ALERT-BOX.
                  NEXT.
               END.

            END.
      
         END.

         APPLY LASTKEY.
   
      END.

      LEAVE CRITERIA.

   END.

   ASSIGN
      ufk    = 0
      ufk[1] = 132
      ufk[2] = 0
      ufk[4] = 0
      ufk[5] = 795
      ufk[8] = 8
      ehto   = 0.
   
   RUN ufkey.

   IF toimi = 5 THEN
      RUN tapfilecr(lcPLMN,ldaDate1,ldaDate2,lcTestDisp,llTest).

   IF toimi = 8 THEN DO:
      HIDE FRAME fUI.
      RETURN.
   END.
   
END.
