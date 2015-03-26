/*-----------------------------------------------------------------------------
  MODULE .......: invpplan
  FUNCTION .....: Payment plan handling for chosen invoice
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 14.03.06
  CHANGED . ....: 
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}

DEF INPUT PARAMETER iiInvNum  AS INT  NO-UNDO.

DEF VAR lcMenuc   AS CHAR  NO-UNDO EXTENT 5 FORMAT "X(40)". 
DEF VAR liType    AS INT   NO-UNDO.
DEF VAR lcLetters AS CHAR  NO-UNDO.
DEF VAR llActive  AS LOG   NO-UNDO.

ASSIGN lcLetters = "A,B,C,D,E,F"
       llActive  = FALSE.


/* get names from tmscodes */
DO liType = 1 TO 3:
   lcMenuc[liType + 1] = " " + ENTRY(liType + 1,lcLetters) + ") " +
                         DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                          "PaymPlan",
                                          "PPType",
                                          STRING(liType)).
END.

ASSIGN 
   lcMenuc[1] = " A) Browse Payment Plans"
   lcMenuc[5] = " X) QUIT   (F8)".
                                             
   PAUSE 0.

FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.

DO WHILE TRUE:

      ASSIGN ufk    = 0 
             ufk[8] = 8 
             ehto   = 3. 
      RUN ufkey. 

      DISPLAY
         lcMenuc[1] SKIP(1)
         "CREATE A NEW PLAN" AT 2 SKIP
         lcMenuc[2] SKIP
         lcMenuc[3] SKIP
         lcMenuc[4] SKIP(1)
         lcMenuc[5] SKIP
      WITH 1 COLUMN OVERLAY FRAME choices NO-LABELS.
       
      CHOOSE FIELD lcMenuc AUTO-RETURN go-on (F8) WITH FRAME choices
         TITLE " PAYMENT PLANS "  CENTERED ROW 10.

      HIDE FRAME choices.

      IF LOOKUP(KEYLABEL(LASTKEY),"x,8,F8") > 0 THEN LEAVE.

      IF FRAME-INDEX = 1 THEN DO:
         RUN paymplan (Invoice.CustNum,0,0).
      END.
      
      ELSE IF FRAME-INDEX = 2  THEN DO:
          RUN duedatechg (iiInvNum).
      END.

      ELSE IF FRAME-INDEX = 3  THEN DO:
         RUN ppaymcrea (iiInvNum).
      END.

      ELSE IF FRAME-INDEX = 4  THEN DO:
         RUN paymplan (Invoice.CustNum,-1,0).
      END.

      ELSE LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
   
HIDE MESSAGE.


