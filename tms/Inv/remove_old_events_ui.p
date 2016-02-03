/* ----------------------------------------------------------------------
  MODULE .......: remove_old_events_ui.p
  TASK .........: Remove old unbilled events from billing (ui)
  APPLICATION ..: tms     
  AUTHOR .......: aam
  CREATED ......: 03.04.12
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}
{Inv/old_unbilled_events.i}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR ldaEventDate  AS DATE NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR llCDRs        AS LOG  NO-UNDO.
DEF VAR llFees        AS LOG  NO-UNDO.
DEF VAR llFATimes     AS LOG  NO-UNDO.
DEF VAR llOk          AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Remove old unbilled events from billing." AT 5 
   SKIP(3)
                   
   liCustNum COLON 15
      LABEL "Customer" 
      HELP "Invoice customer number, 0=ALL"
      FORMAT ">>>>>>>9"
   lcCustName 
      NO-LABEL
      FORMAT "X(30)" SKIP
      
   ldaEventDate COLON 15 
      LABEL "Event Date"
      HELP "Events older than and equal to this date will be handled"
      FORMAT "99-99-99"
   llCDRs COLON 15   
      LABEL "EDRs" 
      HELP "Handle EDRs"
      FORMAT "Yes/No"
   llFees COLON 15
      LABEL "Fees"
      HELP "Handle fixed and single fees"
      FORMAT "Yes/No"
   llFATimes COLON 15
      LABEL "FATimes"
      HELP "Handle FATimes"
      FORMAT "Yes/No"
   SKIP(6)
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  OLD UNBILLED EVENTS  " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey     = FALSE
   ldaEventDate = fOldUnbilledEventLimit(0)
   llCDRS    = TRUE
   llFees    = TRUE
   llFATimes = TRUE.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   IF liCustNum = 0 THEN lcCustName = "ALL".
   ELSE DO:
      FIND FIRST Customer WHERE 
         Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).
   END.

   PAUSE 0.
   DISPLAY 
      liCustNum 
      lcCustName
      ldaEventDate
      llCDRs
      llFees
      llFATimes
   WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN ufkey.
   END.
   ELSE ASSIGN 
      toimi = 1
      ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN ufkey.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE 
            liCustNum  
            ldaEventDate
            llCDRs
            llFees
            llFATimes
         WITH FRAME fCrit EDITING:

            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME fCrit:
               PAUSE 0.

               IF FRAME-FIELD = "liCustNum" THEN DO:
                  IF INPUT liCustNum = 0 THEN 
                     DISPLAY "ALL" @ lcCustName. 

                  ELSE DO:
                     FIND FIRST Customer WHERE 
                        Customer.CustNum = INPUT liCustNum NO-LOCK NO-ERROR.
                     IF NOT AVAIL Customer THEN DO:
                        BELL.
                        MESSAGE "Unknown customer".
                        NEXT.
                     END.
                     lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                   BUFFER Customer).
                     DISP lcCustName WITH FRAME fCrit.
                  END.
               END.
            END.
                
            APPLY LASTKEY.

         END. 

         LEAVE. 
      END.

   END.

   ELSE IF toimi = 5 THEN DO:
      
      IF ldaEventDate = ? OR 
         (llCDRs = FALSE AND llFees = FALSE AND llFATimes = FALSE)
      THEN DO:
         MESSAGE "Invalid criteria"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      llOk = FALSE.
      MESSAGE "Start removing old unbilled events from billing?"
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO
      SET llOk.
      IF NOT llOk THEN NEXT.

      RUN remove_old_events.p(liCustNum,
                              ldaEventDate,
                              llCDRs,
                              llFees,
                              llFATimes,
                              OUTPUT liCount).
      
      IF RETURN-VALUE BEGINS "ERROR" THEN 
         MESSAGE "Removal failed;" SKIP
                 RETURN-VALUE
         VIEW-AS ALERT-BOX ERROR.
         
      ELSE MESSAGE liCount "events were removed from billing"
           VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    


