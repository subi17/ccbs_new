/* ----------------------------------------------------------------------
  MODULE .......: recalculate_invrowcounter_ui.p
  TASK .........: Recalculate invoice row counters
  APPLICATION ..: tms     
  AUTHOR .......: aam
  CREATED ......: 05.11.10
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'InvRowCounter'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR ldaFromDate   AS DATE NO-UNDO.
DEF VAR ldaToDate     AS DATE NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.


FORM 
   SKIP(2)
   "Recalculate invoice row counters from unbilled EDRs." AT 5 
   SKIP(3)
                   
   liCustNum COLON 15
      LABEL "Customer" 
      HELP "Invoice customer number"
      FORMAT ">>>>>>>9"
   lcCustName 
      NO-LABEL
      FORMAT "X(30)"
   SKIP
   ldaFromDate COLON 15 
      LABEL "Period"
      HELP "First day of period"
      FORMAT "99-99-99"
   "-"
   ldaToDate 
      NO-LABEL
      HELP "Last day of period"
      FORMAT "99-99-99"
   SKIP(9)
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  INVOICE ROW COUNTERS  " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey       = FALSE
   ldaFromDate = DATE(MONTH(TODAY),1,YEAR(TODAY))
   ldaToDate   = IF MONTH(TODAY) = 12
                 THEN DATE(12,31,YEAR(TODAY))
                 ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY liCustNum ldaFromDate ldaToDate WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.
   END.
   ELSE ASSIGN 
      toimi = 1
      ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE 
            liCustNum  
            ldaFromDate
            ldaToDate
         WITH FRAME fCrit EDITING:

            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME fCrit:
               PAUSE 0.

               IF FRAME-FIELD = "liCustNum" THEN DO:
                  IF INPUT liCustNum = 0 THEN 
                     DISPLAY "" @ lcCustName. 

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
      
      IF liCustNum = 0 OR ldaFromDate = ? OR ldaToDate = ? THEN DO:
         MESSAGE "Invalid criteria"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Inv/recalculate_invrowcounter.p(liCustNum,
                                      0,
                                      0,
                                      ldaFromDate,
                                      ldaToDate,
                                      OUTPUT liCount).
      
      IF RETURN-VALUE BEGINS "ERROR" THEN 
         MESSAGE "Recalculation failed;" SKIP
                 RETURN-VALUE
         VIEW-AS ALERT-BOX ERROR.
         
      ELSE MESSAGE liCount "entries were added to TMQueue for recalculation"
           VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    


