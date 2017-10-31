/* ----------------------------------------------------------------------
  MODULE .......: invoice_deliverystate_ui.p
  TASK .........: Set delivery state to invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 07.04.09
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR ldaDueDate    AS DATE NO-UNDO. 

FORM 
   SKIP(2)
   "Mark invoices as transferrable to IFS, i.e. set delivery" AT 8 SKIP
   "state as 1." AT 8 SKIP
   SKIP(1)
                   
   ldaInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldaInvDate NE ?,
               "Invoice date is mandatory")
      HELP "Invoice date"
      SKIP

   ldaDueDate COLON 20 
      LABEL "Inv Due Date" 
      FORMAT "99-99-9999"
      HELP "Due date"
      SKIP
   SKIP(9)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + "  DELIVERY STATE  " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey      = FALSE
   ldaInvDate = TODAY.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY ldaInvDate SKIP
           ldaDueDate WITH FRAME fCrit.

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

         UPDATE ldaInvDate SKIP
                ldaDueDate
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF LOOKUP(nap,poisnap) > 0 THEN DO:
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   /* setting */
   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF ldaInvDate = ? THEN DO:
         MESSAGE "Invoice date has not been chosen"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Inv/invoice_deliverystate.p(ldaInvDate,
                                1,  /* inv.type */
                                1,  /* state */
                                0,  /* FRProcessID */
                                0,  /* UpdateInterval */
                                ldaDueDate, /* due date if entered */
                                OUTPUT liCount).
      
      MESSAGE "Delivery state was set to" liCount "invoices"
      VIEW-AS ALERT-BOX TITLE "DONE".
       
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

