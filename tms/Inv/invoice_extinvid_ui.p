/* ----------------------------------------------------------------------
  MODULE .......: invoice_extinvid_ui.p
  TASK .........: Set external IDs to invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 14.01.09
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{timestamp.i}

{lib/tokenlib.i}
{lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 

FORM 
   SKIP(2)
   "Number invoices, i.e. set external IDs to them. Only unprinted" AT 8 SKIP
   "invoices with blank ID are handled." AT 8  
   SKIP(1)
                   
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtInvDate NE ?,
               "Invoice date is mandatory")
      HELP "Invoice date"
      SKIP
   SKIP(11)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  INVOICE NUMBERING " + STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


ASSIGN 
   ufkey      = FALSE
   ldtInvDate = TODAY.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY ldtInvDate WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7
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

         UPDATE ldtInvDate
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
   ELSE IF toimi = 5 THEN DO:
      
      IF ldtInvDate = ? THEN DO:
         MESSAGE "Invoice date has not been chosen"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN invoice_extinvid(ldtInvDate,
                           1,  /* inv.type */
                           2,  /* action */
                           0,  /* not a function run */
                           0,  /* not a function run */
                           OUTPUT liCount).
      
      IF liCount > 0 THEN 
         MESSAGE "External Invoice ID was set to" liCount "invoices"
         VIEW-AS ALERT-BOX TITLE "DONE".
       
      ELSE MESSAGE "Renumbering was not done" SKIP
           RETURN-VALUE
           VIEW-AS ALERT-BOX INFORMATION.
       
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

