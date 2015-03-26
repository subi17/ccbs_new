/* ----------------------------------------------------------------------
  MODULE .......: invoice_webdisp_ui.p
  TASK .........: Set web display flag to invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 10.12.09
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
DEF VAR ldaInvDate    AS DATE NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR llDisplay     AS LOG  NO-UNDO.
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR lcIgName      AS CHAR NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO EXTENT 2.
DEF VAR lcInvID       AS CHAR NO-UNDO EXTENT 2.

FORM 
   SKIP(2)
   "Mark web display permit to invoices." AT 8 SKIP
   SKIP(1)
                   
   ldaInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldaInvDate NE ?,
               "Invoice date is mandatory")
      HELP "Invoice date"
      SKIP

   lcInvGroup COLON 20
      LABEL "Invoice Group"
      HELP  "Invoice group, EMPTY = all"
   lcIgName
      NO-LABEL
      FORMAT "X(30)" 
   SKIP

   liCustNum[1] COLON 20
      LABEL  "Customers"
      HELP   "Customers"
      FORMAT ">>>>>>>9"
   "-"
   liCustNum[2]
      NO-LABEL
      HELP "Customers"
      VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
               "Invalid definition")
      FORMAT ">>>>>>>9"
      SKIP  

   lcInvID[1] COLON 20
      LABEL "Invoices"
      HELP  "Invoices"
      FORMAT "X(12)"
   "-"
   lcInvID[2]
      NO-LABEL
      HELP "Invoices"
      VALIDATE(INPUT lcInvID[2] >= INPUT lcInvID[1],
               "Invalid definition")
      FORMAT "X(12)"
      SKIP(1)           

   llDisplay COLON 20
      LABEL "Display Allowed" 
      FORMAT "Yes/No"
      HELP "Is web display permitted"
   SKIP(7)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + "  WEB DISPLAY  " + STRING(pvm,"99-99-99") + " "
     FRAME fCrit.

FUNCTION fIGName RETURNS LOGIC
   (INPUT icInvGroup AS CHAR):
   
   lcIGName = "".
   
   IF icInvGroup = "" THEN lcIgName = "ALL".
               
   ELSE DO:
      FIND InvGroup WHERE 
           InvGroup.Brand    = gcBrand AND
           InvGroup.InvGroup = icInvGroup
      NO-LOCK NO-ERROR.
      IF AVAILABLE InvGroup THEN lcIgName = InvGroup.IGName.
   END.
   
   DISPLAY lcIgName WITH FRAME fCrit.
   
END FUNCTION.



ASSIGN 
   ufkey        = FALSE
   ldaInvDate   = TODAY
   liCustNum[2] = 99999999
   lcInvID[2]   = FILL("Z",12)
   llDisplay    = TRUE.

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY ldaInvDate 
           lcInvGroup 
           lcInvID
           liCustNum
           llDisplay
   WITH FRAME fCrit.

   fIGName(lcInvGroup).

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

         UPDATE 
            ldaInvDate
            lcInvGroup
            liCustNum
            lcInvID
            llDisplay
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF LOOKUP(nap,poisnap) > 0 THEN DO:
               IF FRAME-FIELD = "lcInvGroup" THEN DO:

                  IF INPUT lcInvGroup > "" AND
                     NOT CAN-FIND(InvGroup WHERE 
                                  InvGroup.Brand    = gcBrand AND
                                  InvGroup.InvGroup = INPUT lcInvGroup)
                  THEN DO:
                     MESSAGE "Unknown invoicing group."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  fIGName(INPUT INPUT lcInvGroup).
               END.
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   /* setting */
   ELSE IF toimi = 5 THEN DO:
      
      IF ldaInvDate = ? THEN DO:
         MESSAGE "Invoice date has not been chosen"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN invoice_webdisp(ldaInvDate,
                          1,     /* inv.type */
                          lcInvGroup,
                          liCustNum[1],
                          liCustNum[2],
                          lcInvID[1],
                          lcInvID[2],
                          llDisplay,
                          OUTPUT liCount).
      
      MESSAGE "Display permit was set to" liCount "invoices"
      VIEW-AS ALERT-BOX TITLE "DONE".
       
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

