/* ----------------------------------------------------------------------
  MODULE .......: invrun_multi.p
  TASK .........: Create invoices in multiple runs (screens)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 04.09.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR llInvType     AS LOG  NO-UNDO.
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR ldtDueDate    AS DATE NO-UNDO.
DEF VAR ldtDateFrom   AS DATE NO-UNDO.
DEF VAR ldtDateTo     AS DATE NO-UNDO.
DEF VAR liFeePeriod   AS INT  NO-UNDO.
DEF VAR liScreenQty   AS INT  NO-UNDO.
DEF VAR liFileCnt     AS INT  NO-UNDO.
DEF VAR lcFileList    AS CHAR NO-UNDO.
DEF VAR liCustCnt     AS INT  NO-UNDO.

FORM 
   SKIP(2)
   "Create invoices in multiple runs. Customers are divided into " AT 8 
   "groups according to traffic amounts on billing period." AT 8 SKIP
   "Make sure that You have started this in a screen session." AT 8 
   SKIP(1)
                   
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtInvDate NE ?,
               "Invoice date is mandatory")
      HELP "Invoice date"
      SKIP
   ldtDueDate COLON 20 
      LABEL "Due Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDueDate = ? OR
               INPUT ldtDueDate >= INPUT ldtInvDate,
               "Due date cannot be earlier then invoice date")
      HELP "Due date"
      SKIP(1)

   ldtDateFrom COLON 20 
      LABEL "Billing Period" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDateFrom NE ?,
               "Billing period is mandatory")
      HELP "Billing period"
   "-"
   ldtDateTo 
      NO-LABEL 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtDateTo NE ? AND INPUT ldtDateTo >= INPUT ldtDateFrom,
               "Period end cannot be earlier than period begin")
      HELP "Billing period"
      SKIP

   liFeePeriod COLON 20
      LABEL "Fee Period"
      FORMAT "999999"
      HELP "Billing period for fees"
      SKIP

   llInvType COLON 20
      LABEL "Type Of Invoices"
      FORMAT "Normal/Test"
      HELP "(N)ormal or (T)est invoices"
      SKIP(1)
      
   liScreenQty COLON 20
      LABEL "Screens"
      FORMAT ">9"
      HELP "How many screens (runs) are started"
      SKIP(3)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.Var:ynimi + " BILLING RUN " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.

FUNCTION fDefaultPeriod RETURNS LOGIC
   (INPUT idtInvDate AS DATE):

   IF MONTH(idtInvDate) = 1 THEN ASSIGN
      ldtDateFrom = DATE(12,1,YEAR(idtInvDate) - 1)
      ldtDateTo   = DATE(12,31,YEAR(idtInvDate) - 1).
   ELSE ASSIGN
      ldtDateFrom = DATE(MONTH(idtInvDate) - 1,1,YEAR(idtInvDate))
      ldtDateTo   = DATE(MONTH(idtInvDate),1,YEAR(idtInvDate)) - 1.
      
   liFeePeriod = YEAR(ldtDateTo) * 100 + MONTH(ldtDateTo).

   PAUSE 0.
   DISPLAY ldtDateFrom ldtDateTo liFeePeriod WITH FRAME fCrit.

END FUNCTION.


ASSIGN ufkey         = FALSE
       ldtInvDate    = TODAY
       llInvType     = TRUE
       liScreenQty   = 10.

fDefaultPeriod(ldtInvDate).

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY 
      ldtInvDate
      ldtDueDate
      ldtDateFrom
      ldtDateTo
      liFeePeriod
      llInvType
      liScreenQty
   WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 132 
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

         UPDATE ldtInvDate
                ldtDueDate
                ldtDateFrom
                ldtDateTo
                liFeePeriod
                llInvType
                liScreenQty
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.Var:nap = KEYLABEL(LASTKEY).

            IF LOOKUP(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "ldtInvDate" THEN DO:
                  IF ldtInvDate ENTERED THEN DO:
                     fDefaultPeriod(INPUT INPUT ldtInvDate).
                  END.
               END.
               
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   /* split customers into groups and start billing runs */
   ELSE IF Syst.Var:toimi = 5 THEN DO:

      /* check that no test invoices exist */
      IF CAN-FIND(FIRST Invoice WHERE 
                        Invoice.Brand   = Syst.Var:gcBrand AND
                        Invoice.InvType = 99)
      THEN DO:
         MESSAGE "Test invoices must be deleted " SKIP
                 "before normal invoices can be created." 
         VIEW-AS ALERT-BOX.
         NEXT.
      END.
      
      RUN Inv/invrun_split.p(ldtInvDate,
                       ldtDueDate,
                       ldtDateFrom,
                       ldtDateTo,
                       liFeePeriod,
                       llInvType,
                       liScreenQty,
                       0,
                       0,
                       0,
                       "",
                       0,
                       0,
                       "",
                       OUTPUT lcFileList,
                       OUTPUT liCustCnt).
                     
      /* create screens for actual billing runs */
      IF lcFileList > "" THEN DO:

         RUN Inv/invrun_start_screens.p(ldtInvDate,
                                  lcFileList).

         IF RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:
            CREATE ErrorLog.
            ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
                   ErrorLog.ActionID  = "SplitBillRun" 
                   ErrorLog.TableName = "Invoice"
                   ErrorLog.KeyValue  = STRING(ldtInvDate,"99-99-99")
                   ErrorLog.ErrorMsg  = RETURN-VALUE
                   ErrorLog.UserCode  = Syst.Var:katun.
                   ErrorLog.ActionTS  = Func.Common:mMakeTS().
         END.
      
         IF RETURN-VALUE = "" THEN 
            MESSAGE "Screens have been started"
            VIEW-AS ALERT-BOX.
         ELSE 
            MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX ERROR.
      END.
      
      ELSE MESSAGE "No customers were found to be billed" SKIP
                   RETURN-VALUE
           VIEW-AS ALERT-BOX INFORMATION.
           
      LEAVE CritLoop.
   END.

   ELSE IF Syst.Var:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

