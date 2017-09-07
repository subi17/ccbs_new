/* ----------------------------------------------------------------------
  MODULE .......: invrowcounter_report_ui.p
  TASK .........: Print a report from invoice row counters
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 16.11.10
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'InvRowCounter'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR ldaFromDate   AS DATE NO-UNDO.
DEF VAR ldaToDate     AS DATE NO-UNDO.
DEF VAR lcBillCode    AS CHAR NO-UNDO.
DEF VAR liCCN         AS INT  NO-UNDO.
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCLI         AS CHAR NO-UNDO.
DEF VAR liInvNum      AS INT  NO-UNDO.
DEF VAR lcExtInvID    AS CHAR NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liExtent      AS INT  NO-UNDO.
DEF VAR lcCustName    AS CHAR NO-UNDO.
DEF VAR lcBilled      AS CHAR NO-UNDO.

FORM 
   SKIP(2)
   "Print a control report of invoice row counters." AT 10 
   SKIP(2)
                   
   ldaFromDate COLON 20 
      LABEL "Period" 
      FORMAT "99-99-9999"
      HELP "Period begin date"
   "-"
   ldaToDate COLON 20 
      NO-LABEL  
      FORMAT "99-99-9999"
      HELP "Period end date"
      SKIP

   lcExtInvID COLON 20
      LABEL "Invoice"
      FORMAT "X(14)"
      HELP "Invoice ID, empty=all"
      SKIP
      
   liCustNum COLON 20
      LABEL "Invoice Customer"
      FORMAT ">>>>>>>9"
      HELP "Invoice customer, 0=all"
   lcCustName 
      NO-LABEL
      FORMAT "X(35)"
      SKIP

   lcCLI COLON 20
      LABEL "MSISDN"
      FORMAT "X(12)"
      HELP "MSISDN, empty=all"
      SKIP

   lcBillCode COLON 20
      LABEL "Billing Item"
      FORMAT "X(16)"
      HELP "Billing item, empty=all"
   BillItem.BIName
      NO-LABEL
      SKIP

   liCCN COLON 20
      LABEL "CCN"
      FORMAT ">>>9"
      HELP "Reporting CCN, 0=all"
   CCN.CCNName
      NO-LABEL
      SKIP
      
   lcBilled COLON 20
      LABEL "Billed"
      FORMAT "X(8)"
      HELP "Collect (B)illed, (U)nbilled or (A)ll"
      SKIP(1)
      
   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(55)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(55)" 
   SKIP(3)

WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + ynimi + " INVOICE ROW COUNTER REPORT " + 
           STRING(pvm,"99-99-99") + " "
     FRAME fCrit.


FUNCTION fDispCustomer RETURNS LOGIC
   (iiCustNum AS INT):
   
   IF iiCustNum = 0 THEN lcCustName = "ALL".
   
   ELSE DO:
      FIND FIRST Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).
      ELSE lcCustName = "".
   END.
   
   DISP lcCustName WITH FRAME fCrit.

   RETURN (lcCustName > "").
   
END FUNCTION.

FUNCTION fDispBillItem RETURNS LOGIC
   (icBillCode AS CHAR):
   
   DEF VAR lcBIName AS CHAR NO-UNDO.
   
   IF icBillCode = "" THEN lcBIName = "ALL".
   
   ELSE DO:
      FIND FIRST BillItem WHERE
                 BillItem.Brand = gcBrand AND
                 BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcBIName = BillItem.BIName.
      ELSE lcBIName = "".
   END.

   DISP lcBIName @ BillItem.BIName WITH FRAME fCrit.

   RETURN (lcBIName > "").
   
END FUNCTION.

FUNCTION fDispCCN RETURNS LOGIC
   (iiCCN AS INT):

   DEF VAR lcCCNName AS CHAR NO-UNDO. 
   
   IF iiCCN = 0 THEN lcCCNName = "ALL".
      
   ELSE DO:  
      FIND FIRST CCN WHERE
                 CCN.Brand = gcBrand AND
                 CCN.CCN   = iiCCN NO-LOCK NO-ERROR.
      IF AVAILABLE CCN THEN lcCCNName = CCN.CCNName.
      ELSE CCN.CCNName = "".
   END.   

   DISP lcCCNName @ CCN.CCNName WITH FRAME fCrit.
   
   RETURN (lcCCNName > "").

END FUNCTION.


ASSIGN 
   ufkey       = FALSE
   lcFile      = fCParamC("IRCounterRepFileName")
   lcTransDir  = fCParamC("IRCounterRepTransDir")
   ldaToDate   = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
   ldaFromDate = DATE(MONTH(ldaToDate),1,YEAR(ldaToDate)).
       
CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY 
      ldaFromDate
      ldaToDate
      lcExtInvId
      liCustNum
      lcCli
      lcBillCode
      liCCN
      lcFile
      lcTransDir
   WITH FRAME fCrit.

   fDispCustomer(liCustNum).
   fDispBillItem(lcBillCode).
   fDispCCN(liCCN).
   
   IF ufkey THEN DO:
      ASSIGN
         ufk    = 0
         ufk[1] = 7 
         ufk[5] = 795
         ufk[8] = 8 
         ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   
   ELSE ASSIGN 
      toimi = 1
      ufkey = TRUE.

   IF toimi = 1 THEN DO:

      ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE 
            ldaFromDate
            ldaToDate
            lcExtInvID
            liCustNum
            lcCLI
            lcBillCode
            liCCN
            lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF nap = "F9" AND FRAME-FIELD = "x" THEN DO:
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcExtInvID" THEN DO:
                  
                  IF INPUT lcExtInvID > "" THEN DO:  
                     IF NOT CAN-FIND(FIRST Invoice WHERE 
                                           Invoice.Brand = gcBrand AND
                                           Invoice.ExtInvID = INPUT lcExtInvId)
                     THEN DO:
                        MESSAGE "Unknown invoice"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     lcBilled = "Billed".
                     DISPLAY lcBilled WITH FRAME fCrit.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "liCustNum" THEN DO:
                  IF NOT fDispCustomer(INPUT INPUT liCustNum) THEN DO:
                     MESSAGE "Unknown customer"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "lcBillCode" THEN DO:
                  IF NOT fDispBillItem(INPUT INPUT lcBillCode) THEN DO:
                     MESSAGE "Unknown billing item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "liCCN" THEN DO:
                  IF NOT fDispCCN(INPUT INPUT liCCN) THEN DO:
                     MESSAGE "Unknown CCN"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "lcBilled" THEN DO:
                  IF LOOKUP(INPUT lcBilled,"Billed,Unbilled,All") = 0 THEN DO:
                     MESSAGE "Invalid value"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.   
            END.
 
            ELSE IF FRAME-FIELD = "lcBilled" THEN DO WITH FRAME fCrit:
               APPLY LASTKEY.
               CASE SUBSTRING(INPUT lcBilled,1,1):
               WHEN "B" THEN DISPLAY "Billed" @ lcBilled.
               WHEN "U" THEN DISPLAY "Unbilled" @ lcBilled.
               WHEN "A" THEN DISPLAY "ALL" @ lcBilled.
               END CASE.
               NEXT.
            END.
 
            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.
   
   ELSE IF toimi = 5 THEN DO:
      
      IF ldaFromDate = ? OR ldaTodate = ? OR ldaToDate < ldaFromDate THEN DO:
         MESSAGE "Invalid period"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      IF lcFile = "" OR lcFile = ? THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      IF lcExtInvID > "" THEN DO:

         IF lcBilled = "Unbilled" THEN DO:
            MESSAGE "Invoice cannot be chosen with unbilled selection"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         FIND FIRST Invoice WHERE 
                    Invoice.Brand = gcBrand AND
                    Invoice.ExtInvID = lcExtInvId NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Invoice THEN DO:
            MESSAGE "Unknown invoice"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         IF liCustNum > 0 AND liCustNum NE Invoice.CustNum THEN DO:
            MESSAGE "Chosen invoice does not belong to the customer"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.
      
      IF lcCLI > "" AND liCustNum > 0 THEN DO:
         IF NOT CAN-FIND(FIRST MsOwner WHERE
                               MsOwner.InvCust = liCustNum AND
                               MsOwner.CLI = lcCLI) THEN DO:
            MESSAGE "Customer has never been an invoice customer to the"
                    "chosen subscription"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
      END.
           
      RUN Inv/invrowcounter_report.p (ldaFromDate,
                                  ldaToDate,
                                  lcExtInvID,
                                  liCustNum,
                                  lcCLI,
                                  lcBillCode,
                                  liCCN,
                                  lcBilled,
                                  lcFile,
                                  OUTPUT liCount).
                                                   
      MESSAGE liCount "counter rows were picked to the report" SKIP
              RETURN-VALUE
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

