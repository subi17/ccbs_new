/* ---------------------------------------------------------------------------
  MODULE .......: PDFINVUI
  FUNCTION .....: Interface for printing invoices to PDF files
  APPLICATION ..: TMS
  CREATED ......: 30.04.03/aam (base from nnlaki)
  MODIFIED .....: 08.10.03/aam mail target (customer/user)
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

def var i-date1      AS DATE FORMAT "99-99-99"       NO-UNDO.
def var i-date2      AS DATE FORMAT "99-99-99"       NO-UNDO.
DEF VAR InvNum1      AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR InvNum2      AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR CustNum1     AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR CustNum2     AS INT  FORMAT "zzzzzzz9"       NO-UNDO.
DEF VAR status1      AS INT  FORMAT "9"              NO-UNDO.
DEF VAR status2      AS INT  FORMAT "9"              NO-UNDO.

DEF VAR rap          AS LOG                          NO-UNDO INIT TRUE.
DEF VAR InvGroup     LIKE invgroup.InvGroup          NO-UNDO.
DEF VAR CustGroup    LIKE CustGroup.CustGroup        NO-UNDO.

DEF VAR lcigroup     AS CHAR                         NO-UNDO.
DEF VAR c-inv        AS CHAR                         NO-UNDO. 
DEF VAR llok         AS LOGIC                        NO-UNDO. 
DEF VAR liCount      AS INT                          NO-UNDO. 
DEF VAR liMailSent   AS INT                          NO-UNDO. 
DEF VAR liErrors     AS INT                          NO-UNDO. 

DEF VAR llSendeMail  AS LOGIC                        NO-UNDO INIT TRUE. 
DEF VAR llFormPDF    AS LOGIC                        NO-UNDO INIT TRUE.
DEF VAR liDelType    AS INT                          NO-UNDO. 
DEF VAR lcCode       AS CHAR                         NO-UNDO. 
DEF VAR llM2Cust     AS LOG                          NO-UNDO INIT TRUE. 

DEF STREAM sRead.

form
   skip(17)
   WITH 
   COLOR value(cfc) width 80 OVERLAY FRAME taka.

form
   InvNum1 label " Invoice number .........."
      help "Invoices from number ..."
   "-" InvNum2 NO-LABEL 
      help "Invoices to number ..."                                 SKIP
   " Invoicing group .........:" InvGroup  NO-LABEL
      help "Invoice group to print, empty for all"
      InvGroup.IGName no-label format "x(20)"SKIP
   " Customer number .........:" CustNum1    NO-LABEL AT 29 
      help "Customers from number ..."
   "-" CustNum2 NO-LABEL 
      help "Customers to number ..."                                SKIP
   " External Customer Group .:" CustGroup NO-LABEL
      help "Code of an External Customer Group; (Empty = NONE)"
   CustGroup.CGName no-label  format "x(24)"  SKIP
   i-date1 label " Invoice date ............"
      help "From date (INVOICE DATE) ..." 
   "-" i-date2 NO-LABEL
      help "To date (INVOICE DATE) ..."                             SKIP
   " Printing state ..........:" status1  NO-LABEL AT 36
      help "Invoices from status code ..."
   "-" status2 NO-LABEL
      help "Invoices to status code ..."                            SKIP
   " Delivery type ...........:" liDelType NO-LABEL FORMAT ">9"
      HELP "If not 2 (eMail/PDF), then eMail can not be selected"  
      SKIP
   " Specification reports ...:"  rap no-label format "Yes/No"
      help "Print call specification reports for each invoice (Y/N)" SKIP
      
   " FORM PDF-file ...........:"  llFormPDF   NO-LABEL FORMAT "Yes/No"
      HELP "FORM PDF-files from invoices" SKIP
   " Send invoices via eMail .:"  llSendeMail NO-LABEL FORMAT "Yes/No"
      HELP "Send invoices to customers' eMail addresses" SKIP
   " Mail to customer/user ...:"
      llM2Cust 
      NO-LABEL 
      HELP "Send PDF to invoicing (C)ustomer or to TMS (U)ser (i.e. You)"
      FORMAT "Customer/User"

WITH TITLE COLOR value(ctc)
   " " + ynimi + " PDF INVOICE PRINTOUT " + STRING(pvm,"99-99-99") + " "
side-labels COLOR value(cfc) ROW 1 centered OVERLAY FRAME rajat.

form
    "  0: Not printed, 'new' invoices            " skip
    "  4: XML-file formed                        " skip
    "  5: PDF-file formed                        " skip
    "  6: PDF sent to customer via eMail         " skip
with
   title color value (ctc) " Printing Status For Invoices " color value(cfc)
   overlay centered row 14 frame statu.

ASSIGN 
   i-date1   = pvm
   i-date2   = pvm
   liDelType = 2.

cfc = "sel". RUN Syst/ufcolor. ccc = cfc.
view FRAME taka. PAUSE 0 no-message.

ehto = 9. RUN Syst/ufkey.
ASSIGN InvNum1 = 000000 InvNum2 = 99999999
       CustNum1  = 0 CustNum2 = 99999999.

view FRAME rajat. view FRAME statu.

disp "NONE" @ CustGroup.CGName WITH FRAME rajat.

IF si-recid2 NE ? THEN DO:

   FIND Invoice WHERE RECID(Invoice) = si-recid2 NO-LOCK NO-ERROR.

   IF Invoice.DelType NE 2 THEN DO:
      MESSAGE "Invoice's delivery type is not 2 (PDF via eMail)."
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.
              
   FIND FIRST Customer no-lock where
              Customer.CustNum = Invoice.CustNum.
   ASSIGN
      InvNum2  = InvNum1
      InvGroup = Customer.InvGroup
      i-date1  = Invoice.InvDate
      i-date2  = Invoice.InvDate
      CustNum1 = Customer.CustNum
      CustNum2 = Customer.CustNum
      status1  = Invoice.PrintState
      status2  = Invoice.PrintState
      si-recid2 = ?.
      
   DISP 
      InvNum1   InvNum2
      InvGroup 
      i-date1 i-date2
      CustNum1 CustNum2 
      status1 status2
   WITH FRAME rajat.
END.   

PAUSE 0 no-message.
LOOP:
repeat:

   /* KysellAAn rajaukset */
   ehto = 9. RUN Syst/ufkey.
   PAUSE 0 no-message.
   
    UPDATE
      InvNum1    InvNum2
      InvGroup
      CustNum1    CustNum2
      CustGroup
      i-date1  i-date2
      status1  status2
      liDelType
      rap
      llFormPDF 
      llSendeMail
      llM2Cust
   WITH FRAME rajat EDITING:
      
      READKEY. nap = keylabel(LASTKEY).

       IF nap = "F9" AND 
          FRAME-FIELD = "liDelType"
       THEN DO:

         RUN Help/h-tmscodes(INPUT "Invoice",     /* TableName*/
                              "DelType",       /* FieldName */
                              "Billing",     /* GroupCode */
                        OUTPUT lcCode).
             
         IF lcCode ne "" AND lcCode NE ?
         THEN DISPLAY INTEGER(lcCode) ;& liDelType WITH FRAME rajat.
                  
         ehto = 9.
         RUN Syst/ufkey.
         NEXT. 
      END.
 
      IF lookup(nap,poisnap) > 0 THEN DO:
         PAUSE 0.

         if frame-field = "InvNum2" THEN DO:
            ASSIGN INPUT InvNum1 INPUT InvNum2.
            IF INPUT InvNum2 = 0 THEN DO:
               InvNum2 = INPUT InvNum1.
               DISP InvNum1 @ InvNum2 WITH FRAME rajat.
               ASSIGN InvNum2.
            END.

            ELSE IF INPUT InvNum1 > INPUT InvNum2 THEN DO:
               InvNum1 = INPUT InvNum2.
               InvNum2 = INPUT InvNum1.
               DISP InvNum1 InvNum2
               WITH FRAME rajat.
               ASSIGN INPUT InvNum1 INPUT InvNum2.
            END.

            IF INPUT InvNum1 = INPUT InvNum2 THEN DO:

               FIND FIRST Invoice no-lock where
                          Invoice.InvNum  = INPUT InvNum1.
               FIND FIRST Customer no-lock where
                          Customer.CustNum = Invoice.CustNum.
               ASSIGN
                  InvGroup  = Customer.InvGroup
                  i-date1   = Invoice.InvDate
                  i-date2   = Invoice.InvDate
                  CustNum1  = Customer.CustNum
                  CustNum2  = Customer.CustNum
                  status1   = Invoice.PrintState
                  status2   = Invoice.PrintState
                  liDelType = Invoice.DelType.
               DISP 
                  InvGroup 
                  i-date1 i-date2
                  CustNum1 CustNum2 
                  status1 status2
                  liDelType
               WITH FRAME rajat.

            END.
         END.

         else if frame-field = "InvGroup" THEN DO:
             ASSIGN INPUT InvGroup.
             if InvGroup ne "" AND NOT
                can-find(invgroup where 
                         InvGroup.Brand    = gcBrand AND 
                         invgroup.InvGroup = InvGroup) THEN DO:
                BELL.
                message "UNKNOWN INVOICEGROUP !".
                NEXT-PROMPT InvGroup.
                NEXT.
             END.
         END.


         else if frame-field = "CustGroup" THEN DO WITH FRAME rajat:
             ASSIGN INPUT FRAME rajat CustGroup.
             if CustGroup = "" then disp "NONE" @ CustGroup.CGName.
             ELSE DO:
                FIND CustGroup where 
                     CustGroup.Brand     = gcBrand AND
                     CustGroup.custGroup = CustGroup 
                   no-lock no-error.
                IF NOT AVAIL CustGroup THEN DO:
                   BELL.
                   message "UNKNOWN EXTERNAL CUSTOMER GROUP !".
                   NEXT.
                END.
                DISP CustGroup.CGName.
             END.   
         END.

         else if frame-field = "CustNum2" THEN DO:
            ASSIGN INPUT CustNum1 INPUT CustNum2.
            IF INPUT CustNum2 = 0 THEN DO:
               CustNum2 = INPUT CustNum1.
               DISP CustNum1 @ CustNum2 WITH FRAME rajat.
               ASSIGN CustNum2.
            END.

            IF INPUT CustNum1 > INPUT CustNum2 THEN DO:
               CustNum1 = INPUT CustNum2.
               CustNum2 = INPUT CustNum1.
               DISP CustNum1 CustNum2
               WITH FRAME rajat.
               ASSIGN INPUT CustNum1 INPUT CustNum2.
            END.

         END.

         else if frame-field = "status2" THEN DO:
            IF INPUT status1 > INPUT status2 THEN DO:
               status1 = INPUT status2.
               status2 = INPUT status1.
               DISP status1 status2
               WITH FRAME rajat.
               ASSIGN INPUT status1 INPUT status2.
            END.
         END.

         ELSE IF FRAME-FIELD = "liDelType" THEN DO:
            IF INTEGER(INPUT FRAME rajat liDelType) NE 2
            THEN DISPLAY FALSE @ llSendeMail WITH FRAME rajat.
         END.

      END.
      APPLY LASTKEY.
   END.
   
   IF liDelType NE 2 THEN DO:
      llSendeMail = FALSE.
      DISPLAY llSendeMail WITH FRAME rajat.
   END.

   toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      ASSIGN
      ufk = 0 ufk[1] = 132 ufk[4] = 0 /* 797*/ ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.

      IF toimi = 1 THEN NEXT LOOP.

      ELSE IF toimi = 5 THEN DO:

         IF NOT llFormPDF AND NOT llSendeMail THEN DO:
            MESSAGE "At least one action must be chosen."
            VIEW-AS ALERT-BOX
            ERROR.
            NEXT.
         END.
         
         IF NOT llFormPDF AND status2 < 5 THEN DO:
            MESSAGE "If only eMail sending is chosen then status of"
                    "invoices must be at least 5."
            VIEW-AS ALERT-BOX
            ERROR.
            NEXT.
         END.
         
         IF INPUT CustNum2  = 0  THEN InvNum2 = 99999999.
         if input CustNum2   = "" THEN CustNum2  = 9999999.
         
         llok = FALSE.
         
         /* are customers in the ext group in correct i-group also ? */
         if CustGroup ne "" THEN DO:
            FOR EACH  cgmember no-lock where 
                      cgMember.Brand     = gcBrand AND
                      cgmember.custgroup = CustGroup,
                FIRST Customer no-lock where
                      Customer.CustNum  = cgmember.custnum AND
                      Customer.InvGroup = InvGroup:
                llok = TRUE.
                LEAVE.
            END.
            IF NOT llok THEN DO:
               PAUSE 0.
               DISP                                         skip(1)
               " All customers in the selected external "   SKIP
               " CUSTOMER  Group belong to wrong "          SKIP
               " INVOICING Group !"                         skip(1)
               " Change either of those 2 group codes and " SKIP
               " try again."                                skip(1)
               " PRESS ENTER TO CONTINUE"               skip(1)
               WITH centered OVERLAY ROW 8 FRAME mm  TITLE
               " INVOICING / CUSTOMER GROUP MISMATCH ! ".
               PAUSE no-message.
               NEXT TOIMI.
            END.

         END.

         LEAVE toimi.
      END. /* toimi = 5 */

      ELSE IF toimi = 8 THEN LEAVE LOOP.
      
   END. /* toimi */

   RUN Inv/pdfinvcl (InvNum1, 
                 InvNum2,
                 i-date1,
                 i-date2,
                 CustNum1,
                 CustNum2,
                 status1, 
                 status2,
                 InvGroup,
                 CustGroup,
                 liDelType,
                 rap,
                 llFormPDF,
                 (IF llSendeMail
                  THEN (IF llM2Cust
                        THEN 1
                        ELSE 2)
                  ELSE 0),
                 OUTPUT liCount,
                 OUTPUT liMailSent,
                 OUTPUT liErrors). 
   
   MESSAGE liCount    "PDF-files were made from invoices." SKIP
           liMailSent "were sent via eMail." SKIP
           liErrors   "errors occurred during process."                    
   VIEW-AS ALERT-BOX
   INFORMATION.
   
   LEAVE LOOP.

END.  /* LOOP */

HIDE MESSAGE NO-pause.
HIDE FRAME rajat  NO-pause.
HIDE FRAME statu  NO-pause.
HIDE FRAME taka   NO-pause.


