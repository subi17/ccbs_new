/* ------------------------------------------------------
  MODULE .......: custcont.p
  FUNCTION .....: customer's contact data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 14.11.06
  MODIFIED .....: 
  Version ......: yoigo
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/femailinvoice.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustomer).
   END.

END.

DEF VAR llOk  AS LOG  NO-UNDO.
DEF VAR lcResult AS CHAR NO-UNDO. 
DEF VAR liRequest AS INT NO-UNDO. 
DEF VAR lcEmailAddress AS CHAR NO-UNDO.
DEF VAR lcMemo    AS CHAR  NO-UNDO. 
DEF VAR liEmail_validated AS INT NO-UNDO. /* APIBSS-188 */

ASSIGN lcMemo = "Agent" + CHR(255) + "TMS".

FORM
   SKIP(1)
   Customer.Email COLON 15      
      LABEL "Email"  FORMAT "X(50)"  SKIP
  
   Customer.SMSNumber COLON 15
     LABEL "Mobile Number"
     HELP  "Mobile contact number"
     FORMAT "X(17)" SKIP
       
  Customer.Phone COLON 15
     LABEL "Fixed Number" 
     HELP  "Fixed contact number"
     FORMAT "X(17)"  
   
   SKIP(1)

   WITH ROW 4 OVERLAY SIDE-LABELS CENTERED 
        TITLE " CONTACT DATA, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
      
   PAUSE 0.
   DISPLAY Customer.Email
           Customer.SMSNumber
           Customer.Phone.

   ASSIGN
      Syst.Var:ufk   = 0  
      Syst.Var:ufk[1]= 7  
      Syst.Var:ufk[5]= 1096 WHEN CAN-FIND(FIRST CustContact WHERE
                                       CustContact.Brand = Syst.Var:gcBrand AND
                                       CustContact.Custnum = Customer.Custnum AND
                                       CustContact.CustType = 5)
      Syst.Var:ufk[8]= 8 
      Syst.Var:ehto = 0.
   RUN Syst/ufkey.p.

   IF Syst.Var:toimi = 1 THEN DO:

      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
            
         Syst.Var:ehto = 9. RUN Syst/ufkey.p.
         
         PROMPT Customer.Email
                Customer.SMSNumber
                Customer.Phone.

         IF Customer.Email NE INPUT Customer.Email THEN DO:
            IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
               Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

               IF INPUT Customer.Email = "" THEN DO:
                  MESSAGE "Customer email address can not be blank because " +
                          "customer's invoice delivery type is EMAIL."
                  VIEW-AS ALERT-BOX ERROR.
                  LEAVE.
               END. /* IF INPUT Customer.Email = "" THEN DO: */

               MESSAGE "Customer's invoice delivery type is EMAIL. " +
                       "Activation email will be mailed to customer. " + 
                       "Are you sure you want to continue?" 
               VIEW-AS ALERT-BOX BUTTONS YES-NO
               TITLE "Email Address Change" UPDATE llOK.
               IF NOT llOK THEN DO:
                  MESSAGE "Cancelled" VIEW-AS ALERT-BOX.
                  LEAVE.
               END. /* IF NOT llOK THEN DO: */
            END. /* IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR */
            ELSE DO:
               FIND FIRST InvoiceTargetGroup WHERE
                          InvoiceTargetGroup.CustNum = Customer.CustNum AND
                          InvoiceTargetGroup.ToDate >= TODAY AND
                         (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                          InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                    NO-LOCK NO-ERROR.
               IF AVAIL InvoiceTargetGroup THEN DO:
                  IF INPUT Customer.Email = "" THEN DO:
                     MESSAGE "Customer email address can not be blank because " +
                             "customer's has Fusion product."
                        VIEW-AS ALERT-BOX ERROR.
                     LEAVE.
                  END. /* IF INPUT Customer.Email = "" THEN DO: */

                  MESSAGE "Customer has Fusion product. " +
                          "Validation email will be mailed to customer. " + 
                          "Are you sure you want to continue?" 
                     VIEW-AS ALERT-BOX BUTTONS YES-NO
                     TITLE "Email Address Change" UPDATE llOK.
                  IF NOT llOK THEN DO:
                     MESSAGE "Cancelled" VIEW-AS ALERT-BOX.
                     LEAVE.
                  END. /* IF NOT llOK THEN DO: */
               END. /* IF AVAIL InvoiceTargetGroup THEN DO: */
            END. /* ELSE DO: */
         END. /* IF Customer.Email NE INPUT Customer.Email THEN DO: */

         FIND CURRENT Customer EXCLUSIVE-LOCK.
         
         IF CURRENT-CHANGED Customer THEN DO:
            
            RELEASE Customer.

            MESSAGE 
               "This record has been changed elsewhere while updating" 
            VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

         END.
         ELSE DO:
      
            IF Customer.Email NE INPUT Customer.Email THEN DO:
               ASSIGN
                  lcEmailAddress = INPUT Customer.Email
                  liEmail_validated = 1.  /* APIBSS-188 1 = Not validated */

               IF Customer.DelType EQ {&INV_DEL_TYPE_EMAIL} OR
                  Customer.DelType EQ {&INV_DEL_TYPE_EMAIL_PENDING} THEN DO:

                  /* Cancel Ongoing Email Activation Request and create new */
                  IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
                     fCancelPendingEmailActRequest(
                                     INPUT Customer.Custnum,
                                     INPUT "Customer email address is changed").

                  liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                                   INPUT TODAY,
                                                   INPUT Syst.Var:katun,
                                                   INPUT 0, /* msseq */
                                                   INPUT "", /* cli */
                                                   INPUT Customer.CustNum,
                                                   INPUT {&REQUEST_SOURCE_MANUAL_TMS},
                                                   INPUT lcEmailAddress,
                                                   INPUT 0, /*orderid*/
                                                   OUTPUT lcResult).
                  IF liRequest = 0 THEN DO:
                     RELEASE Customer.
                     MESSAGE 
                        "Customer email address can not be changed: " + lcResult
                     VIEW-AS ALERT-BOX ERROR.
                     LEAVE.
                  END. /* IF liRequest = 0 THEN DO: */

                  /* If Email already validated then mark DelType EMAIL */
                  IF liRequest = 1 THEN
                     Customer.DelType = {&INV_DEL_TYPE_EMAIL}.
                  ELSE
                     Customer.DelType = {&INV_DEL_TYPE_EMAIL_PENDING}.
               END.

               FIND FIRST InvoiceTargetGroup WHERE
                          InvoiceTargetGroup.CustNum = Customer.CustNum AND
                          InvoiceTargetGroup.ToDate >= TODAY AND
                         (InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} OR
                          InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL})
                    EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL InvoiceTargetGroup THEN DO:
                  /* If request is not already created from Customer DelType */
                  IF liRequest = 0 THEN DO:
                     /* Cancel existing ReqType=84 request */
                     IF fPendingEmailActRequest(INPUT Customer.Custnum) THEN
                        fCancelPendingEmailActRequest(
                                     INPUT Customer.Custnum,
                                     INPUT "Customer email address is changed").

                     liRequest = fEmailInvoiceRequest(INPUT Func.Common:mMakeTS(),
                                                      INPUT TODAY,
                                                      INPUT Syst.Var:katun,
                                                      INPUT 0, /* msseq */
                                                      INPUT "", /* cli */
                                                      INPUT Customer.CustNum,
                                                      INPUT {&REQUEST_SOURCE_FUSION_EMAIL},
                                                      INPUT lcEmailAddress,
                                                      INPUT 0, /*orderid*/
                                                      OUTPUT lcResult).
                     IF liRequest = 0 THEN DO:
                        RELEASE Customer.
                        RELEASE InvoiceTargetGroup.
                        MESSAGE 
                           "Customer email address can not be changed: " + lcResult
                           VIEW-AS ALERT-BOX ERROR.
                        LEAVE.
                     END. /* IF liRequest = 0 THEN DO: */
                  END. /* IF liRequest = 0 THEN DO: */

                  /* If Email already validated then mark DelType EMAIL */
                  IF liRequest = 1 THEN
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL}.
                  ELSE
                     InvoiceTargetGroup.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}.

                  RELEASE InvoiceTargetGroup.
               END. /* IF AVAIL InvoiceTargetGroup THEN DO: */
            END.
            
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).     

            ASSIGN
               Customer.Email
               Customer.SMSNumber
               Customer.Phone
               Customer.Email_validated = liEmail_validated. /* APIBSS-188 */           
 
            IF llDoEvent THEN 
               RUN StarEventMakeModifyEventWithMemo(lhCustomer, 
                                                    {&STAR_EVENT_USER}, 
                                                    lcMemo).
         
            RELEASE Customer.
         END.
         
         LEAVE.
      END.
   END.
   ELSE IF Syst.Var:toimi = 5 AND Syst.Var:ufk[5] > 0 THEN DO:
      RUN Mc/custcontact.p(customer.custnum, 5).
   END.
   
   ELSE IF Syst.Var:toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    



