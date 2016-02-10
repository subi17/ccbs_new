/* ------------------------------------------------------
  MODULE .......: custmarket.p
  FUNCTION .....: customer's marketing permits
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 17.01.06
  MODIFIED .....: 14.11.06/aam RobinsonsLimit
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventval.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

IF llDoEvent THEN DO FOR Customer:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustomer).
   END.

END.

DEF VAR llOk  AS LOG  NO-UNDO.

FORM
   SKIP(1)
   "Robinsons Limit"  AT 4 SKIP
   Customer.RobinsonsLimit COLON 15
      LABEL "Limit"
   SKIP(1)
      
   "Direct Marketing" AT 4 SKIP
   Customer.DirMarkSMS COLON 15 
      LABEL "SMS"
      FORMAT "Yes/No"
      SKIP
   Customer.DirMarkEMail COLON 15
      LABEL "EMail"
      FORMAT "Yes/No"
      SKIP
   Customer.DirMarkPost COLON 15
      LABEL "Post"
      FORMAT "Yes/No"
      SPACE(2)
   SKIP(1)

   "3rd Party Marketing" AT 4 SKIP   
   Customer.OutMarkSMS COLON 15
      LABEL "SMS"
      FORMAT "Yes/No"
      SKIP
   Customer.OutMarkEMail COLON 15
      LABEL "EMail"
      FORMAT "Yes/No"
      SKIP
   Customer.OutMarkPost COLON 15
      LABEL "Post"
      FORMAT "Yes/No"
      SKIP
   Customer.OutMarkBank COLON 15
      LABEL "Bank"
      FORMAT "Yes/No"
   SKIP(1)

   WITH ROW 3 OVERLAY SIDE-LABELS CENTERED WIDTH 40
        TITLE " MARKETING PERMITS, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
      
   PAUSE 0.
   DISPLAY Customer.DirMarkSMS 
           Customer.DirMarkEMail
           Customer.DirMarkPost
           Customer.OutMarkSMS
           Customer.OutMarkEMail
           Customer.OutMarkPost
            Customer.OutMarkBank
           Customer.RobinsonsLimit.

   ASSIGN
      ufk   = 0  
      ufk[1]= 7  
      ufk[8]= 8 
      ehto = 0.
   RUN Syst/ufkey.

   IF toimi = 1 THEN DO:

      REPEAT WITH FRAME fCriter ON ENDKEY UNDO, LEAVE:
            
         ehto = 9. RUN Syst/ufkey.
         
         FIND CURRENT Customer EXCLUSIVE-LOCK.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustomer).     
         
         UPDATE Customer.RobinsonsLimit
                Customer.DirMarkSMS 
                Customer.DirMarkEMail
                Customer.DirMarkPost
                Customer.OutMarkSMS
                Customer.OutMarkEMail
                Customer.OutMarkPost
                Customer.OutMarkBank.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustomer).
          
         RELEASE Customer.
         
         LEAVE.
      END.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

