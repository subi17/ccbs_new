/* ------------------------------------------------------
  MODULE .......: custpinlet.p
  FUNCTION .....: order a pin for activating periodical contract
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 09.02.06
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/fctserval.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

DEF VAR llOk      AS LOG  NO-UNDO.
DEF VAR liCreated AS INT  NO-UNDO.
DEF VAR lcError   AS CHAR NO-UNDO. 

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF Customer.CustNum NE Customer.AgrCust THEN DO:
   MESSAGE "Customer is not an agreement customer"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

/* is there a change request already */
IF fPendingRequest(iiCustNum,7) THEN DO:
   MESSAGE "There is a pending request for PIN letter."
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END. 
                   

FORM
   SKIP(1)
   "A letter containing PIN for activating periodical"  AT 5 SPACE(5) SKIP
   "contract will be sent to customer."                 AT 5 SKIP(1)
   WITH ROW 7 OVERLAY SIDE-LABELS CENTERED 
        TITLE " PIN LETTER, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   PAUSE 0.
   VIEW FRAME fCriter.

   ASSIGN
      ufk   = 0  
      ufk[5]= 1027  
      ufk[8]= 8 
      ehto = 0.
   RUN ufkey.

   IF toimi = 5 THEN DO:

      llOk = FALSE.
      MESSAGE "A PIN letter will be sent." SKIP
              "Continue with request creation ?" 
      VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO 
      SET llOk.
      
      IF NOT llOk THEN NEXT.
      
      liCreated = fPerContractPIN(iiCustNum,
                                  "",
                                  ?,
                                  OUTPUT lcError).
                                      
      IF liCreated > 0 THEN 
         MESSAGE "Request was created with ID" liCreated
         VIEW-AS ALERT-BOX INFORMATION.
         
      ELSE 
         MESSAGE "Request could not be created;" SKIP
                 lcError
         VIEW-AS ALERT-BOX ERROR.

      LEAVE.
   END.
   
   ELSE IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

