/* ------------------------------------------------------
  MODULE .......: custpcpin.p
  FUNCTION .....: customer's pin for periodical contract
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 09.02.06
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/fwebuser.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 

DEF VAR llOk      AS LOG  NO-UNDO.
DEF VAR lcDispPwd AS CHAR NO-UNDO.
DEF VAR lcAskPwd  AS CHAR NO-UNDO. 

FORM
   SKIP(1)
   Customer.PContractPIN AT 2 
   SKIP(1)

   WITH ROW 7 OVERLAY SIDE-LABELS CENTERED 
        TITLE " PIN, CUSTOMER " + STRING(iiCustNum) + " " 
        FRAME fCriter.

FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF Customer.CustNum NE Customer.AgrCust THEN DO:
   MESSAGE "Customer is not an agreement customer"
   VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.

VIEW FRAME fCriter.
PAUSE 0 NO-MESSAGE.

lCustMark:
REPEAT WITH FRAME fCriter ON ENDKEY UNDO lCustMark, NEXT lCustMark:

   PAUSE 0.
   DISPLAY Customer.PContractPIN 
   WITH FRAME fCriter.

   ASSIGN
      ufk   = 0  
      ufk[8]= 8 
      ehto = 0.
   RUN Syst/ufkey.

   IF toimi = 8 THEN LEAVE.

END. /* lCustMark */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCriter NO-PAUSE.    

