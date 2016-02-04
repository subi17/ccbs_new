/* ----------------------------------------------------------------------
  MODULE .......: custer.p
  TASK .........: Show all Terminals of a customer
  APPLICATION ..: MASTER
  AUTHOR .......: jp
  CREATED ......: 
  CHANGED ......: 03.03.03 tk tokens
                  30.12.04/aam Secret from services
                  24.01.06/jt DYNAMIC-FUNCTION("fDispCustName"
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}

{Func/fsubser.i}

DEF INPUT  PARAMETER CustNum     AS I  NO-UNDO .

DEF VAR  amt       AS I   NO-UNDO.
DEF VAR llSecret   AS LOG NO-UNDO FORMAT "Yes/No".
DEF VAR lcCustName AS CHAR NO-UNDO.
ASSIGN
   ufk = 0 ehto = 3. RUN Syst/ufkey.

   FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK.
   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                  BUFFER Customer).
                                       
   IF NOT CAN-FIND(FIRST mobsub OF Customer) THEN DO:
      MESSAGE "Customer" CustNum "has NO terminals"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   PAUSE 0.
   FOR
   EACH  mobsub NO-LOCK WHERE
         mobsub.CustNum = Customer.CustNum:

      amt = amt + 1.

      llSecret = fSecretValue(MobSub.MsSeq).
      
      DISP
      amt                     LABEL "Amt" Format "zz9"
      mobsub.CLI
      mobsub.ActivationDate
      mobsub.MsStatus
      mobsub.Contract
      llSecret
      WITH CENTERED OVERLAY ROW 3 11 DOWN
      TITLE " ALL ACTIVE TERMINALS AT CUST. " + STRING(Customer.CustNum) + 
      ": " +  lcCustName + " " FRAME SUB.


      IF FRAME-LINE(sub) < FRAME-DOWN(sub) THEN DOWN.
      ELSE DO:
         PAUSE 0.
         MESSAGE "MORE USERS: PRESS ENTER !".
         PAUSE NO-MESSAGE.
      END.   


   END.
   PAUSE 0.
   MESSAGE "PRESS ENTER TO CONTINUE !".
   PAUSE NO-MESSAGE.

   HIDE FRAME sub NO-PAUSE.
   
