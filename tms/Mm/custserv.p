/* ----------------------------------------------------------------------
  MODULE .......: Custser.P
  TASK .........: UPDATE Mobile subs. Services
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 29-06-99
  CHANGED ......: 03.03.03 tk tokens
                  13.12.04/aam SSDate for SubSer
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'mobsub'}

DEFINE INPUT PARAMETER CustNum AS INT NO-UNDO .
DEFINE VARIABLE lcCustName AS CHARACTER NO-UNDO.

FIND Customer WHERE 
     customer.custnum = CustNum NO-LOCK NO-ERROR.
lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                               BUFFER Customer).
                                    
DEFINE TEMP-TABLE TTservice
FIELD servcom  LIKE subser.servcom
FIELD ServName LIKE Servcom.SCName
FIELD ssstat   LIKE subser.ssstat
FIELD Qty      AS   INTEGER
INDEX servcom IS PRIMARY servcom ssstat .

FOR EACH Mobsub WHERE 
         mobsub.CustNum  = CustNum NO-LOCK.

   FOR EACH subser WHERE 
            subser.msseq = mobsub.msseq  NO-LOCK,
      FIRST servcom WHERE
            ServCom.Brand   = MobSub.Brand AND
            servcom.servcom = subser.servcom NO-LOCK 
   BREAK BY SubSer.ServCom
         BY SubSer.SSDate DESC:

      /* use newest */
      IF NOT FIRST-OF(SubSer.ServCom) THEN NEXT.
      
      FIND FIRST TTservice WHERE
                 TTservice.servcom = subser.servcom AND
                 TTservice.ssstat  = subser.ssstat  EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL TTservice THEN DO:           

          CREATE TTservice.
          ASSIGN
          TTservice.servcom  = subser.servcom
          TTservice.ServName = Servcom.SCName
          TTservice.ssstat   = subser.ssstat.
      END.
      ASSIGN
      TTservice.Qty = TTservice.Qty + 1.
   END.      
END.         

FOR EACH TTservice no-lock.

   Disp 
   TTservice.servcom   FORMAT "x(5)"
   TTservice.ServName  FORMAT "x(48)"
   TTservice.ssstat  
   TTservice.Qty       FORMAT "zz9"
   WITH CENTERED OVERLAY ROW 3 13 DOWN
   TITLE " ALL SERVICE PARAMETERS AT CUST. " + STRING(Customer.CustNum) + ": " 
   + lcCustName + " " FRAME Terminals.

   IF FRAME-LINE(Terminals) < FRAME-DOWN(Terminals) THEN DOWN.
   ELSE DO:
      PAUSE 0.
      MESSAGE "MORE SERVICES: PRESS ENTER !".
      PAUSE NO-MESSAGE.
   END.


END.
PAUSE 0.
MESSAGE "PRESS ENTER TO CONTINUE !".
PAUSE NO-MESSAGE.

