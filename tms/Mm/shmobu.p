/* ----------------------------------------------------------------------
  MODULE .......: shmobu.p
  TASK .........: Show User Data (MobSub record) of an IMSI
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 20-06-99
  CHANGED ......: 03.08.99 mu-Lname, mu-Fname
                  03.03.03 tk tokens
                  13.01.06/aam name and others from customer
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}

DEF VAR lcCustName AS CHAR NO-UNDO.
DEF INPUT PARAMETER UserSeq AS INT NO-UNDO.

form
    lcCustName                         
    Customer.COName                         
    Customer.Address    
    Customer.ZipCode                      
    Customer.PostOffice
    Customer.Country
    Country.CoName
    Customer.Email

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(Syst.Var:cfc)
    title COLOR VALUE(Syst.Var:ctc) " Subscriber Data "
    side-labels 1 columns FRAME subs.


DO WITH FRAME subs:
   PAUSE 0.
   FIND MobSub WHERE MobSub.MsSeq = UserSeq NO-LOCK NO-ERROR. 
   IF NOT AVAIL MobSub THEN DO:
      message "No Subscriber record available !"
      VIEW-AS ALERT-BOX error.
      RETURN.
   END. 

   FIND Customer OF MobSub NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      MESSAGE "User customer not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   lcCustName = Func.Common:mDispCustName(BUFFER Customer).
   FIND Country WHERE Country.Country = Customer.Country NO-LOCK NO-ERROR.

   DISP
      lcCustName 
      Customer.CoName                         
      Customer.Address    
      Customer.ZipCode                      
      Customer.PostOffice
      Customer.Country
      Country.CoName    WHEN AVAIL Country
      Customer.Email

   WITH FRAME subs.

   ASSIGN
   Syst.Var:ufk = 0 Syst.Var:ufk[8] = 8 Syst.Var:ehto = 0.
   RUN Syst/ufkey.p.

   HIDE FRAME subs NO-PAUSE.
END.   


