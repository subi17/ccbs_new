/* ----------------------------------------------------------------------
  MODULE .......: createkeys.p
  TASK .........: create username and userkeys records
  APPLICATION ..: TMS
  AUTHOR .......: TK
  CREATED ......: 13.05.04
  CHANGED ......: 04.08.05 tk new password
   
  Version ......: Tele Finland
  ---------------------------------------------------------------------- */

{commali.i}
              
DEF INPUT PARAMETER MsSeq LIKE Mobsub.MsSeq NO-UNDO.
              
FIND FIRST Mobsub NO-LOCK WHERE MobSub.MsSeq = MsSeq.
FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
              
IF NOT AVAIL MobSub THEN RETURN.
              
FIND FIRST username EXCLUSIVE-LOCK WHERE
           username.usercode = MobSub.CLI NO-ERROR.

IF NOT AVAIL username THEN DO:
   CREATE username.
   ASSIGN
      username.usercode = Mobsub.CLI.
END.

ASSIGN 
   username.email        = Customer.email   
   username.lastname     = Customer.CustName   
   username.firstname    = Customer.firstname   
   username.menulanguage = "fin"
   username.password     = string(random(10000,99999)).
                 
find first userkeys no-lock where
           userkeys.usercode = MobSub.CLI  AND
           userkeys.keytype  = "CLI"       AND
           userkeys.key      = MobSub.CLI no-error. 
           
if not avail userkeys then do:
   create userkeys.
   assign  
      userkeys.usercode = Mobsub.CLI 
      userkeys.keytype  = "CLI"  
      userkeys.key      = MobSub.CLI.  
end.

for each userkeys where 
         userkeys.usercode = MobSub.CLI AND
         userkeys.keytype  = "Customer" AND
         userkeys.key     NE STRING(MobSub.CustNum):
         
   delete userkeys.     

end.                  
                  
find first userkeys no-lock where
           userkeys.usercode = Mobsub.CLI  AND
           userkeys.keytype  = "Customer"  AND
           userkeys.key      = STRING(MobSub.CustNum) no-error. 
           
if not avail userkeys then do:
   create userkeys.
   assign  
      userkeys.usercode = Mobsub.CLI 
      userkeys.keytype  = "Customer"  
      userkeys.key      = STRING(MobSub.CustNum).  
end.
 
