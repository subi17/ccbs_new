/*
   RES-885 National roaming traffic restrreiction.
   Mass integration for all customers.
   Initializes Customer.OrgId values.
*/
{Syst/tmsconst.i}

def STREAM sout.

DEFINE VARIABLE iDispInterval AS INTEGER NO-UNDO INITIAL 10000.
DEFINE VARIABLE iDate AS INT NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

OUTPUT STREAM sout TO res_885_migrate_cust_profile.log append.

FOR EACH Customer EXCLUSIVE-LOCK WHERE
   Customer.Roles NE "inactive":
   IF Customer.ChgStamp < 20180401 THEN
      ASSIGN
         Customer.NWProfile = {&CUSTOMER_NW_PROFILE_YG_OR_TEL}.
   ELSE
      ASSIGN
         Customer.NWProfile = {&CUSTOMER_NW_PROFILE_YG_OR}.

   iCount = iCount + 1.
   IF iDispInterval > 0 AND iCount MOD iDispInterval EQ 0 THEN DO:
      DISP iCount with frame a.
      pause 0.
   END.
END.

RELEASE Customer.

PUT STREAM sout UNFORMATTED "Updated " iCount  " Customer rows." SKIP.

