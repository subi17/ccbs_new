using OpenEdge.DataAdmin.*.
using OpenEdge.DataAdmin.Lang.Collections.*.

define variable service as DataAdminService no-undo.
input from ../tms_support/utilities/multitenant/activate_table_tenancies.input.

define variable intTable as ITable no-undo.
DEF VAR lcLine AS CHAR NO-UNDO. 
DEFINE VARIABLE lcTable AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcDB AS CHARACTER NO-UNDO. 

repeat with frame a:

   import unformatted lcLine.
   
   if lcLine begins "||" then next.
   
   /* change DB if needed */
   if lcDB NE trim(entry(2,lcLine,"|")) then do:
      lcDB = trim(entry(2,lcLine,"|")).
      if VALID-OBJECT(service) then delete object service.
      service = new DataAdminService(lcDB).
   end.

   if entry(5,lcLine,"|") ne "T" then next.
   lcTable = trim(entry(3,lcLine,"|")).

   intTable = service:GetTable(lcTable).
   disp lcDB lcTable intTable:IsMultitenant.

   IF intTable:IsMultitenant NE true then do on error undo, throw:
      intTable:IsMultitenant = true.
      service:UpdateTable(intTable).
   end.
   CATCH oneError AS Progress.Lang.SysError: 
     MESSAGE oneError:GetMessage(1) VIEW-AS ALERT-BOX BUTTONS OK. 
     LEAVE.
   END CATCH.

end.


if VALID-OBJECT(service) then delete object service.
