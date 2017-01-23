routine-level on error undo, throw.
using Progress.Lang.*.
using OpenEdge.DataAdmin.*.
using OpenEdge.DataAdmin.Error.*.

DEFINE INPUT PARAMETER icDBName AS CHARACTER NO-UNDO. 

define variable service as DataAdminService no-undo.
define variable errorhandler as DataAdminErrorHandler no-undo.
define variable usr as IUser no-undo.

DO TRANSACTION:

   MESSAGE "Creating tenant".
   RUN create_tenant.p ("Masmovil", "Regular", icDBName).

   MESSAGE 'Creating super tenant'.
   RUN create_tenant.p ("Super", "Super", icDBName).

   MESSAGE 'Creating default user'.

   service = NEW DataAdminService(icDBName).
   usr = service:NewUser("yoigo").
   usr:Password = "yoigo".
   usr:Description = "Generic default user".
   service:CreateUser(usr).

END.

delete object service.

catch e as  Error :
    errorHandler =  new DataAdminErrorHandler().
    errorHandler:Error(e).
end catch.

