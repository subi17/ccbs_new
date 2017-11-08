routine-level on error undo, throw.
using Progress.Lang.*.
using OpenEdge.DataAdmin.*.
using OpenEdge.DataAdmin.Error.*.

DEFINE INPUT PARAMETER icDBName AS CHARACTER NO-UNDO. 

define variable errorhandler as DataAdminErrorHandler no-undo.

DO TRANSACTION:

   MESSAGE "Creating masmovil tenant".
   RUN create_tenant.p ("masmovil", "Regular", icDBName).

   MESSAGE 'Creating super tenant'.
   RUN create_tenant.p ("super", "Super", icDBName).

   MESSAGE 'Creating yoigo/default user'.
   RUN create_tenant.p ("yoigo", "Default", icDBName).

END.


catch e as  Error :
    errorHandler =  new DataAdminErrorHandler().
    errorHandler:Error(e).
end catch.

