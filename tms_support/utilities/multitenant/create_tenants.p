routine-level on error undo, throw.
using Progress.Lang.*.
using OpenEdge.DataAdmin.*.
using OpenEdge.DataAdmin.Error.*.

DEFINE INPUT PARAMETER icDBName AS CHARACTER NO-UNDO. 

define variable errorhandler as DataAdminErrorHandler no-undo.

DO TRANSACTION:

   MESSAGE "Creating masmovil tenant".
   RUN ../tms_support/utilities/multitenant/create_tenant.p ("Masmovil", "Regular", icDBName).

   MESSAGE 'Creating super tenant'.
   RUN ../tms_support/utilities/multitenant/create_tenant.p ("Super", "Super", icDBName).

   MESSAGE 'Creating yoigo/default user'.
   RUN ../tms_support/utilities/multitenant/create_tenant.p ("Yoigo", "Default", icDBName).

END.


catch e as  Error :
    errorHandler =  new DataAdminErrorHandler().
    errorHandler:Error(e).
end catch.

