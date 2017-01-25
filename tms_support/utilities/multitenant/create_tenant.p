routine-level on error undo, throw.
using Progress.Lang.*.
using OpenEdge.DataAdmin.*.
using OpenEdge.DataAdmin.Error.*.

DEFINE INPUT PARAMETER icTenant AS CHAR.
DEFINE INPUT PARAMETER icTenantType AS CHAR.
DEFINE INPUT PARAMETER icDBName AS CHAR.

define variable service as DataAdminService no-undo.
define variable errorhandler as DataAdminErrorHandler no-undo.
define variable tenant as ITenant no-undo.
define variable domain as IDomain no-undo.
define variable usr as IUser no-undo.

service = NEW DataAdminService(icDBName).

IF icTenantType NE "Default" THEN DO:

   tenant = service:NewTenant("T" + icTenant).  

   /* Create a tenant */
   IF icTenantType EQ "Super" THEN DO:
      tenant:IsDataEnabled = true.
      tenant:Type = icTenantType.
   END.
   ELSE DO:
      tenant:name = "T" + icTenant.
      tenant:Type = icTenantType.
      tenant:IsDataEnabled = TRUE.   /* enabled for access */
      tenant:DefaultAllocation = "Immediate".
      tenant:DefaultDataArea  = service:GetArea("Sta_Data_64").
      tenant:DefaultIndexArea = service:GetArea("Sta_Index_64").
      tenant:DefaultLobArea   = service:GetArea("Sta_Data_64").
   END.

   service:CreateTenant(tenant).
END.

/* Create a domain */
domain = service:NewDomain("D" + icTenant).
IF icTenantType EQ "Default" THEN
   domain:Tenant = service:GetTenant("Default").
ELSE 
   domain:Tenant = service:GetTenant("T" + icTenant).
domain:AuthenticationSystem = service:GetAuthenticationSystem("_oeusertable").
domain:AccessCode =  "D" + icTenant.
domain:IsEnabled = TRUE.
service:CreateDomain(domain). 

/* Create user */
usr = service:NewUser(lower(icTenant) + "@" + domain:name).
usr:Password = lower(ictenant).
usr:Description = subst("Generic &1 user", icTenant).
usr:Domain = domain.
service:CreateUser(usr).

delete object service. 
catch e as  Error :
    errorHandler =  new DataAdminErrorHandler().
    errorHandler:Error(e).      
end catch.

