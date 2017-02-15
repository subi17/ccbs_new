/**/
{Func/multitenantfunc.i}
fsetEffectiveTenantForAllDB("Default").
input from ../tms_support/utilities/multitenant/customer.d.

repeat:
   create customer.
   import customer.
end.
