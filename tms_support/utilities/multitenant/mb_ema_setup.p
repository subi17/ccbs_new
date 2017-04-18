DEF VAR liloop AS INT NO-UNDO.
DEF VAR liTenantCount AS INT NO-UNDO.

liTenantCount = fgetMaxTenantId(). /* as super, go through all tenants */
DO liLoop = 0 to liTenantCount:
   fsetEffectiveTenantForAllDB(fgetTenantNamebyId(liLoop)).
   FOR EACH MSISDN WHERE MsIsdn.brand EQ "1"
                         msisdn.cli BEGINS "72260000":
      MSISDN.POS = "VIP".
   END.
END.
