for each servpac no-lock where
         brand = "1" ,
    each ctservpac no-lock where
         ctservpac.brand = "1" and
         ctservpac.servpac = servpac.servpac:
         
         
   for each servel of servpac no-lock where
            servel.servcom = "BCG",
      first servcom no-lock where
            servcom.brand = "1" and
            servcom.servcom = servel.servcom:
      
      if not can-find(first ctservel where
            CTServEl.Brand   = "1"   AND
            CTServEl.CLIType = CTServPac.CLIType  AND
            CTServEl.ServPac = CTServPac.ServPac  AND
            ctservel.servcom = servel.servcom)
      then do:
    
         disp ctservpac.clitype ctservpac.servpac
              servel.servcom.

         create ctservel.
         assign 
            CTServEl.Brand      = "1"
            CTServEl.CTServEl   = NEXT-VALUE(CTServEl)
            CTServEl.CLIType    = ctservpac.CLIType
            CTServEl.ServPac    = ctservpac.ServPac
            CTServEl.ServCom    = ServEl.ServCom
            CTServEl.FromDate   = 7/1/8
            CTServEl.ChgAllowed = ServCom.SCChgable
            CTServEl.ServType   = ServCom.ServType
            CTServEl.DefValue   = ServEl.SeValue.

      end.
   end.
   
end.
    