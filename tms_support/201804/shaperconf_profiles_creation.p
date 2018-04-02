CREATE ShaperConf.
ASSIGN
   ShaperConf.Brand         = Syst.Var:gcBrand
   ShaperConf.ShaperConfID  = "CONT28"
   ShaperConf.Template      = "HSPA_MONTHLY"
   ShaperConf.TariffType    = "PLANA_HI"
   ShaperConf.Tariff        = "PLANA_I"
   ShaperConf.LimitUnshaped = 104857600 
   ShaperConf.LimitShaped   = 20971520
   ShaperConf.Active        = YES.

CREATE ShaperConf.
ASSIGN
   ShaperConf.Brand         = Syst.Var:gcBrand
   ShaperConf.ShaperConfID  = "CONT29"
   ShaperConf.Template      = "HSPA_MONTHLY"
   ShaperConf.TariffType    = "PLANA_HI"
   ShaperConf.Tariff        = "PLANA_I"
   ShaperConf.LimitUnshaped = 104857600 
   ShaperConf.LimitShaped   = 20971520
   ShaperConf.Active        = YES.

FIND FIRST ShaperConf EXCLUSIVE-LOCK WHERE 
           ShaperConf.ShaperConfID EQ "CONT30" NO-ERROR.

IF AVAIL ShaperConf THEN DO:
  ASSIGN
    ShaperConf.Brand         = Syst.Var:gcBrand
    ShaperConf.Template      = "HSPA_MONTHLY"
    ShaperConf.TariffType    = "PLANA_VD01"
    ShaperConf.Tariff        = "PLANA_H_VD01"
    ShaperConf.LimitUnshaped = 148938728407
    ShaperConf.LimitShaped   = 61954903245
    ShaperConf.Active        = YES. 
END.

