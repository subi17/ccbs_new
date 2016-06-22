FOR EACH MobSub NO-LOCK WHERE 
         MobSub.Brand          = "1" AND
         MobSub.PayType        = FALSE AND
         MobSub.ActivationDate < 04/22/14:
  IF CAN-FIND (FIRST DCCLI WHERE 
                     DCCLI.MsSeq = MobSub.MsSeq AND
                     DCCLI.DCEvent BEGINS "PAYTERM")
                  /* DCCLI.ValidTo >= TODAY) */ THEN NEXT.
  IF CAN-FIND (FIRST FixedFee WHERE 
                     FixedFee.KeyValue = STRING(MobSub.MsSeq) AND
                     FixedFee.Active = TRUE) THEN NEXT.

  DISP MobSub.Cli.

END.
