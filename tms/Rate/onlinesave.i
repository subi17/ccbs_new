{error_codes.i}

CREATE ttCall.
ASSIGN
   ttCall.AddBPref            = lcAddPrefix
   ttCall.ErrorCode           = {&CDR_ERROR_NOT_ANALYSED_YET}
   ttCall.CLI                 = lcOrigAddress
   ttCall.gsmbnr              = lcDestAddress
   ttCall.EventType           = lcEvent
   ttCall.EventSubType        = lcEventSubType
   ttCall.imei                = lcIMEI
   ttCall.Imei2               = lcImei2 
   ttCall.mscid               = lcmscid 
   ttCall.rateccn             = liRateccn
   ttcall.spocmt              = liRateccn
   ttCall.Atype               = liAType 
   ttCall.Btype               = liBType
   ttCall.bpref               = lcPrefix
   ttCall.RoutingNumber       = lcRoutingnumber
   ttCall.imsi                = lcimsi
   ttCall.imsi2               = lcImsi2
   ttCall.Servicename         = lcServiceName
   ttCall.SubsType            = lcSubscriptionType
   ttCall.ServiceAddress      = lcServiceAddress  
   ttCall.BillDur             = liDuration
   ttCall.Currency            = lcCurrencyCode    
   ttCall.ccharge             = ldeCharge    
   ttCall.charge              = ldecharge         
   ttCall.Pulses              = liPulses          
   ttCall.PPFlag              = liprepaid        
   ttCall.Forwarding          = liForwarding 
   ttCall.RoamingIND          = liRoaming     
   ttCall.DataIn              = ldeDataIn
   ttCall.DataOut             = ldeDataOut
   ttCall.OrigRecordType      = liPartialInd
   ttCall.CdrId               = lcCdrId
   ttCall.CallIdNum           = lcCallIdNum.
   
