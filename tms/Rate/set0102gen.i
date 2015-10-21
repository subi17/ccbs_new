
ASSIGN
 lcCustomerName        =     TRIM(ENTRY( 1,callrec,lcSep))
 liSourceID            = INT(TRIM(ENTRY( 2,callrec,lcSep)))   
 lcmscid               =     TRIM(ENTRY( 3,callrec,lcSep))
 lcRecordType          =     TRIM(ENTRY( 4,callrec,lcSep))
 lcVersion             =     TRIM(ENTRY( 5,callrec,lcSep))
 lcBatchSeq            = INT(TRIM(ENTRY( 8,callrec,lcSep)))  
 lcEvent               =     TRIM(ENTRY( 9,callrec,lcSep))
 lcEventSubType        =     TRIM(ENTRY( 10,callrec,lcSep))  
 lcOriginalCDRtype     =     TRIM(ENTRY(11,callrec,lcSep))
 liOrigCallCategory    = INT(TRIM(ENTRY(12,callrec,lcSep)))
 liRateccn             = INT(TRIM(ENTRY(13,callrec,lcSep)))  
 liNoCharge            = INT(TRIM(ENTRY(14,callrec,lcSep))) 
 liErrorCode           = INT(TRIM(ENTRY(15,callrec,lcSep)))
 lcNetworkowner        =     TRIM(ENTRY(16,callrec,lcSep))   
 lcStartDate           =     TRIM(ENTRY(17,callrec,lcSep))
 lcStartTime           =     TRIM(ENTRY(18,callrec,lcSep))
 lcutc                 =     TRIM(ENTRY(19,callrec,lcSep))
 liChargedParty        = INT(TRIM(ENTRY(20,callrec,lcSep)))
 lcOrigAddress         =     TRIM(ENTRY(21,callrec,lcSep))
 lcDestAddress         =     TRIM(ENTRY(22,callrec,lcSep)) 



 liAtype               = INT(TRIM(ENTRY(25,callrec,lcSep))).
 liBtype               = INT(TRIM(ENTRY(26,callrec,lcSep))).
 lcPrefixCode          =     TRIM(ENTRY(27,callrec,lcSep)).
 lcPrefix              =     TRIM(ENTRY(28,callrec,lcSep)).
 lcAddPrefix           =     TRIM(ENTRY(29,callrec,lcSep)).
 lcRoutingnumber       = IF   TRIM(ENTRY(30,callrec,lcSep)) = "0" THEN ""
                         ELSE TRIM(ENTRY(30,callrec,lcSep))   .
 
 lcIMEI                =     TRIM(ENTRY(31,callrec,lcSep)).
 lcIMSI                =     TRIM(ENTRY(32,callrec,lcSep)).
 lcIMSI2               =     TRIM(ENTRY(34,callrec,lcSep)).         
 lcChargedsubs         =     TRIM(ENTRY(35,callrec,lcSep)).
 lcServiceName         =     TRIM(ENTRY(39,callrec,lcSep)).
 lcServiceAddress      =     TRIM(ENTRY(40,callrec,lcSep)).
 lcDestURL             =     TRIM(ENTRY(41,callrec,lcSep)).
 lcServiceType         =     TRIM(ENTRY(42,callrec,lcSep)).
 lcBSCode              =     TRIM(ENTRY(43,callrec,lcSep)).
 lcSSI                 =     TRIM(ENTRY(44,callrec,lcSep)).
 liMSMark              =  INT(TRIM(ENTRY(46,callrec,lcSep))).
 lcInTrunkName         =      TRIM(ENTRY(47,callrec,lcSep)).
 lcOutTrunkName        =      TRIM(ENTRY(48,callrec,lcSep)).
 liDuration            = INT(TRIM(ENTRY(49,callrec,lcSep))).
 ldeDataIn             = DEC(TRIM(ENTRY(50,callrec,lcSep))).
 ldeDataOut            = DEC(TRIM(ENTRY(51,callrec,lcSep))).
 ldeMessageLength      = DEC(TRIM(ENTRY(52,callrec,lcSep))).
 lcContentType         =     TRIM(ENTRY(53,callrec,lcSep)).
 lcTariffClass         =     TRIM(ENTRY(54,callrec,lcSep)).
 lcServiceDescription  =     TRIM(ENTRY(55,callrec,lcSep)).
 lcServiceGroup        = TRIM(ENTRY(56,callrec,lcSep)).
 lcCurrencyCode        =      TRIM(ENTRY(57,callrec,lcSep)).
 ldeCharge             =  DEC(TRIM(ENTRY(58,callrec,lcSep))).
 ldeTax                =  DEC(TRIM(ENTRY(59,callrec,lcSep))).
 liTaxCode             =  STRING(TRIM(ENTRY(60,callrec,lcSep))).
 liPulses              =  INT(TRIM(ENTRY(61,callrec,lcSep))).
 liCauseForTermination = INT(TRIM(ENTRY(62,callrec,lcSep))).
 liTestIndicator       = INT(TRIM(ENTRY(63,callrec,lcSep))). 
 liRoaming             = INT(TRIM(ENTRY(64,callrec,lcSep))).
 liPartialInd          = INT(TRIM(ENTRY(65,callrec,lcSep))).
 liPrePaid             = INT(TRIM(ENTRY(67,callrec,lcSep))).
 lcNCR                 =     TRIM(ENTRY(68,callrec,lcSep)).
 
