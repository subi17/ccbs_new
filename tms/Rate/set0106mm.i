
ASSIGN

 lcCustomerName        =  REPLACE(TRIM(ENTRY( 1,callrec,lcSep)),'"',"")
 liSourceID            = INT(TRIM(ENTRY( 2,callrec,lcSep)))   
 lcMscId               =     TRIM(ENTRY( 3,callrec,lcSep))
 lcRecordType          =     TRIM(ENTRY( 4,callrec,lcSep))
 lcVersion             =     TRIM(ENTRY( 5,callrec,lcSep))
 lcBatchSeq            = INT(TRIM(ENTRY( 7,callrec,lcSep)))  
 lcMediatorTime        =     TRIM(ENTRY(10,callrec,lcSep))
 lcEvent               =     TRIM(ENTRY(11,callrec,lcSep))
 lcEventSubType        =     TRIM(ENTRY(12,callrec,lcSep))  
 lcOriginalCDRtype     =     TRIM(ENTRY(13,callrec,lcSep))
 liRateccn             = INT(TRIM(ENTRY(14,callrec,lcSep)))  
 liNoCharge            = INT(TRIM(ENTRY(15,callrec,lcSep))) 
 lcStartDate           =     TRIM(ENTRY(16,callrec,lcSep))
 lcStartTime           =     TRIM(ENTRY(17,callrec,lcSep))
 lcUtc                 =     TRIM(ENTRY(18,callrec,lcSep))
 liDuration            = INT(TRIM(ENTRY(19,callrec,lcSep)))
 liChargedParty        = INT(TRIM(ENTRY(20,callrec,lcSep)))
 lcSubscriptionType    =     TRIM(ENTRY(21,callrec,lcSep))
 lcOrigAddress         =     TRIM(ENTRY(22,callrec,lcSep))
 liAtype               = INT(TRIM(ENTRY(23,callrec,lcSep)))
 lcIMSI                =     TRIM(ENTRY(24,callrec,lcSep))
 lcIMEI                =     TRIM(ENTRY(25,callrec,lcSep))
 lcDestAddress         =     TRIM(ENTRY(26,callrec,lcSep)) 
 liBtype               = INT(TRIM(ENTRY(27,callrec,lcSep)))
 lcIMSI2               =     TRIM(ENTRY(28,callrec,lcSep))
 lcIMEI2               =     TRIM(ENTRY(29,callrec,lcSep))
 lcTranslatedAddress   =     TRIM(ENTRY(30,callrec,lcSep))
 lcCallIdNum           =     TRIM(ENTRY(34,callrec,lcSep))
 liPartialInd          = INT(TRIM(ENTRY(41,callrec,lcSep))) 
 lcForwardIndicator    =     TRIM(ENTRY(46,callrec,lcSep))
 lcRoutingnumber       =     TRIM(ENTRY(47,callrec,lcSep))
 
 lcSGSNAddress         =     TRIM(ENTRY(58,callrec,lcSep))
 ldeDataIn             = DEC(TRIM(ENTRY(65,callrec,lcSep)))
 ldeDataOut            = DEC(TRIM(ENTRY(66,callrec,lcSep)))
 liPrePaid             = INT(TRIM(ENTRY(69,callrec,lcSep)))
 liRoaming             = INT(TRIM(ENTRY(70,callrec,lcSep)))
 lcMSRN                =     TRIM(ENTRY(71,callrec,lcSep))
 ldeCharge             = DEC(TRIM(ENTRY(77,callrec,lcSep)))
 liAccumulator        = INT(TRIM(ENTRY(78,callrec,lcSep)))
 lcPrefixCode          = ""    
 lcPrefix              = "" 
 lcAddPrefix           = ""
 lcChargedsubs         = ""
 lcCdrId               = "".
