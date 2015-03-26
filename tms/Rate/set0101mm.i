
ASSIGN

 lcCustomerName        =  REPLACE(TRIM(ENTRY( 1,callrec,lcSep)),'"',"")
 liSourceID            = INT(TRIM(ENTRY( 2,callrec,lcSep)))   
 lcmscid               =     TRIM(ENTRY( 3,callrec,lcSep))
 lcRecordType          =     TRIM(ENTRY( 4,callrec,lcSep))
 lcVersion             =     TRIM(ENTRY( 5,callrec,lcSep))
 lcBatchSeq            = INT(TRIM(ENTRY( 7,callrec,lcSep)))
 lcMediatorTime        = ""
 lcEvent               =     TRIM(ENTRY( 8,callrec,lcSep))
 lcEventSubType        =     TRIM(ENTRY( 9,callrec,lcSep))  
 lcOriginalCDRtype     =     TRIM(ENTRY(10,callrec,lcSep))
 liRateccn             = INT(TRIM(ENTRY(11,callrec,lcSep)))  
 liNoCharge            = INT(TRIM(ENTRY(12,callrec,lcSep))) 
 lcStartDate           =     TRIM(ENTRY(13,callrec,lcSep))
 lcStartTime           =     TRIM(ENTRY(14,callrec,lcSep))
 lcutc                 =     TRIM(ENTRY(15,callrec,lcSep))
 liDuration            = INT(TRIM(ENTRY(16,callrec,lcSep))).
 liChargedParty        = INT(TRIM(ENTRY(17,callrec,lcSep))).
 lcSubscriptionType    =     TRIM(ENTRY(18,callrec,lcSep)).
 lcOrigAddress         =     TRIM(ENTRY(19,callrec,lcSep)).
 liAtype               = INT(TRIM(ENTRY(20,callrec,lcSep))).
 lcIMSI                =     TRIM(ENTRY(21,callrec,lcSep)).
 lcIMEI                =     TRIM(ENTRY(22,callrec,lcSep)).
 lcDestAddress         =     TRIM(ENTRY(23,callrec,lcSep)) .
 liBtype               = INT(TRIM(ENTRY(24,callrec,lcSep))).
 lcIMSI2               =     TRIM(ENTRY(25,callrec,lcSep)).
 lcIMEI2               =     TRIM(ENTRY(26,callrec,lcSep)).
 lcPrefixCode          =     TRIM(ENTRY(27,callrec,lcSep)).
 lcPrefix              =     TRIM(ENTRY(28,callrec,lcSep)).
 lcAddPrefix           =     TRIM(ENTRY(29,callrec,lcSep)).
 lcRoutingnumber       =   TRIM(ENTRY(46,callrec,lcSep)).
 liPrePaid             = INT(ENTRY(66,callrec,lcSep)).
 lcChargedsubs         =     TRIM(ENTRY(35,callrec,lcSep)).

 ldeDataIn             = DEC(TRIM(ENTRY(62,callrec,lcSep))).
 ldeDataOut            = DEC(TRIM(ENTRY(63,callrec,lcSep))).
 ldeCharge             =  DEC(TRIM(ENTRY(71,callrec,lcSep))).
 liRoaming             = INT(TRIM(ENTRY(67,callrec,lcSep))).
 liPartialInd          = 0.
 lcCdrId               = "".
 lcCallIdNum           = "".

