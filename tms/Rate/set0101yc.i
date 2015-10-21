
ASSIGN

 lcCustomerName        =  REPLACE(TRIM(ENTRY( 1,callrec,lcSep)),'"',"")
 liSourceID            = INT(TRIM(ENTRY( 2,callrec,lcSep)))   
 lcmscid               =     TRIM(ENTRY( 3,callrec,lcSep))
 lcRecordType          =     TRIM(ENTRY( 4,callrec,lcSep))
 lcVersion             =     TRIM(ENTRY( 5,callrec,lcSep))
 lcBatchSeq            = INT(TRIM(ENTRY( 7,callrec,lcSep)))  
 lcEvent               =     TRIM(ENTRY( 9,callrec,lcSep))
 lcEventSubType        =     TRIM(ENTRY(10,callrec,lcSep))  
 lcOriginalCDRtype     =     TRIM(ENTRY(11,callrec,lcSep))
 liRateccn             = INT(TRIM(ENTRY(12,callrec,lcSep)))  
 liNoCharge            = INT(TRIM(ENTRY(13,callrec,lcSep))) 
 lcStartDate           =     TRIM(ENTRY(14,callrec,lcSep))
 lcStartTime           =     TRIM(ENTRY(15,callrec,lcSep))
 lcutc                 =     TRIM(ENTRY(16,callrec,lcSep))
 liDuration            = INT(TRIM(ENTRY(17,callrec,lcSep))).
 liChargedParty        = INT(TRIM(ENTRY(18,callrec,lcSep))).
 lcSubscriptionType    =     TRIM(ENTRY(19,callrec,lcSep)).
 lcSubsInfo            =     TRIM(ENTRY(20,callrec,lcSep)).
 lcOrigAddress         =     TRIM(ENTRY(26,callrec,lcSep)).
 
 liAtype               = INT(TRIM(ENTRY(21,callrec,lcSep))).
 lcIMSI                =     TRIM(ENTRY(22,callrec,lcSep)).
 lcDestAddress         =     TRIM(ENTRY(23,callrec,lcSep)) .
 liBtype               = INT(TRIM(ENTRY(24,callrec,lcSep))).
 lcIMSI2               =     TRIM(ENTRY(25,callrec,lcSep)).
 lcSubsInfo            =     TRIM(ENTRY(20,callrec,lcSep)).
 lcChargedsubs         =  TRIM(ENTRY(26,callrec,lcSep)).

 lcServiceName         =      TRIM(ENTRY(55,callrec,lcSep)).
 ldeCharge             =  DEC(TRIM(ENTRY(58,callrec,lcSep))).

IF lcSubscriptionType = "2" then liPrePaid = 1.
ELSE                             liPrePaid = 0.
                          
