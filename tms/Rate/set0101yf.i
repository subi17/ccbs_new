ASSIGN
 lcCustomerName        =  REPLACE(TRIM(ENTRY( 1,callrec,lcSep)),'"',"")
 liSourceID            = INT(TRIM(ENTRY( 2,callrec,lcSep)))   
 lcSourceName          =     TRIM(ENTRY( 3,callrec,lcSep))
 lcRecordType          =     TRIM(ENTRY( 4,callrec,lcSep))
 lcVersion             =     TRIM(ENTRY( 5,callrec,lcSep))
 lcBatchSeq            = INT(TRIM(ENTRY( 7,callrec,lcSep)))  
 liRunningindex        = INT(TRIM(ENTRY( 8,callrec,lcSep)))
 lcExtRunningIndex     =     TRIM(ENTRY( 9,callrec,lcSep))  
 lcHandlingTime        =     TRIM(ENTRY(10,callrec,lcSep))
 lcEvent               =     TRIM(ENTRY(11,callrec,lcSep))  
 lcEventSubType        =     TRIM(ENTRY(12,callrec,lcSep)) 
 lcOriginalCdrType     =     TRIM(ENTRY(13,callrec,lcSep))
 liRateCCN             = INT(TRIM(ENTRY(14,callrec,lcSep)))
 liNoCharge            = INT(TRIM(ENTRY(15,callrec,lcSep)))
 lcStartDate           =     TRIM(ENTRY(16,callrec,lcSep))
 lcStartTime           =     TRIM(ENTRY(17,callrec,lcSep))
 lcUTC                 =     TRIM(ENTRY(18,callrec,lcSep))
 liDuration            = INT(TRIM(ENTRY(19,callrec,lcSep)))
 liChargedParty        = INT(TRIM(ENTRY(20,callrec,lcSep)))
 lcSubscriptionType    =     TRIM(ENTRY(21,callrec,lcSep))
 lcOrigAddress         =     TRIM(ENTRY(22,callrec,lcSep))
 liOrigAddressType     = INT(TRIM(ENTRY(23,callrec,lcSep)))
 lcDestAddress         =     TRIM(ENTRY(24,callrec,lcSep))
 liDestAddressType     = INT(TRIM(ENTRY(25,callrec,lcSep)))
 lcCallIdNum           =     TRIM(ENTRY(26,callrec,lcSep)).
