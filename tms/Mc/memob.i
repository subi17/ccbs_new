DEFINE temp-table MemoType
FIELD MemoType  AS CHAR
FIELD MemoTable AS CHAR 
INDEX MemoType IS PRIMARY UNIQUE MemOtype MemoTable.

CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "MOBSUB"
   MemoType.MemoTable = "MOBSUB".

CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "INVOICE"
   MemoType.MemoTable = "INVOICE".

CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "INVOICE"
   MemoType.MemoTable = "PAYMENT".
   
CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "CREDITC"
   MemoType.MemoTable = "HIGHUSAGE".   
   
CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "CREDITC"
   MemoType.MemoTable = "Contact".   
   
   
CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "CUSTOMER"
   MemoType.MemoTable = "CUSTOMER".   

CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "CUSTOMER"
   MemoType.MemoTable = "fixedfee".   
   
CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "CUSTOMER"
   MemoType.MemoTable = "SINGLEFEE".   
   
   
   
CREATE MemoType.
ASSIGN
   MemoType.MemoType  = "*"
   MemoType.MemoTable = "*".   
   
   
   





