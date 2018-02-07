DEFINE VARIABLE llActive      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcCLITypeList AS CHARACTER NO-UNDO.

ASSIGN
   llActive      = YES /* if active = NO then inactivate web and stc for the tariff
                         otherwise activate */ 
   lcCLITypeList = "CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000".
   

DEFINE VARIABLE lii AS INTEGER NO-UNDO.

DO lii = 1 TO NUM-ENTRIES(lcCLITypeList):
   
   FOR CLIType EXCLUSIVE-LOCK WHERE
       CLIType.Brand = "1" AND
       CLIType.CLIType = ENTRY(lii, lcCLITypeList):
      
      
      IF NOT llActive
      THEN ASSIGN
              CLIType.WebStatusCode     = 3
              CLIType.StatusCode        = 0
              .
      ELSE ASSIGN
              CLIType.WebStatusCode     = 1
              CLIType.StatusCode        = 1
              .
          
   END. 
END.
