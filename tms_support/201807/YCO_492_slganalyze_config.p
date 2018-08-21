DEFINE VARIABLE lcCLITypes AS CHARACTER NO-UNDO.

lcCLITypes = "CONTDSL48,CONTFH48_50,CONTFH58_300,CONTFH76_1000".

DEFINE BUFFER bSLGAnalyse   FOR SLGAnalyse.
DEFINE BUFFER bufSLGAnalyse FOR SLGAnalyse.

DEFINE VARIABLE liCount AS INTEGER NO-UNDO.

FOR EACH bSLGAnalyse NO-LOCK WHERE
         bSLGAnalyse.Brand             EQ "1"    AND
         bSLGAnalyse.ServiceLimitGroup EQ "DSS2" AND
         bSLGAnalyse.CLIType           EQ "CONTFH7G_50":

   DO liCount = 1 TO NUM-ENTRIES(lcCLITypes):

      IF NOT CAN-FIND(FIRST bufSLGAnalyse NO-LOCK WHERE
                            bufSLGAnalyse.Brand             EQ "1"                       AND
                            bufSLGAnalyse.BillCode          EQ bSLGAnalyse.BillCode      AND
                            bufSLGAnalyse.CLIType           EQ ENTRY(liCount,lcCLITypes) AND
                            bufSLGAnalyse.ServiceLimitGroup EQ bSLGAnalyse.ServiceLimitGroup) THEN DO:

         CREATE SLGAnalyse.
         BUFFER-COPY bSLGAnalyse EXCEPT CLIType ValidFrom TO SLGAnalyse.
         ASSIGN SLGAnalyse.CliType   = ENTRY(liCount,lcCLITypes)
                SLGAnalyse.ValidFrom = TODAY + 1.

      END. 

   END.

END.
