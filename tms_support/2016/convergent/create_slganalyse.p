DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.

FUNCTION fcreateSLGAnalyse RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icDCEvent AS CHAR,
                                             INPUT idaVAlidFrom AS DATE,
                                             INPUT icclitype AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   FOR EACH bSLGAnalyse WHERE
            bSLGAnalyse.brand EQ "1" AND
            bSLGAnalyse.clitype EQ icBaseDCEvent AND
            bSLGAnalyse.validto > TODAY.
      CREATE ttSLGAnalyse.
      BUFFER-COPY bSLGAnalyse TO ttSLGAnalyse.
      ttSLGAnalyse.ValidFrom = TODAY. 
      ttSLGAnalyse.clitype = icCliType.
      IF  ttSLGAnalyse.servicelimitgroup EQ icBaseDCEvent THEN
         ttSLGAnalyse.servicelimitgroup = icclitype.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE SLGAnalyse.
         BUFFER-COPY ttSLGAnalyse TO SLGAnalyse.
         DELETE ttSLGAnalyse. /*ror safety reasons*/
      END.
      ELSE DISP ttSLGAnalyse.   
   END.
END.

fcreateSLGAnalyse("CONT24","CONTDSL10",TODAY,"CONTDSL10",0).

