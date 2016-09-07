DEF VAR ldaFrom AS DATE 09/07/16.

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
      ttSLGAnalyse.ValidFrom = ldaFrom. 
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

fcreateSLGAnalyse("CONT24","CONTDSL45",ldaFrom,"CONTDSL45",0).
fcreateSLGAnalyse("CONT24","CONTDSL55",ldaFrom,"CONTDSL55",0).
fcreateSLGAnalyse("CONT24","CONTFH45_50",ldaFrom,"CONTFH45_50",0).
fcreateSLGAnalyse("CONT24","CONTFH55_50",ldaFrom,"CONTFH55_50",0).
fcreateSLGAnalyse("CONT24","CONTFH55_300",ldaFrom,"CONTFH55_300",0).
fcreateSLGAnalyse("CONT24","CONTFH65_300",ldaFrom,"CONTFH65_300",0).

