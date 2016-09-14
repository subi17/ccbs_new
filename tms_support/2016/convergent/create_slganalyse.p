DEF VAR ldaFrom AS DATE INIT 09/07/16.
DEF VAR liMode AS INT INIT 0.
DEF VAR liMode_ra AS INT INIT 1.

DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER bRequestAction FOR RequestAction.

FUNCTION fcreateSLGAnalyse RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icDCEvent AS CHAR,
                                             INPUT idaVAlidFrom AS DATE,
                                             INPUT icclitype AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   FOR EACH bSLGAnalyse WHERE
            bSLGAnalyse.brand EQ "1" AND
            bSLGAnalyse.clitype EQ icBaseDCEvent AND
            bSLGAnalyse.validto > TODAY.
      
      IF bSLGAnalyse.servicelimitgroup BEGINS "DSS" THEN NEXT.
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "DATA7" THEN NEXT.


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

fcreateSLGAnalyse("CONT24","CONTDSL45",ldaFrom,"CONTDSL45",liMode).
fcreateSLGAnalyse("CONT24","CONTDSL55",ldaFrom,"CONTDSL55",liMode).
fcreateSLGAnalyse("CONT24","CONTFH45_50",ldaFrom,"CONTFH45_50",liMode).
fcreateSLGAnalyse("CONT24","CONTFH55_50",ldaFrom,"CONTFH55_50",liMode).
fcreateSLGAnalyse("CONT24","CONTFH55_300",ldaFrom,"CONTFH55_300",liMode).
fcreateSLGAnalyse("CONT24","CONTFH65_300",ldaFrom,"CONTFH65_300",liMode).

FOR EACH CliType WHERE 
         Clitype.brand EQ "1" AND
         Clitype.clitype BEGINS "CONTDSL":
   ASSIGN
   Clitype.fixedlinetype = 1
   Clitype.FixedLineDownload = "20"
   Clitype.FixedLineUpload = "20".
END.        

FOR EACH CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype MATCHES "CONTFH*50":
   ASSIGN
   Clitype.fixedlinetype = 2
   Clitype.FixedLineDownload = "50"
   Clitype.FixedLineUpload = "5".

END.

FOR EACH CliType WHERE
      Clitype.brand EQ "1" AND
      Clitype.clitype MATCHES "CONTFH*300":
   ASSIGN
   Clitype.fixedlinetype = 2
   Clitype.FixedLineDownload = "300"
   Clitype.FixedLineUpload = "300".

END.

FUNCTION create_ra returns log(INPUT icBasetype AS CHAR,
                    INPUT icClitype AS CHAR,
                    INPUT iiUpdateMode AS INT):
   DEF VAR liActionId AS INT.
   FOR EACH bRequestAction WHERE 
            bRequestAction.clitype EQ icBaseType:
      FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
           IF AVAILABLE RequestAction THEN
              liActionID = RequestAction.RequestActionID + 1.
           ELSE liActionID = 1.
      CREATE ttRequestAction.
      BUFFER-COPY bRequestAction TO ttRequestAction.
      ASSIGN
      ttRequestAction.ValidFrom = ldaFrom
      ttRequestAction.clitype = icCliType
      ttRequestAction.RequestActionID = liActionID.
      IF  ttRequestaction.actionkey EQ icBasetype THEN
         ttRequestaction.actionkey = icclitype.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE RequestAction.
         BUFFER-COPY ttRequestAction TO RequestAction.
         DELETE ttRequestAction. /*ror safety reasons*/
      END.
      ELSE DISP ttRequestAction.

   END.

END. 
   
create_ra("CONT24","CONTDSL45",liMode_ra).
create_ra("CONT24","CONTDSL55",liMode_ra).
create_ra("CONT24","CONTFH45_50",liMode_ra).
create_ra("CONT24","CONTFH55_50",liMode_ra).
create_ra("CONT24","CONTFH55_300",liMode_ra).
create_ra("CONT24","CONTFH65_300",liMode_ra).

 
