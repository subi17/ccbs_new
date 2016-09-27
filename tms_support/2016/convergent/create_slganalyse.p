{commpaa.i}
DEF VAR ldaFrom AS DATE INIT 09/07/16.
DEF VAR liMode AS INT INIT 0.
DEF VAR liMode_ra AS INT INIT 0.

DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER bRequestAction FOR RequestAction.
DEF BUFFER bBillItem FOR BillItem.
DEF VAR liActionId AS INT.

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
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "MDUB3" THEN NEXT.
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "MDUB4" THEN NEXT.
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

FUNCTION create_ra returns log(INPUT icBasetype AS CHAR,
                    INPUT icClitype AS CHAR,
                    INPUT iiUpdateMode AS INT):
   
   FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.   
   FOR EACH bRequestAction WHERE
            bRequestAction.clitype EQ icBaseType:
      CREATE ttRequestAction.
      BUFFER-COPY bRequestAction TO ttRequestAction.
      ASSIGN
      ttRequestAction.ValidFrom = ldaFrom
      ttRequestAction.clitype = icCliType
      ttRequestAction.RequestActionID = liActionID.
      IF  ttRequestaction.actionkey EQ icBasetype THEN
         ttRequestaction.actionkey = icclitype.
      IF (icClitype BEGINS "CONTDSL" OR icClitype BEGINS "CONTFH") AND
          bRequestAction.reqtype EQ 13 THEN DO:
         ttRequestAction.Reqtype = 14.
         IF ttRequestaction.actionkey BEGINS "CONTDSL" THEN
            ttRequestaction.actionkey = "CONTDSL".
         ELSE IF ttRequestaction.actionkey BEGINS "CONTFH" THEN DO:
            IF INDEX(ttRequestaction.actionkey, "50") > 0 THEN
               ttRequestaction.actionkey = "CONTFH50".
            ELSE IF INDEX(ttRequestaction.actionkey,"300") > 0 THEN
               ttRequestaction.actionkey = "CONTFH300".
         END.
      END.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE RequestAction.
         BUFFER-COPY ttRequestAction TO RequestAction.
      END.
      ELSE DISP ttRequestAction.
      DELETE ttRequestAction. /*ror safety reasons*/
      liActionID = liActionID + 1.
   END.

END.

FUNCTION create_ra_mob returns log(INPUT icBasetype AS CHAR,
                                   INPUT icClitype AS CHAR,
                                   INPUT ickey AS CHAR,
                                   INPUT iiUpdateMode AS INT):

   FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
      IF AVAILABLE RequestAction THEN
         liActionID = RequestAction.RequestActionID + 1.
      ELSE liActionID = 1.
   FIND FIRST bRequestAction WHERE
              bRequestAction.clitype EQ icBaseType AND
              bRequestAction.reqtype = 13 NO-ERROR.
      CREATE ttRequestAction.
      BUFFER-COPY bRequestAction TO ttRequestAction.
      ASSIGN
      ttRequestAction.ValidFrom = ldaFrom
      ttRequestAction.clitype = icCliType
      ttRequestAction.RequestActionID = liActionID.
      ttRequestaction.actionkey = ickey.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE RequestAction.
         BUFFER-COPY ttRequestAction TO RequestAction.
      END.
      ELSE DISP ttRequestAction.
      DELETE ttRequestAction. /*ror safety reasons*/
      liActionID = liActionID + 1.


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
   Clitype.FixedLineDownload = "20M"
   Clitype.FixedLineUpload = "20M".
END.        

FOR EACH CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype MATCHES "CONTFH*50":
   ASSIGN
   Clitype.fixedlinetype = 2
   Clitype.FixedLineDownload = "50M"
   Clitype.FixedLineUpload = "5M".

END.

FOR EACH CliType WHERE
      Clitype.brand EQ "1" AND
      Clitype.clitype MATCHES "CONTFH*300":
   ASSIGN
   Clitype.fixedlinetype = 2
   Clitype.FixedLineDownload = "300M"
   Clitype.FixedLineUpload = "300M".

END.

   
create_ra("CONT24","CONTDSL45",liMode_ra).
create_ra("CONT24","CONTDSL55",liMode_ra).
create_ra("CONT24","CONTFH45_50",liMode_ra).
create_ra("CONT24","CONTFH55_50",liMode_ra).
create_ra("CONT24","CONTFH55_300",liMode_ra).
create_ra("CONT24","CONTFH65_300",liMode_ra).

create_ra_mob("CONT24","CONTDSL45","CONTS2GB",liMode_ra).
create_ra_mob("CONT24","CONTDSL55","CONTS10GB",liMode_ra).
create_ra_mob("CONT24","CONTFH45_50","CONTS2GB",liMode_ra).
create_ra_mob("CONT24","CONTFH55_50","CONTS10GB",liMode_ra).
create_ra_mob("CONT24","CONTFH55_300","CONTS2GB",liMode_ra).
create_ra_mob("CONT24","CONTFH65_300","CONTS10GB",liMode_ra).

FIND LAST RequestAction USE-INDEX RequestActionID NO-LOCK NO-ERROR.
           IF AVAILABLE RequestAction THEN
              liActionID = RequestAction.RequestActionID + 1.
           ELSE liActionID = 1.

CREATE RequestAction.
ASSIGN
   RequestAction.brand = "1"
   RequestAction.reqtype = 0
   RequestAction.clitype = ""
   RequestAction.action = 11
   RequestAction.actionkey = "FTERM6-100"
   RequestAction.actiontype = "DayCampaign"
   RequestAction.paytype = 1
   RequestAction.requestactionid = liActionID 
   RequestAction.validfrom = 09/07/16
   RequestAction.validto = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "#FEECOMPARE"
   RequestActionRule.paramvalue = "+,ORIGINAL>NEW"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "ReqCParam1"
   RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "ReqCParam2"
   RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "ReqIParam5"
   RequestActionRule.paramvalue = "+,0"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

liActionID = liActionID + 1.

CREATE RequestAction.
ASSIGN
   RequestAction.brand = "1"
   RequestAction.reqtype = 0
   RequestAction.clitype = ""
   RequestAction.action = 1
   RequestAction.actionkey = "FTERM6-100"
   RequestAction.actiontype = "DayCampaign"
   RequestAction.paytype = 1
   RequestAction.requestactionid = liActionID
   RequestAction.validfrom = 09/07/16
   RequestAction.validto = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "ReqCParam1"
   RequestActionRule.exclparamvalue = "CONTDSL*,CONTFH*,CONTSF*,CONTFF"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

CREATE RequestActionRule.
ASSIGN
   RequestActionRule.RequestActionid = liActionID
   RequestActionRule.paramfield = "ReqCParam2"
   RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
   RequestActionRule.fromdate = 09/07/16
   RequestActionRule.todate = 12/31/49.

/* prevent removal */
liActionID = liActionID + 1.

CREATE RequestAction.
ASSIGN
   RequestAction.brand = "1"
   RequestAction.reqtype = 46
   RequestAction.clitype = ""
   RequestAction.action = 5
   RequestAction.actionkey = "FTERM6-100"
   RequestAction.actiontype = "DayCampaign"
   RequestAction.paytype = 1
   RequestAction.requestactionid = liActionID
   RequestAction.validfrom = 09/07/16
   RequestAction.validto = 12/31/49.


Function createBillItem RETURNS LOG ( INPUT icbase AS CHAR,
                                      INPUT icBillcode AS CHAR,
                                      INPUT icBiName AS CHAR,
                                      INPUT iiUpdatemode AS INT):
   IF CAN-FIND(FIRST BillItem WHERE
                     billitem.brand EQ "1" AND
                     billItem.billcode EQ icBillcode) THEN
      MESSAGE "Billitem already exist " + icBillCode VIEW-AS ALERT-BOX.   

   FIND FIRST BillItem WHERE
              billitem.brand EQ "1" AND
              billItem.billcode EQ icBase NO-ERROR.

   IF NOT AVAIL BillItem THEN
      MESSAGE "Bill item termperiod find failed " + icbase VIEW-AS ALERT-BOX.

   ELSE DO:
         CREATE ttBillItem.
         BUFFER-COPY BillItem TO ttBillItem.
         ttBillItem.BillCode = icBillCode.
         ttBillItem.BIName = icBiName.
         IF iiUpdateMode NE 0 THEN DO:
            CREATE BillItem.
            BUFFER-COPY ttBillItem TO BillItem.
            DELETE ttBillItem. /*ror safety reasons*/
         END.
         ELSE DISP ttBillItem.   

   END.

END.

createBillItem("TERMPERIOD", "FTERMPERIOD", "Convergent permanency", 0).
createBillItem("DISCPAYTERMDIR", "DISCFTERMPERIOD", "Convergent permanency discount", 0).
createBillItem("DISCPAYTERMDIR", "DISCFHDSL", "Convergent fixed line quota discount", 0).


