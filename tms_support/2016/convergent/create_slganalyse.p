{commpaa.i}
DEF VAR ldaFrom AS DATE INIT 09/07/16.
DEF VAR liMode AS INT INIT 0.
DEF VAR liMode_ra AS INT INIT 0.
DEF VAR liModeBI AS INT INIT 0.
DEF VAR liModeCliType AS INT INIT 0.
DEF VAR liModeCCN AS INT INIT 0.
DEF VAR liModeTariff AS INT INIT 1.

DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER bRequestAction FOR RequestAction.
DEF BUFFER bBillItem FOR BillItem.
DEF BUFFER bTariff FOR Tariff.
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
/*fcreateSLGAnalyse("CONT24","CONTDSL55",ldaFrom,"CONTDSL55",liMode).*/
fcreateSLGAnalyse("CONT24","CONTFH45_50",ldaFrom,"CONTFH45_50",liMode).
/*fcreateSLGAnalyse("CONT24","CONTFH55_50",ldaFrom,"CONTFH55_50",liMode).*/
fcreateSLGAnalyse("CONT24","CONTFH55_300",ldaFrom,"CONTFH55_300",liMode).
/*fcreateSLGAnalyse("CONT24","CONTFH65_300",ldaFrom,"CONTFH65_300",liMode).*/

IF liModeCliType > 0 THEN DO:
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
END.
   
create_ra("CONT24","CONTDSL45",liMode_ra).
/*create_ra("CONT24","CONTDSL55",liMode_ra).*/
create_ra("CONT24","CONTFH45_50",liMode_ra).
/*create_ra("CONT24","CONTFH55_50",liMode_ra).*/
create_ra("CONT24","CONTFH55_300",liMode_ra).
/*create_ra("CONT24","CONTFH65_300",liMode_ra).*/

create_ra_mob("CONT24","CONTDSL45","CONTS2GB",liMode_ra).
/*create_ra_mob("CONT24","CONTDSL55","CONTS10GB",liMode_ra).*/
create_ra_mob("CONT24","CONTFH45_50","CONTS2GB",liMode_ra).
/*create_ra_mob("CONT24","CONTFH55_50","CONTS10GB",liMode_ra).*/
create_ra_mob("CONT24","CONTFH55_300","CONTS2GB",liMode_ra).
/*create_ra_mob("CONT24","CONTFH65_300","CONTS10GB",liMode_ra).*/

IF liMode_ra > 0 THEN DO:
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
      RequestAction.actionkey = "FTERM12-100"
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
      RequestAction.actionkey = "FTERM12-100"
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
      RequestAction.actionkey = "FTERM12-100"
      RequestAction.actiontype = "DayCampaign"
      RequestAction.paytype = 1
      RequestAction.requestactionid = liActionID
      RequestAction.validfrom = 09/07/16
      RequestAction.validto = 12/31/49.
END.

Function createBillItem RETURNS LOG ( INPUT icbase AS CHAR,
                                      INPUT icBillcode AS CHAR,
                                      INPUT icBiName AS CHAR,
                                      INPUT iiAccount AS INT,
                                      INPUT icGroup AS CHAR,
                                      INPUT icSection AS CHAR,
                                      INPUT icTaxClass AS CHAR,
                                      INPUT icSAPRid AS CHAR,
                                      INPUT ilDispMPM AS LOG,
                                      INPUT icCostCentre AS CHAR,
                                      INPUT iiUpdatemode AS INT):
   IF CAN-FIND(FIRST BillItem WHERE
                     billitem.brand EQ "1" AND
                     billItem.billcode EQ icBillcode) THEN DO:
      MESSAGE "Billitem already exist " + icBillCode VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   FIND FIRST BillItem WHERE
              billitem.brand EQ "1" AND
              billItem.billcode EQ icBase NO-ERROR.

   IF NOT AVAIL BillItem THEN
      MESSAGE "Bill item termperiod find failed " + icbase VIEW-AS ALERT-BOX.

   ELSE DO:
         CREATE ttBillItem.
         BUFFER-COPY BillItem TO ttBillItem.
         ASSIGN
            ttBillItem.BillCode = icBillCode
            ttBillItem.BIName = icBiName
            ttBillItem.accnum = iiAccount
            ttBillItem.altaccnum = iiAccount
            ttBillItem.euaccnum = iiAccount
            ttBillItem.euconaccnum = iiAccount
            ttBillItem.FSaccnum = iiAccount
            ttBillItem.vipaccnum = iiAccount
            ttBillItem.InvSect = icSection
            ttBillItem.taxclass = ictaxclass
            ttBillItem.saprid = icsaprid
            ttBillItem.dispMPM = ildispmpm
            ttBillItem.bigroup = icgroup
            ttBillItem.costcentre = icCostCentre.

         IF iiUpdateMode NE 0 THEN DO:
            CREATE BillItem.
            BUFFER-COPY ttBillItem TO BillItem.
            DELETE ttBillItem. /*ror safety reasons*/
         END.
         ELSE DISP ttBillItem.   

   END.

END.

createBillItem("TERMPERIOD", "FTERMPERIOD", "Convergent permanency", 
               70518111,"33","","0","046",FALSE,"SL",liModeBI).
createBillItem("DISCPAYTERMDIR", "DISCFTERMPERIOD", "Convergent permanency discount", 70020105,"13","","0","046",FALSE,"SL",liModeBI).
createBillItem("DISCPAYTERMDIR", "DISCFHDSL", "Convergent fixed line quota discount", 70020105,"13","","0","046",FALSE,"SL",liModeBI).

createBillItem("FLATVOICE", "F10100003", "Fixed to Spanish mobile", 
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "F10100004", "Fixed to international mobile",
               70510100,"51","3","1","004",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "F10100005", "Fixed to Spanish fixed",
               70510100,"51","1","1","005",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "F10100006", "Fixed to international fixed",
               70510100,"51","3","1","006",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "F15100023", "Fixed to VAS Premium Voice Service",
               70515100,"53","","1","",FALSE,"",liModeBI).
createBillItem("FLATVOICE", "F15100027", "Fixed to VAS Freephone Service",
               70515100,"51","","1","",FALSE,"",liModeBI).
createBillItem("FLATVOICE", "FINFSERVICE", "Fixed to Information Services",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "FSHORTNU", "Fixed to Voice Own Network to Short Numbers",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("FLATVOICE", "FCCYOIGO", "Fixed to Customer Care",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).

/* Fixed line inside package billing items */
createBillItem("CONT24VOICE_A", "CONTDSL_QTY_IN", 
               "Convergent flat rate national fixed to fixed",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTDSL_MIN_IN",
               "Convergent flat rate national fixed to mobil",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTFH50_QTY_IN",
               "Convergent flat rate national fixed to fixed",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTFH50_MIN_IN",
               "Convergent flat rate national fixed to mobile",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTFH300_QTY_IN",
               "Convergent flat rate national fixed to fixed",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTFH300_MIN_IN",
               "Convergent flat rate national fixed to mobile",
               70510100,"51","1","1","003",FALSE,"SL",liModeBI).

/* Mobile line inside package billing items (La Infinita) */
createBillItem("CONT24VOICE_A", "CONTS2GB_VOICE",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS10GB_VOICE",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS2GB_CF",
               "Flat rate call forwarding",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS10GB_CF",
               "Flat rate call forwarding",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS2GB_DATA_A",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS2GB_DATA_B",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS10GB_DATA_A",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS10GB_DATA_B",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).

/* CCN changes */

FUNCTION createCCN RETURNS LOG (INPUT liCCN AS INT,
                                INPUT lcName AS CHAR,
                                INPUT liMode AS INT).
   IF CAN-FIND(FIRST CCN WHERE
                    CCN.brand EQ "1" AND
                    CCN.ccn EQ liCCN) THEN
   MESSAGE "CCN exist" + STRING(liCCN) VIEW-AS ALERT-BOX.
   ELSE IF liMode > 0 THEN DO:
      CREATE CCN.
      ASSIGN
         CCN.Brand = "1"
         CCN.CCN = liCCN
         CCN.CCNName = lcName. 
   END.
   RETURN TRUE.
END.                                

FUNCTION createTariff RETURNS LOG (INPUT lcBase AS CHAR,
                                   INPUT liCCN AS INT,
                                   INPUT lcpricelist AS CHAR,
                                   INPUT lcBdest AS CHAR,
                                   INPUT liMode AS INT).
   DEF VAR lcTempCCN AS CHAR NO-UNDO.
   DEF VAR lcNotNeeded AS CHAR NO-UNDO.
   lcNotNeeded = "010,012,011,061,062,065,080,085,088,091,092,112,1006,116000,116111,016,025,900116016,900840111".
   IF lcBdest EQ "" AND CAN-FIND(FIRST Tariff WHERE
                                       Tariff.brand EQ "1" AND
                                       Tariff.CCN EQ liCCN AND
                                       Tariff.pricelist EQ lcpricelist) THEN
      MESSAGE "tariff exists " + STRING(liCCN) VIEW-AS ALERT-BOX.
   ELSE IF CAN-FIND(FIRST Tariff WHERE
                          Tariff.brand EQ "1" AND
                          Tariff.CCN EQ liCCN AND
                          Tariff.bdest EQ lcBdest AND
                          Tariff.pricelist EQ lcpricelist) THEN
      MESSAGE "tariff exists " + STRING(liCCN) VIEW-AS ALERT-BOX.
   ELSE IF liMode > 0 THEN DO:
      IF lcBase > "" THEN DO:
         IF STRING(liCCN) BEGINS "10" THEN
            lcTempCCN = SUBSTRING(STRING(liCCN),3).
         ELSE IF STRING(liCCN) BEGINS "16" THEN
           lcTempCCN = SUBSTRING(STRING(liCCN),2).
         ELSE
            lcTempCCN = STRING(liCCN).
         FOR EACH bTariff WHERE 
                  bTariff.brand EQ "1" AND
                  bTariff.ccn EQ INT(lcTempCCN) AND
                  bTariff.pricelist EQ lcBase AND
                  bTariff.validto > TODAY:
             IF LOOKUP(bTariff.bdest,lcNotNeeded) > 1 THEN NEXT.
             CREATE ttTariff.
             BUFFER-COPY bTariff TO ttTariff.
             ASSIGN
                ttTariff.CCN = liCCN
                ttTariff.pricelist = lcPriceList
                ttTariff.validFrom = 10/15/16. 
             CREATE Tariff.
             ttTariff.tariffnum = next-value(Tariff).
             ttTariff.billcode = "F" + ttTariff.billcode.
             BUFFER-COPY ttTariff TO Tariff.
             DELETE ttTariff.
         END.
      END.
      ELSE DO:
         FIND FIRST bTariff WHERE 
                    bTariff.brand EQ "1" AND
                    bTariff.ccn EQ liCCN AND
                    bTariff.validto > TODAY NO-ERROR.
         IF AVAIL bTariff THEN DO:           
            BUFFER-COPY bTariff TO ttTariff.
            CREATE Tariff.            
            ASSIGN
               ttTariff.Brand = "1"
               ttTariff.CCN = liCCN
               ttTariff.pricelist = lcpricelist
               ttTariff.bdest = lcBDest
               ttTariff.billcode = "F" + ttTariff.billcode
               ttTariff.tariffnum = next-value(Tariff).
            BUFFER-COPY ttTariff TO Tariff.
            DELETE ttTariff.
         END.
      END.
   END.
   RETURN TRUE.

END.

createCCN(1002, "Fixed to International Call", limodeCCN).
createCCN(1081, "Fixed to National", limodeCCN).
createCCN(1008, "Fixed to Freephone Voice", limodeCCN).
createCCN(1061, "Service numbers 60-30-30", limodeCCN).
createCCN(1069, "Service numbers 60-30-30", limodeCCN).
createCCN(1063, "micropagos numbers -SPECSERV", limodeCCN).
createCCN(1631, "micropagos numbers -SPECSERV", limodeCCN).
createCCN(1064, "Service numbers 20-30", limodeCCN).
createCCN(1066, "Fixed to special short numbers", limodeCCN).
createCCN(1067, "Fixed to special short numbers", limodeCCN).   


createTariff("COMMON",1002,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1008,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1061,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1063,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1064,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1066,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1067,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1069,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1081,"CONTRATOFIXED","",liModeTariff).
createTariff("COMMON",1631,"CONTRATOFIXED","",liModeTariff).

createTariff("",1008,"CONTRATOFIXED","70",liModeTariff).
createTariff("",1008,"CONTRATOFIXED","082",liModeTariff).
createTariff("",1008,"CONTRATOFIXED","083",liModeTariff).
createTariff("",1008,"CONTRATOFIXED","085",liModeTariff).
createTariff("",1008,"CONTRATOFIXED","088",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTDSL_QTY_IN",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTDSL_MIN_IN",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTFH50_QTY_IN",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTFH50_MIN_IN",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTFH300_QTY_IN",liModeTariff).
createTariff("",1081,"CONTRATOFIXED","CONTFH300_MIN_IN",liModeTariff).


/* permanency for 12 months again */
FIND FIRST Daycampaign WHERE
           DayCampaign.dcevent EQ "FTERM6-100" NO-ERROR.
IF AVAIL Daycampaign THEN
   DAycampaign.validto = 10/20/16.
   /* in staging there are already subscriptions with old one 
      ended here */

FIND FIRST Daycampaign WHERE
           DayCampaign.dcevent EQ "FTERM12-100" NO-ERROR.
IF NOT AVAIL Daycampaign THEN DO:
   CREATE Daycampaign.
   ASSIGN
      Daycampaign.brand = "1"
      DayCampaign.dcevent = "FTERM12-100"
      DayCampaign.DCName = "FTERM12 periodical contract"
      DayCampaign.statuscode = 1
      DayCampaign.dctype = "3"
      DayCampaign.instancelimit = 1
      DayCampaign.calcmethod = 1
      DayCampaign.effective = 1
      DayCampaign.durtype = 3
      DayCampaign.durmonths = 12
      DayCampaign.durunit = 2
      Daycampaign.termfeemodel = "FTERMPERIOD"
      Daycampaign.TermFeeCalc = 2
      DayCampaign.validfrom = 10/21/16
      DayCampaign.validto = 12/31/49.

END.
