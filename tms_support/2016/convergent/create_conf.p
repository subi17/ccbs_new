{commpaa.i}
DEF VAR ldaFrom AS DATE INIT 10/27/16.
DEF VAR liMode AS INT INIT 1.
DEF VAR liMode_ra AS INT INIT 1.
DEF VAR liModeBI AS INT INIT 1.
DEF VAR liModeCliType AS INT INIT 1.
DEF VAR liModeCCN AS INT INIT 1.
DEF VAR liModeTariff AS INT INIT 1.
DEF VAR liModeDC AS INT INIT 1.

DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEF BUFFER bRequestAction FOR RequestAction.
DEF BUFFER bBillItem FOR BillItem.
DEF BUFFER bTariff FOR Tariff.
DEF VAR liActionId AS INT.

FUNCTION create_bdest RETURNS CHAR (INPUT ictariffcode AS CHAR):
   DEF VAR liBDCount AS INT.
   DEF VAR lcBDestList AS CHAR INIT "QTY_IN,MIN_IN".
   DEF VAR lcBDestination AS CHAR.
   DEF VAR liCCN AS INT.
   DEF VAR gcBrand AS CHAR INIT "1".
   DEF VAR liBDLValue AS INT.

   DO liBDCount = 1 TO NUM-ENTRIES(lcBDestList):

      ASSIGN lcBDestination = icTariffCode + "_" +
                              TRIM(ENTRY(liBDCount,lcBDestList,","))
             liCCN          = IF TRIM(ENTRY(liBDCount,lcBDestList,",")) BEGINS "DATA"
                              THEN 93 ELSE 81.

      FIND FIRST BDest WHERE
                 BDest.Brand = gcBrand        AND
                 BDest.BDest = lcBDestination NO-LOCK NO-ERROR.

      IF NOT AVAILABLE BDest THEN DO:

         FIND LAST BDest USE-INDEX
                   BDestID NO-LOCK NO-ERROR.

         IF AVAILABLE BDest THEN
            liBDLValue = BDest.BDestID + 1.
         ELSE liBDLValue = 1.

         CREATE BDest.
         ASSIGN
            BDest.Brand    = gcBrand
            BDest.BDestID  = liBDLValue
            BDest.BDest    = lcBDestination
            BDest.BDName   = icTariffCode + " " +
                             TRIM(ENTRY(liBDCount,lcBDestList,","))
            BDest.DestType = 0
            BDest.CCN      = liCCN
            BDest.Class    = 1
            BDest.FromDate = ldaFrom
            BDest.ToDate   = 12/31/49 NO-ERROR.

         IF ERROR-STATUS:ERROR THEN
            RETURN "Creating BDest".

      END.
   END.
END.


create_bdest("CONTDSL").
create_bdest("CONTFH50").
create_bdest("CONTFH300").

IF CAN-FIND(FIRST bitemgroup WHERE
                  bitemgroup.brand = "1" AND
                  bitemgroup.bigroup = "46") THEN
   MESSAGE "bigroup already found: 46" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE bitemgroup.
   ASSIGN bitemgroup.bigname = "Convergent Monthly fee"
          bitemgroup.brand = "1"
          bitemgroup.bigroup = "46"
          bitemgroup.grouptype = 0
          bitemgroup.invoiceorder = 31.
END.

/* FEEMODEL */

FUNCTION create_feemodel RETURNS log (INPUT lcmodel AS CHAR,
                                      INPUT ligroup AS INT,
                                      INPUT lcname as char,
                                      INPUT ldamt AS DEC,
                                      INPUT lcBilltype AS CHAR,
                                      INPUT lcbrokenrental AS INT,
                                      INPUT lcPriceList AS CHAR,
                                      INPUT llMethod AS LOG,
                                      INPUT liInterval AS INT,
                                      INPUT liBMeth AS INT):

   FIND FIRST feemodel WHERE feemodel.brand EQ "1" AND
                             feemodel.feemodel EQ lcmodel NO-ERROR.
   IF NOT AVAIL Feemodel THEN DO:
      Create FeeModel.
      ASSIGN FeeModel.brand = "1"
             Feemodel.feemodel = lcmodel
             feemodel.feename = lcname
             feemodel.fmgroup = ligroup.
   END.

   FIND FIRST fmitem WHERE fmitem.brand EQ "1" AND
                           fmitem.feemodel EQ lcmodel AND
                           fmitem.pricelist EQ "CONTRATOFIXED" NO-ERROR.
   IF NOT AVAIL fmitem THEN DO:
      CREATE fmitem.
      ASSIGN
         fmitem.amount = ldamt
         fmitem.billcode = lcmodel
         fmitem.billcycle = liBMeth
         fmitem.billmethod = llMethod
         fmitem.billtype = lcBillType
         fmitem.brand = "1"
         fmitem.brokenrental = lcbrokenrental
         fmitem.feemodel = lcmodel
         fmitem.todate = 12/31/49
         fmitem.firstmonthbr = 0
         fmitem.fromdate = ldafrom
         fmitem.pricelist = lcPriceList
         fmitem.interval = liInterval.

   END.
END.

create_feemodel("CONTDSLMF",0,"Convergent ADSL monthly fee",23.97,"MF",1,"CONTRATOFIXED",FALSE,1,2).
create_feemodel("CONTFH50MF",0,"Convergent FIBER 50MB monthly fee",23.97,"MF",1,"CONTRATOFIXED",FALSE,1,2).
create_feemodel("CONTFH300MF",0,"Convergent FIBER 300MB monthly fee",32.23,"MF",1,"CONTRATOFIXED",FALSE,1,2).
create_feemodel("CONTS2GBMF",0,"Convergent mobile monthly fee",15.70,"MF",1,"CONTRATOFIXED",FALSE,1,2).
create_feemodel("FTERMPERIOD",0,"Fixed line contract termination",100.0,"PF",1,"COMMON",TRUE,0,1).



/* SERVICE LIMITS */

DEF VAR laskuri AS INT.

FUNCTION create_group returns log (INPUT lcCode as CHAR, INPUT lcName AS CHAR):
   IF CAN-FIND(FIRST ServiceLimitgroup WHERE
                     Servicelimitgroup.brand = "1" AND
                     servicelimitgroup.groupcode EQ lcCode) THEN
      return false.
   CREATE servicelimitgroup.
   ASSIGN servicelimitgroup.brand = "1"
          servicelimitgroup.groupcode = lcCode
          servicelimitgroup.groupname = lcName
          servicelimitgroup.validfrom = ldafrom
          servicelimitgroup.validto = 12/31/49.
   return true.
END.

FUNCTION create_limit returns log (INPUT lcCode as CHAR,
                                   INPUT lcName AS CHAR,
                                   INPUT lclimit AS CHAR,
                                   INPUT ldAmt AS DEC,
                                   INPUT liDialType AS INT,
                                   INPUT liUnit AS INT):
   DEF VAR lcTempBI AS CHAR.

   IF CAN-FIND(FIRST ServiceLimit WHERE servicelimit.groupcode EQ lccode AND
                                  servicelimit.slname EQ lcName) THEN
      RETURN false.
   find last servicelimit  use-index slseq no-lock no-error.

           if not avail servicelimit then laskuri = 1.
           else laskuri = servicelimit.slseq + 1.

   CREATE servicelimit.
   ASSIGN servicelimit.dialtype = lidialtype
          servicelimit.inclunit = liUnit
          servicelimit.groupcode = lcCode
          servicelimit.slcode = lcCode + lcLimit
          servicelimit.slname = lcName
          servicelimit.validfrom = ldafrom
          servicelimit.validto = 12/31/49
          servicelimit.slseq = laskuri
          servicelimit.inclamt = ldAmt
          servicelimit.firstmonthcalc = 0
          servicelimit.lastmonthcalc = 0
          servicelimit.webdisp = 1.

   CREATE servicelimittarget.
   IF lcLimit EQ "_MIN" THEN lcTempBI = "F10100003".
   ELSE lcTempBI = "F10100005".
   ASSIGN servicelimittarget.ServiceLMember = lcTempBI
          servicelimittarget.insiderate = lcCode + lcLimit + "_IN"
          servicelimittarget.outsiderate = lcCode + lcLimit + "_OUT"
          servicelimittarget.slseq = laskuri.
   return true.
END.

create_group("CONTDSL", "Contrato DSL").
create_group("CONTFH50", "Contrato FH50").
create_group("CONTFH300", "Contrato FH300").

create_limit("CONTDSL", "BDest", "_QTY",120.0, 50, 7).
create_limit("CONTFH50", "BDest", "_QTY",120.0, 50, 7).
create_limit("CONTFH300", "BDest", "_QTY",120.0, 50, 7).

create_limit("CONTDSL", "National calls", "_MIN",60.0, 1, 1).
create_limit("CONTFH50", "National calls", "_MIN",60.0, 1, 1).
create_limit("CONTFH300", "National calls", "_MIN",60.0, 1, 1).


/* MATRIX */
DEF VAR liSeq AS INT.

FUNCTION create_matrix returns int(
   input ickey AS CHAR,
   input icName AS CHAR):

   DEF VAR liPriority AS INT.
   FIND LAST Matrix WHERE
             Matrix.mxkey EQ icKey.

   liPriority = Matrix.prior + 1.

   CREATE Matrix.
   ASSIGN
      Matrix.Brand  = "1"
      Matrix.MXSeq  = NEXT-VALUE(imsi)
      Matrix.mxkey  = icKey
      Matrix.mxname = icName
      Matrix.prior  = liPriority
      Matrix.mxres  = 1.
   RETURN Matrix.MXSeq.
END.

FUNCTION create_mxitem returns int (input icname AS CHAR,
                                    input icValue AS CHAR,
                                    input iiseq AS INT):
   CREATE MXItem.
   ASSIGN
      MXItem.mxseq = iiseq
      MXItem.mxvalue = icValue
      MXItem.mxname = icname.
END.

/* Main */

liSeq = create_matrix("PERCONTR", "Convergent 2GB mobile").
create_mxitem("PerContract","CONTS2GB",liSeq).
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL45",liSeq).
create_mxitem("SubsTypeTo","CONTFH45_50",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTDSL").
create_mxitem("PerContract","CONTDSL",liSeq).
create_mxitem("SubsTypeTo","CONTDSL*",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH50").
create_mxitem("PerContract","CONTFH50",liSeq).
create_mxitem("SubsTypeTo","CONTFH45_50",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent CONTFH300").
create_mxitem("PerContract","CONTFH300",liSeq).
create_mxitem("SubsTypeTo","CONTFH55_300",liSeq).

liSeq = create_matrix("PERCONTR", "Convergent permanency").
create_mxitem("PerContract","FTERM*",liSeq).
create_mxitem("SubsTypeFrom","CONTDSL*",liSeq).
create_mxitem("SubsTypeFrom","CONTFH*",liSeq).

liSeq = create_matrix("Bundle", "Convergent Bundle list").
create_mxitem("PerContract","DATA6",liSeq).
create_mxitem("SubsTypeTo","CONTDSL45,CONTFH45_50,CONTFH55_300",liSeq).


/* CParams and so on */

FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icBaseDCEvent AS CHAR,
                                                 INPUT icDCEvent AS CHAR,
                                                 INPUT iiUpdateMode AS INT):
   IF iiUpdateMode NE 0 THEN DO:
      FOR EACH TMSParam WHERE LOOKUP(icBaseDCEvent,
                                       tmsParam.charval) > 0:
         IF LOOKUP(icDCEvent, tmsParam.charval) > 0 THEN NEXT.
         IF tmsParam.paramcode EQ "DATA_BUNDLE_BASED_CLITYPES" THEN NEXT.

         tmsParam.charval = tmsParam.charval + "," + icDCEvent.
      END.
   END.
   RETURN TRUE.
END FUNCTION.

FIND FIRST TMSParam NO-LOCK WHERE
           TMSParam.Brand EQ "1" AND
           TMSParam.ParamGroup EQ "MasMovil" AND
           TMSParam.ParamCode EQ "RootDir" NO-ERROR.
IF AVAIL TMSParam THEN
   message TMSParam.ParamCode + " already found" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE TMSParam.

   ASSIGN TMSParam.Brand = "1"
          TMSParam.ParamGroup = "MasMovil"
          TMSParam.ParamCode = "RootDir"
          TMSParam.ParamName = "Root directory for MasMovil files"
          TMSParam.CharVal = "/store/riftp/logistics/masmovil/"
          TMSParam.ParamType = "C".

END.

/* This function add new values in the end of existing char cparam value */
FUNCTION faddToExistingCparamChar RETURNS LOGICAL (INPUT icCparamCode AS CHAR,
                                                   INPUT icAddChar AS CHAR):

   FIND FIRST TMSParam EXCLUSIVE-LOCK WHERE
              TMSParam.Brand      EQ gcBrand AND
              TMSParam.ParamCode  EQ icCparamCode NO-ERROR.
   IF NOT AVAIL TMSParam THEN DO:
      MESSAGE icCparamCode + " not found" VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.
   ASSIGN TMSParam.CharVal = TMSParam.CharVal + "," + icAddChar.
   RELEASE TMSParam.
   RETURN TRUE.
END FUNCTION.

faddToExistingCparamChar("NoPostMinCons", "FTERMPERIOD").
faddToExistingCparamChar("NoPostMinCons", "DISCFTERMPERIOD").

faddTMSParam("CONT24", "CONTS2GB", 0).

FIND FIRST TMSParam WHERE TMSParam.ParamCode EQ "DATA_BUNDLE_BASED_CLITYPES"
   NO-ERROR.

IF LOOKUP("CONTDSL45", TMSParam.charval) = 0 THEN
TMSParam.charval = tmsParam.charval + ",CONTDSL45,CONTFH45_50," +
                   "CONTFH55_300".

FIND FIRST DialType WHERE
           DIALType.dialtype EQ 50 NO-ERROR.
IF NOT AVAIL DialType THEN DO:
   CREATE Dialtype.
   ASSIGN DialType.dialtype = 50
          DialType.dtname = "Fixed Voice Bdest".
END.

/* Request actions */
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

IF liModeCliType > 0 THEN DO:
   FOR EACH CliType WHERE
            Clitype.brand EQ "1" AND
            Clitype.clitype BEGINS "CONTDSL":
      ASSIGN
      Clitype.fixedlinetype = 1
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "20M"
      Clitype.FixedLineUpload = "20M".
   END.

   FOR EACH CliType WHERE
            Clitype.brand EQ "1" AND
            Clitype.clitype MATCHES "CONTFH*50":
      ASSIGN
      Clitype.fixedlinetype = 2
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "50M"
      Clitype.FixedLineUpload = "5M".

   END.

   FOR EACH CliType WHERE
         Clitype.brand EQ "1" AND
         Clitype.clitype MATCHES "CONTFH*300":
      ASSIGN
      Clitype.fixedlinetype = 2
      Clitype.webstatuscode = 0
      Clitype.FixedLineDownload = "300M"
      Clitype.FixedLineUpload = "300M".

   END.
END.

create_ra("CONT24","CONTDSL45",liMode_ra).
create_ra("CONT24","CONTFH45_50",liMode_ra).
create_ra("CONT24","CONTFH55_300",liMode_ra).

create_ra_mob("CONT24","CONTDSL45","CONTS2GB",liMode_ra).
create_ra_mob("CONT24","CONTFH45_50","CONTS2GB",liMode_ra).
create_ra_mob("CONT24","CONTFH55_300","CONTS2GB",liMode_ra).

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
      RequestAction.validfrom = ldaFrom
      RequestAction.validto = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "#FEECOMPARE"
      RequestActionRule.paramvalue = "+,ORIGINAL>NEW"
      RequestActionRule.fromdate = ldaFrom
      RequestActionRule.todate = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "ReqCParam1"
      RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
      RequestActionRule.fromdate = ldaFrom
      RequestActionRule.todate = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "ReqCParam2"
      RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
      RequestActionRule.fromdate = ldaFrom
      RequestActionRule.todate = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "ReqIParam5"
      RequestActionRule.paramvalue = "+,0"
      RequestActionRule.fromdate = ldaFrom
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
      RequestAction.validfrom = ldaFrom
      RequestAction.validto = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "ReqCParam1"
      RequestActionRule.exclparamvalue = "CONTDSL*,CONTFH*,CONTSF*,CONTFF"
      RequestActionRule.fromdate = ldaFrom
      RequestActionRule.todate = 12/31/49.

   CREATE RequestActionRule.
   ASSIGN
      RequestActionRule.RequestActionid = liActionID
      RequestActionRule.paramfield = "ReqCParam2"
      RequestActionRule.paramvalue = "+,CONTDSL*,CONTFH*"
      RequestActionRule.fromdate = ldaFrom
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
      RequestAction.validfrom = ldaFrom
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
               70518111,"33","","0","076",FALSE,"SL",liModeBI).
createBillItem("DISCPAYTERMDIR", "DISCFTERMPERIOD", "Convergent permanency discount", 70518111,"13","","0","076",FALSE,"SL",liModeBI).
createBillItem("DISCPAYTERMDIR", "DISCFHDSL", "Convergent fixed line quota discount", 70514100,"13","","1","070",FALSE,"SL",liModeBI).

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
createBillItem("CONT24VOICE_A", "CONTS2GB_CF",
               "Flat rate call forwarding",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS2GB_DATA_A",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_A", "CONTS2GB_DATA_B",
               "Cont S national GPRS",
               70510100,"3","1","1","003",FALSE,"SL",liModeBI).

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
      DayCampaign.validfrom = ldaFrom
      DayCampaign.validto = 12/31/49.

END.

DEF TEMP-TABLE ttDaycampaign NO-UNDO LIKE Daycampaign.

FUNCTION fcreateDaycampaign RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icEvent AS CHAR,
                                             INPUT icname AS CHAR,
                                             INPUT icdctype AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   FIND FIRST Daycampaign WHERE
              Daycampaign.brand EQ "1" AND
              Daycampaign.dcevent EQ icBaseDCEvent NO-ERROR.
      CREATE ttDaycampaign.
      BUFFER-COPY daycampaign TO ttDaycampaign.
      ttDaycampaign.dctype = icDctype.
      ttDaycampaign.dcevent = icevent.
      ttDaycampaign.billcode = icevent + "MF".
      ttDaycampaign.feemodel = icevent + "MF".
      ttDaycampaign.dcname = icName.
      ttDaycampaign.bundleupsell = "".

      IF iiUpdateMode NE 0 THEN DO:
         CREATE Daycampaign.
         BUFFER-COPY ttDaycampaign TO Daycampaign.
         DELETE ttDaycampaign. /*ror safety reasons*/
      END.
      ELSE DISP ttDayCampaign.

END.

fcreateDaycampaign("CONTS2GB","CONTDSL","La Combinada 20","1",limodedc).
fcreateDaycampaign("CONTS10GB","CONTFH50","La Combinada 50","1",limodedc).
fcreateDaycampaign("CONTS2GB","CONTFH300","La Combinada 300","1",limodedc).
