{commpaa.i}
DEF VAR ldaFrom AS DATE INIT 11/01/16.
DEF VAR liMode AS INT INIT 1.
DEF VAR liMode_ra AS INT INIT 1.
DEF VAR liModeBI AS INT INIT 1.
DEF VAR liModeCliType AS INT INIT 1.
DEF VAR liModeCCN AS INT INIT 1.
DEF VAR liModeTariff AS INT INIT 1.
DEF VAR liModebdest AS INT INIT 1.

DEF TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.
DEF TEMP-TABLE ttRequestAction NO-UNDO LIKE RequestAction.
DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER bRequestAction FOR RequestAction.
DEF BUFFER bBillItem FOR BillItem.
DEF BUFFER bTariff FOR Tariff.
DEF BUFFER bBDest FOR BDest.
DEF BUFFER bRateCCN FOR RateCCN.
DEF VAR liActionId AS INT.
DEFINE BUFFER bBdestConf  FOR BDestConf.
DEF TEMP-TABLE ttBDestConf NO-UNDO LIKE BDestConf.
DEFINE BUFFER bBdestConfItem  FOR BDestConfItem.
DEF TEMP-TABLE ttBDestConfitem NO-UNDO LIKE BDestConfItem.
DEF TEMP-TABLE ttBDest NO-UNDO LIKE BDest.
DEF TEMP-TABLE ttRateCCN NO-UNDO LIKE RateCCN.
DEF VAR i AS INT NO-UNDO.

FUNCTION create_tmritem RETURNS CHAR (INPUT lcitem as CHAR,
                                      INPUT liruleseq as INT):
   FIND FIRST TMRItemValue WHERE
              TMRItemValue.tmruleseq =  liruleseq AND
              LOOKUP(lcitem, TMRItemValue.CounterItemValues) > 0
              NO-ERROR.
   IF NOT AVAIL TMRItemValue THEN DO:
      CREATE TMRItemValue.
      ASSIGN TMRItemValue.CounterItemValues = lcitem
             TMRItemValue.fromdate = ldaFrom
             TMRItemValue.todate = 12/31/49
             TMRItemValue.tmruleseq = liruleseq.
   END.
END.

create_tmritem("CONTDSL_DATA_IN,CONTDSL45",14).
create_tmritem("CONTFH50_DATA_IN,CONTFH45_50",14).
create_tmritem("CONTFH300_DATA_IN,CONTFH55_300",14).

create_tmritem("CONTDSL_DATA_IN,CONTDSL58",14).
create_tmritem("CONTFH50_DATA_IN,CONTFH58_50",14).
create_tmritem("CONTFH300_DATA_IN,CONTFH68_300",14).

create_tmritem("CONTS2GB_DATA_IN,CONTDSL45",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH45_50",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH55_300",14).

create_tmritem("CONTDATA24,CONTDSL58",14).
create_tmritem("CONTDATA24,CONTFH58_50",14).
create_tmritem("CONTDATA24,CONTFH68_300",14).

create_tmritem("CONTDSL_QTY_IN,CONTDSL45",34).
create_tmritem("CONTFH50_QTY_IN,CONTFH45_50",34).
create_tmritem("CONTFH300_QTY_IN,CONTFH55_300",34).

create_tmritem("CONTDSL_QTY_IN,CONTDSL58",34).
create_tmritem("CONTFH50_QTY_IN,CONTFH58_50",34).
create_tmritem("CONTFH300_QTY_IN,CONTFH68_300",34).

create_tmritem("CONTDSL_MIN_IN,CONTDSL45",34).
create_tmritem("CONTFH50_MIN_IN,CONTFH45_50",34).
create_tmritem("CONTFH300_MIN_IN,CONTFH55_300",34).

create_tmritem("CONTDSL_MIN_IN,CONTDSL58",34).
create_tmritem("CONTFH50_MIN_IN,CONTFH58_50",34).
create_tmritem("CONTFH300_MIN_IN,CONTFH68_300",34).

IF CAN-FIND(FIRST bitemgroup WHERE
                  bitemgroup.brand = "1" AND
                  bitemgroup.bigroup = "47") THEN
   MESSAGE "bigroup already found: 47" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE bitemgroup.
   ASSIGN bitemgroup.bigname = "Fixed voice"
          bitemgroup.brand = "1"
          bitemgroup.bigroup = "47"
          bitemgroup.grouptype = 0
          bitemgroup.invoiceorder = 32.
END.

IF CAN-FIND(FIRST bitemgroup WHERE
                  bitemgroup.brand = "1" AND
                  bitemgroup.bigroup = "51") THEN
   MESSAGE "bigroup already found: 51" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE bitemgroup.
   ASSIGN bitemgroup.bigname = "Calls Fixed"
          bitemgroup.brand = "1"
          bitemgroup.bigroup = "51"
          bitemgroup.grouptype = 0
          bitemgroup.invoiceorder = 20.
END.

IF CAN-FIND(FIRST bitemgroup WHERE
                  bitemgroup.brand = "1" AND
                  bitemgroup.bigroup = "53") THEN
   MESSAGE "bigroup already found: 53" VIEW-AS ALERT-BOX.
ELSE DO:
   CREATE bitemgroup.
   ASSIGN bitemgroup.bigname = "Premium Services Fixed"
          bitemgroup.brand = "1"
          bitemgroup.bigroup = "53"
          bitemgroup.grouptype = 0
          bitemgroup.invoiceorder = 30.
END.

IF CAN-FIND(FIRST pricelist WHERE
                  pricelist.brand EQ "1" AND
                  pricelist.plname = "Contrato fixed" AND
                  pricelist.pricelist = "CONTRATOFIXED") THEN
   MESSAGE "pricelist already exist" VIEW-AS ALERT-BOX.
ELSE DO:

   CREATE pricelist.
   ASSIGN pricelist.brand = "1"
          pricelist.currency = "EUR"
          pricelist.currunit = TRUE
          pricelist.dediclist = FALSE
          pricelist.inclvat = FALSE
          pricelist.plname = "Contrato fixed"
          pricelist.pricelist = "CONTRATOFIXED"
          pricelist.memo = "Contrato fixed pricelist"
          pricelist.rounding = 4.
END.



FIND LAST BDestConfItem USE-INDEX BDCItemID NO-LOCK NO-ERROR.
IF AVAILABLE BDestConfItem THEN i = BDestConfItem.BDCItemID.
ELSE i = 0.


FOR EACH bBdestConf:
   BUFFER-COPY bBDestConf TO ttBDestConf.
   IF SUBSTRING(ttBDestConf.BDCGroup,9) BEGINS "10" THEN NEXT.
   ASSIGN
      ttBDestConf.fromdate = ldafrom
      ttBDestConf.BDCGroup = SUBSTRING(bBDestConf.BDCGroup,1,8) + "10" +
                             SUBSTRING(bBDestConf.BDCGroup,9).
   FIND FIRST BDestConf WHERE
              Bdestconf.brand EQ "1" AND
              Bdestconf.BDCGroup EQ ttBDestConf.BDCGroup NO-ERROR.
   IF NOT AVAIL BDestConf THEN DO:
      CREATE BDestConf.
      BUFFER-COPY ttBDestConf TO BDestConf.
   END.

   FOR EACH bBDestConfItem WHERE
            bBDestConfItem.brand EQ "1" AND
            bBDestConfItem.BDCGroup EQ bBdestconf.BDCGroup:
      BUFFER-COPY bBDestConfItem TO ttBDestConfItem.
      i = i + 1.
      ASSIGN ttBDestConfItem.fromdate = ldafrom
             ttBDestConfItem.BDCItemID = i
             ttBDestConfItem.BDCGroup = SUBSTRING(bBDestConf.BDCGroup,1,8) +
             "10" + SUBSTRING(bBDestConf.BDCGroup,9)
             ttBDestConfItem.rateccn = 1000 + ttBDestConfItem.rateccn.
      FIND FIRST BDestConfitem WHERE
                 Bdestconfitem.brand EQ "1" AND
                 Bdestconfitem.BDCGroup EQ ttBDestConf.BDCGroup AND
                 Bdestconfitem.rateCCN EQ ttBDestConfItem.rateccn AND
                 Bdestconfitem.bdest = ttBDestConfItem.bdest NO-ERROR.
      IF NOT AVAIL BDestConfitem THEN DO:
         CREATE BDestConfitem.
         BUFFER-COPY ttBDestConfitem TO BDestConfitem.
         DELETE ttBDestConfitem.
      END.
   END.
END.


/* SLGANALYSE */

FUNCTION fcreateSLGAnalyse RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                             INPUT icDCEvent AS CHAR,
                                             INPUT idaVAlidFrom AS DATE,
                                             INPUT icclitype AS CHAR,
                                             INPUT iiUpdateMode AS INT,
                                             INPUT icBaseSLG AS CHAR,
                                             INPUT icSLG AS CHAR):
   FIND FIRST SLGAnalyse WHERE
              SLGAnalyse.clitype EQ icclitype AND
              SLGAnalyse.servicelimitgroup EQ icSLG AND
              SLGAnalyse.validto > TODAY NO-ERROR.
   IF AVAIL SLGAnalyse THEN RETURN TRUE.

   FOR EACH SLGAnalyse WHERE
            SLGAnalyse.brand EQ "1" AND
            SLGAnalyse.clitype EQ icBaseDCEvent AND
            SLGAnalyse.validto > TODAY.

      /*
      IF bSLGAnalyse.servicelimitgroup BEGINS "DSS" THEN NEXT.
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "MDUB3" THEN NEXT.
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "MDUB4" THEN NEXT.
      ELSE IF bSLGAnalyse.servicelimitgroup EQ "DATA7" THEN NEXT.
      */
      IF (icSLG BEGINS "CONTDSL" OR icSLG BEGINS "CONTFH") AND
         SLGAnalyse.servicelimitgroup NE icBaseSLG THEN NEXT.
      IF SLGAnalyse.CCN EQ 93 AND (icSLG BEGINS "CONTDSL" OR 
         icSLG BEGINS "CONTFH") THEN NEXT.
      CREATE ttSLGAnalyse.
      BUFFER-COPY SLGAnalyse TO ttSLGAnalyse.
      ttSLGAnalyse.ValidFrom = ldaFrom.
      ttSLGAnalyse.clitype = icCliType.
      IF  ttSLGAnalyse.servicelimitgroup EQ icBaseSLG THEN
         ttSLGAnalyse.servicelimitgroup = icSLG.
   END.
   FOR EACH ttSLGAnalyse:
      IF iiUpdateMode NE 0 THEN DO:
         CREATE SLGAnalyse.
         BUFFER-COPY ttSLGAnalyse TO SLGAnalyse.
         DISP ttSLGAnalyse.
      END.
      ELSE DISP ttSLGAnalyse.
      DELETE ttSLGAnalyse.
   END.
   IF AVAIL ttSLGAnalyse THEN delete ttSLGAnalyse.
END.

FUNCTION fModifySLGAnalyse RETURNS LOGICAL ( INPUT icDCEvent AS CHAR,
                                             INPUT iiUpdateMode AS INT):
   FOR EACH bSLGAnalyse WHERE
            bSLGAnalyse.brand EQ "1" AND
            bSLGAnalyse.clitype EQ icDCEvent AND
            bSLGAnalyse.validto > TODAY.

      IF bSLGAnalyse.servicelimitgroup BEGINS "CONTDSL" OR 
         bSLGAnalyse.servicelimitgroup BEGINS "CONTFH" THEN DO:
         IF bSLGAnalyse.CCN - 1000 < 0 THEN
            bSLGAnalyse.CCN = bSLGAnalyse.CCN + 1000.
         IF bSLGAnalyse.BillCode BEGINS "F" THEN 
            DISP "Billcode already fixed".
         ELSE 
            bSLGAnalyse.BillCode = "F" + bSLGAnalyse.BillCode.
      END.

      ELSE NEXT.
   END.
END.



fcreateSLGAnalyse("CONTDSL45","CONTDSL45",ldaFrom,"CONTDSL45",liMode,
                  "CONTS2GB","CONTDSL45").

fcreateSLGAnalyse("CONTFH45_50","CONTFH45_50",ldaFrom,"CONTFH45_50",liMode,
                  "CONTS2GB","CONTFH50").
fcreateSLGAnalyse("CONTFH55_300","CONTFH55_300",ldaFrom,"CONTFH55_300",liMode,
                  "CONTS2GB","CONTFH300").

fModifySLGAnalyse("CONTDSL45",liMode).
fModifySLGAnalyse("CONTFH45_50",liMode).
fModifySLGAnalyse("CONTFH55_300",liMode).

fcreateSLGAnalyse("CONTDSL45","CONTDSL58",ldaFrom,"CONTDSL58",liMode,
                  "CONTS2GB","CONT24").
fcreateSLGAnalyse("CONTFH45_50","CONTFH58_50",ldaFrom,"CONTFH58_50",liMode,
                  "CONTS2GB","CONT24").
fcreateSLGAnalyse("CONTFH55_300","CONTFH68_300",ldaFrom,"CONTFH68_300",liMode,
                  "CONTS2GB","CONT24").

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
                ttTariff.validFrom = ldafrom.
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
               ttTariff.billcode = ttTariff.billcode
               ttTariff.vPrice = 0
               ttTariff.vStartCharge = 0
               ttTariff.tariffnum = next-value(Tariff).
            IF INDEX(ttTariff.bdest, "QTY") > 0 THEN 
               ttTariff.billcode = "CONTDSL_QTY_IN".
            ELSE IF INDEX(ttTariff.bdest, "MIN") > 0 THEN
               ttTariff.billcode = "CONTDSL_MIN_IN".
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

FIND FIRST PListConf WHERE
           PListConf.brand EQ "1" AND
           PListConf.rateplan EQ "CONTRATOCONVS" AND
           PListConf.pricelist EQ "CONTRATOS" NO-ERROR.
IF NOT AVAIL PListConf THEN DO:
   CREATE PListConf.
   ASSIGN
      PListConf.dfrom = ldaFrom
      PlistConf.dto = 12/31/49
      PListConf.prior = 20
      PListConf.rateplan = "CONTRATOCONVS"
      PListConf.startcharge = TRUE
      PListConf.brand = "1"
      PListConf.pricelist = "CONTRATOS".
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

IF liModeBdest > 0 THEN DO:
   FOR EACH bBDest WHERE
            bBdest.desttype EQ 2 AND
            bBDest.todate > TODAY:
      FIND FIRST RateCCN WHERE
                 RateCCN.brand EQ "1" AND
                 RateCCN.bdest EQ bBdest.bdest AND
                 RateCCN.desttype EQ bBdest.desttype AND
                 RateCCN.ccn > 1000 NO-ERROR.
      IF NOT AVAIL RateCCN THEN DO:
         FIND FIRST RateCCN WHERE
                 RateCCN.brand EQ "1" AND
                 RateCCN.bdest EQ bBdest.bdest NO-ERROR.
         DISP RateCCN.bdest
              RateCCN.ccn FORMAT ">>>9".

      END.
      IF AVAIL RateCCN THEN DO:
         BUFFER-COPY RateCCN TO ttRateCCN.
         ttRateCCN.ccn = ttRateCCN.ccn + 1000.
         ttRateCCN.dialtype = 1.
         CREATE RateCCN.
         BUFFER-COPY ttRateCCN TO RateCCN.
      END.
      ELSE MESSAGE "not found rateCCN" VIEW-AS ALERT-BOX.
   END.

   FOR EACH bBDest WHERE
            bBdest.class EQ 2 AND
            bBDest.todate > TODAY:
      FIND FIRST RateCCN WHERE
                 RateCCN.brand EQ "1" AND
                 RateCCN.bdestid EQ bBdest.bdestid AND
                 RateCCN.dialtype EQ 1 NO-ERROR.
      IF NOT AVAIL RateCCN THEN DO:
         FIND FIRST RateCCN WHERE
                 RateCCN.brand EQ "1" AND
                 RateCCN.bdestid EQ bBdest.bdestid AND
                 RateCCN.dialtype EQ 4 NO-ERROR.
           /*
           DISP RateCCN.bdest
              RateCCN.ccn FORMAT ">>>9".
           */
         IF AVAIL RateCCN THEN DO:
            BUFFER-COPY RateCCN TO ttRateCCN.
            ttRateCCN.ccn = ttRateCCN.ccn + 1000.
            ttRateCCN.dialtype = 1.
            CREATE RateCCN.
            BUFFER-COPY ttRateCCN TO RateCCN.
         END.
      END.
   END.

   FOR EACH bRateCCN WHERE 
            bRateCCN.brand EQ "1" AND
            bRateCCN.dialtype EQ 20:
      IF bRateCCN.ccn > 1000 THEN NEXT.
      BUFFER-COPY bRateCCN TO ttRateCCN.
      ttRateCCN.ccn = ttRateCCN.ccn + 1000.
      ttRateCCN.dialtype = 23.
      CREATE RateCCN.
      BUFFER-COPY ttRateCCN TO RateCCN.
   END.

   FOR EACH bRateCCN WHERE
            bRateCCN.brand EQ "1" AND
            bRateCCN.dialtype EQ 21:
      IF bRateCCN.ccn > 1000 THEN NEXT.      
      BUFFER-COPY bRateCCN TO ttRateCCN.
      ttRateCCN.ccn = ttRateCCN.ccn + 1000.
      ttRateCCN.dialtype = 24.
      CREATE RateCCN.
      BUFFER-COPY ttRateCCN TO RateCCN.
   END.

   FIND FIRST DialType Where Dialtype.dialtype EQ 23 NO-ERROR.
   IF NOT AVAIL DialType THEN DO:
      CREATE DialType.
      ASSIGN
         DialType.dialtype = 23
         DialType.dtName = "Fixed Voice short number (type1)".
   END.
   FIND FIRST DialType Where Dialtype.dialtype EQ 24 NO-ERROR.
   IF NOT AVAIL DialType THEN DO:
      CREATE DialType.
      ASSIGN
         DialType.dialtype = 24
         DialType.dtName = "Fixed Voice short number (type2)".
   END.   
   FIND FIRST DialType Where Dialtype.dialtype EQ 50 NO-ERROR.
   IF NOT AVAIL DialType THEN DO:
      CREATE DialType.
      ASSIGN
         DialType.dialtype = 50
         DialType.dtName = "Fixed Voice Bdest".

   END.

END.

FIND FIRST RatePref WHERE
           RatePref.dialtype = 23 AND
           RatePref.prefix = "MOB" NO-ERROR.
IF NOT AVAIL RatePref THEN DO:
   CREATE RatePref.
   ASSIGN
      RatePref.dialtype = 23
      RatePref.brand = "1"
      RatePref.prefix = "MOB"
      RatePref.Ratepref = "".

   CREATE RatePref.
   ASSIGN
      RatePref.dialtype = 24
      RatePref.brand = "1"
      RatePref.prefix = "MOB"
      RatePref.Ratepref = "".

   CREATE RatePref.
   ASSIGN
      RatePref.dialtype = 50
      RatePref.brand = "1"
      RatePref.prefix = "MOB"
      RatePref.Ratepref = "".
END.

FUNCTION fCreateTrans RETURNS LOG (INPUT icCode AS CHAR,
                                   INPUT iiLang AS INT,
                                   INPUT icText AS CHAR):
   FIND FIRST RepText WHERE
              Reptext.brand EQ "1" AND
              Reptext.texttype EQ 1 AND
              Reptext.linkcode EQ icCode AND
              Reptext.language EQ iiLang NO-ERROR.

      IF NOT AVAIL RepText THEN DO:
         CREATE RepText.
         ASSIGN
            RepText.Brand    = "1"
            RepText.TextType = 1               /* Default value */
            RepText.LinkCode = icCode
            RepText.Language = iiLang
            RepText.FromDate = TODAY
            RepText.ToDate   = 12/31/49
            RepText.RepText  = icText  NO-ERROR.
       END.
   RETURN TRUE.
END.

fcreateTrans("F10100003",1,"Nacionales").
fcreateTrans("F10100003",2,"Nacionals").
fcreateTrans("F10100003",3,"Nazionalak").
fcreateTrans("F10100003",5,"National").

fcreateTrans("F10100004",1,"Al Extranjero").
fcreateTrans("F10100004",2,"A l'estranger").
fcreateTrans("F10100004",3,"Atzerrira").
fcreateTrans("F10100004",5,"Abroad").

fcreateTrans("F10100005",1,"Nacionales").
fcreateTrans("F10100005",2,"Nacionals").
fcreateTrans("F10100005",3,"Nazionalak").
fcreateTrans("F10100005",5,"National").

fcreateTrans("F15100027",1,"Números Gratuitos").
fcreateTrans("F15100027",2,"Números Gratuitos").
fcreateTrans("F15100027",3,"Doako zenbakiak").
fcreateTrans("F15100027",5,"Free Numbers").

fcreateTrans("FINFSERVICE",1,"At. otras emp").
fcreateTrans("FINFSERVICE",2,"A. altr. empr").
fcreateTrans("FINFSERVICE",3,"Bes. enp. at.").
fcreateTrans("FINFSERVICE",5,"Other co att").

fcreateTrans("FSHORTNU",1,"Tipo A").
fcreateTrans("FSHORTNU",2,"Tipus A").
fcreateTrans("FSHORTNU",3,"A mota").
fcreateTrans("FSHORTNU",5,"Level A").

fcreateTrans("FCCYOIGO",1,"Atención al cliente").
fcreateTrans("FCCYOIGO",2,"Atenció al client").
fcreateTrans("FCCYOIGO",3,"Bezeroarentzako arreta-zerbitzua").
fcreateTrans("FCCYOIGO",5,"Customer Service").

fcreateTrans("F15100023",1,"Llamadas Premium").
fcreateTrans("F15100023",2,"Trucades Premium").
fcreateTrans("F15100023",3,"Premium deiak").
fcreateTrans("F15100023",5,"Premium Calls").

fcreateTrans("CONTDSL_QTY_IN",1,"Nacionales").
fcreateTrans("CONTDSL_QTY_IN",2,"Nacionals").
fcreateTrans("CONTDSL_QTY_IN",3,"Nazionalak").
fcreateTrans("CONTDSL_QTY_IN",5,"National").

fcreateTrans("CONTDSL_MIN_IN",1,"Nacionales").
fcreateTrans("CONTDSL_MIN_IN",2,"Nacionals").
fcreateTrans("CONTDSL_MIN_IN",3,"Nazionalak").
fcreateTrans("CONTDSL_MIN_IN",5,"National").

fcreateTrans("CONTFH50_QTY_IN",1,"Nacionales").
fcreateTrans("CONTFH50_QTY_IN",2,"Nacionals").
fcreateTrans("CONTFH50_QTY_IN",3,"Nazionalak").
fcreateTrans("CONTFH50_QTY_IN",5,"National").

fcreateTrans("CONTFH50_MIN_IN",1,"Nacionales").
fcreateTrans("CONTFH50_MIN_IN",2,"Nacionals").
fcreateTrans("CONTFH50_MIN_IN",3,"Nazionalak").
fcreateTrans("CONTFH50_MIN_IN",5,"National").

fcreateTrans("CONTFH300_QTY_IN",1,"Nacionales").
fcreateTrans("CONTFH300_QTY_IN",2,"Nacionals").
fcreateTrans("CONTFH300_QTY_IN",3,"Nazionalak").
fcreateTrans("CONTFH300_QTY_IN",5,"National").

fcreateTrans("CONTFH300_MIN_IN",1,"Nacionales").
fcreateTrans("CONTFH300_MIN_IN",2,"Nacionals").
fcreateTrans("CONTFH300_MIN_IN",3,"Nazionalak").
fcreateTrans("CONTFH300_MIN_IN",5,"National").
