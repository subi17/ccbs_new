{commpaa.i}
DEF VAR ldaFrom AS DATE INIT 10/27/16.
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

create_tmritem("CONTS2GB_DATA_IN,CONTDSL45",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH45_50",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH55_300",14).

create_tmritem("GPRSDATA_DATA*,CONTDSL45",33).
create_tmritem("GPRSDATA_DATA*,CONTFH45_50",33).
create_tmritem("GPRSDATA_DATA*,CONTFH55_300",33).

create_tmritem("CONTS2GB_VOICE_IN,CONTDSL45",34).
create_tmritem("CONTS2GB_VOICE_IN,CONTFH45_50",34).
create_tmritem("CONTS2GB_VOICE_IN,CONTFH55_300",34).


/* SLGANALYSE */

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

FUNCTION fcreateMobSLGAnalyse RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                                INPUT icDCEvent AS CHAR,
                                                INPUT idaVAlidFrom AS DATE,
                                                INPUT icclitype AS CHAR,
                                                INPUT iiUpdateMode AS INT):
   FOR EACH bSLGAnalyse WHERE
            bSLGAnalyse.brand EQ "1" AND
            bSLGAnalyse.clitype EQ icBaseDCEvent AND
            bSLGAnalyse.validto > TODAY.

      IF bSLGAnalyse.servicelimitgroup NE icBaseDCEvent THEN NEXT. 

      CREATE ttSLGAnalyse.
      BUFFER-COPY bSLGAnalyse TO ttSLGAnalyse.
      ttSLGAnalyse.ValidFrom = ldaFrom.
      ttSLGAnalyse.clitype = icCliType.
      ttSLGAnalyse.servicelimitgroup = icDCEvent.
      IF iiUpdateMode NE 0 THEN DO:
         CREATE SLGAnalyse.
         BUFFER-COPY ttSLGAnalyse TO SLGAnalyse.
         DELETE ttSLGAnalyse. /*ror safety reasons*/
      END.
      ELSE DISP ttSLGAnalyse.
   END.
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


fcreateMobSLGAnalyse("CONTFH45_50","CONTS2GB",ldaFrom,"CONTDSL45",liMode).
fcreateMobSLGAnalyse("CONTFH45_50","CONTS2GB",ldaFrom,"CONTFH45_50",liMode).
fcreateMobSLGAnalyse("CONTFH45_50","CONTS2GB",ldaFrom,"CONTFH55_300",liMode).

FUNCTION createTariffpack RETURNS LOG (INPUT lcBase AS CHAR,
                                       INPUT lcpricelist AS CHAR,
                                       INPUT lcBdest AS CHAR,
                                       INPUT liMode AS INT).
   IF CAN-FIND(FIRST Tariff WHERE
                     Tariff.brand EQ "1" AND
                     Tariff.pricelist EQ lcpricelist AND
                     Tariff.bdest BEGINS lcBdest) THEN
      MESSAGE "tariff bdest exists " + lcbdest VIEW-AS ALERT-BOX.
   ELSE DO:
      IF liMode > 0 THEN DO:   
          FOR EACH bTariff WHERE
                  bTariff.brand EQ "1" AND
                  bTariff.pricelist EQ lcpricelist AND
                  bTariff.bdest BEGINS lcBase AND            
                  bTariff.validto > TODAY:
             CREATE ttTariff.
             BUFFER-COPY bTariff TO ttTariff.
             ASSIGN
                ttTariff.bdest = REPLACE(ttTariff.bdest,lcBase,lcBdest).
                ttTariff.billcode = REPLACE(ttTariff.billcode,lcBase,lcBdest).
                IF ttTariff.bdest EQ "CONTS2GB_DATA_IN" THEN
                   ttTariff.billcode = "CONTS2GB_DATA_A".
                ELSE IF ttTariff.bdest EQ "CONTS2GB_DATA_OUT" THEN
                   ttTariff.billcode = "CONTS2GB_DATA_B".
             CREATE Tariff.
             ttTariff.tariffnum = next-value(Tariff).
             BUFFER-COPY ttTariff TO Tariff.
             DELETE ttTariff.
         END.
      END.
   END.


END.

createTariffpack("CONT24", "CONTRATOS", "CONTS2GB", liModeTariff).


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

/* Mobile line inside package billing items (La Infinita) */
createBillItem("CONT24VOICE_A", "CONTS2GBVOICE_A",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24CF_A", "CONTS2GBCF_A",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24VOICE_B", "CONTS2GBVOICE_B",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).
createBillItem("CONT24CF_B", "CONTS2GBCF_B",
               "Flat rate national Voice",
               70510100,"1","1","1","003",FALSE,"SL",liModeBI).

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
