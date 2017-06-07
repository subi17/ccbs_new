DEF BUFFER bCustCat FOR CustCat.
DEF BUFFER bTMSCodes FOR TMSCodes.
DEF BUFFER bservicelimitTarget FOR ServiceLimitTarget.
DEF BUFFER bSLGAnalyse FOR SLGAnalyse.
DEF BUFFER tempSLGAnalyse FOR SLGAnalyse.

DEF VAR ldaFrom AS DATE INIT TODAY.
DEF VAR limode AS INT INIT 1.
DEF VAR laskuri AS INT.

DEFINE VARIABLE giMXSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE giSlSeq AS INTEGER NO-UNDO.


FUNCTION fcreateCustcat RETURNS CHAR (
   INPUT icbasecat AS CHAR,
   INPUT icnr AS CHAR,
   INPUT icname AS CHAR,
   INPUT iiSubLimit AS INT,
   INPUT iiActLimit AS INT,
   INPUT ilpro AS LOG,
   INPUT icSegment AS CHAR).
   
   IF CAN-FIND (FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icnr) THEN RETURN "Already exist".
   FIND FIRST custcat WHERE
              custcat.brand EQ "1" AND
              custcat.category EQ icbasecat NO-ERROR.
   IF NOT AVAIL CustCat THEN RETURN "Base not exist".
   CREATE bCustCat.
   BUFFER-COPY CustCat EXCEPT custcat.category TO bCustCat.   
   ASSIGN bCustCat.category = icnr
          bCustCat.catname = icname
          bCustCat.mobsublimit = iiSubLimit
          bCustCat.activationlimit = iiActLimit
          bCustCat.pro = ilpro
          bCustCat.segment = icsegment.
   DISP bcustcat.
   RELEASE bCustCat.
END FUNCTION.

fcreateCustCat("20", "21", "PRO SOHO Company CIF", 25, 35, TRUE, "PRO-Company").
fcreateCustCat("20", "22", "Big Companies CIF", 25, 35, TRUE, "PRO-Company").
fcreateCustCat("40", "42", "PRO Self-employee NIF", 5, 7, TRUE, "PRO-Self-employed").
fcreateCustCat("41", "43", "PRO Self-employee NIE", 5, 7, TRUE, "PRO-Self-employed").

FIND FIRST CustCat WHERE 
           custcat.brand EQ "1" AND
           custcat.category EQ "20".
   ASSIGN Custcat.catname = "SOHO Company CIF"
          Custcat.segment = "Company".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "40".
   ASSIGN Custcat.catname = "Self Employee NIF"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7
          Custcat.segment = "Self-employed".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "41".
   ASSIGN Custcat.catname = "Self Employee NIE"
          Custcat.mobsublimit = 5
          CustCat.activationlimit = 7
          Custcat.segment = "Self-employed".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "10".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "11".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "12".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "13".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "30".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "31".
   ASSIGN Custcat.segment = "Consumer".
FIND FIRST CustCat WHERE
           custcat.brand EQ "1" AND
           custcat.category EQ "99".
   ASSIGN Custcat.segment = "Consumer".

IF NOT CAN-FIND (FIRST Tmscodes WHERE
           tmscodes.codegroup EQ "order" AND
           tmscodes.fieldname EQ "orderchannel" AND
           tmscodes.codevalue EQ "fusion_telesales_pro") THEN DO:
   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "fusion_telesales" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.

   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "telesales" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.   

   FIND FIRST Tmscodes WHERE
              tmscodes.codegroup EQ "order" AND
              tmscodes.fieldname EQ "orderchannel" AND
              tmscodes.codevalue EQ "CC" NO-LOCK NO-error.
   CREATE btmscodes.
   BUFFER-COPY tmscodes except codevalue to btmscodes.
   ASSIGN
      btmscodes.codevalue = tmscodes.codevalue + "_PRO"
      btmscodes.codename = tmscodes.codename + " PRO".
   RELEASE btmscodes.

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

/*  old ones, can be removec */
fcreateDaycampaign("FLEX_UPSELL","FLEX_UPSELL_500MB","Flex upsell national GPRS","6",limode).
fcreateDaycampaign("FLEX_UPSELL","FLEX_UPSELL_5GB","Flex upsell national GPRS","6",limode).
fcreateDaycampaign("VOICE100","VOICE5000","Cont national voice","1",limode).
fcreateDaycampaign("VOICE100","VOICE200","Cont national voice","1",limode).
fcreateDaycampaign("VOICE100","INT_VOICE100","Cont international voice","1",limode).
fcreateDaycampaign("VOICE100","FIX_VOICE1000","Fixed national voice","1",limode).
fcreateDaycampaign("VOICE100","INT_FIX_VOICE1000","Fixed international voice","1",limode).
fcreateDaycampaign("VOICE100","SMS5000","National SMS","1",limode).


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
                                   INPUT liUnit AS INT,
                                   INPUT lcbasegroup AS CHAR):
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

   FIND FIRST Servicelimit WHERE
              Servicelimit.groupcode EQ lcbaseGroup NO-ERROR.
   IF AVAIL servicelimit THEN DO:
      FOR EACH servicelimittarget WHERE
               servicelimittarget.slseq eq Servicelimit.slseq:
         CREATE bservicelimitTarget.
         BUFFER-COPY servicelimittarget EXCEPT slseq TO bservicelimittarget.
         ASSIGN bservicelimittarget.slseq = laskuri
                bservicelimittarget.insiderate = lcCode.
      END.         
   END.
   return true.
END.

create_group("VOICE5000", "Voice 5000").
create_group("VOICE200", "Voice 200").
create_group("INT_VOICE100", "International Voice 100").
create_group("FIX_VOICE1000", "National fixed voice 1000").
create_group("INT_FIX_VOICE1000", "International fixed Voice 1000").
create_group("SMS5000", "SMS 5000").

create_limit("VOICE5000", "National calls", "_MIN",5000.0, 1, 1,"VOICE100").
create_limit("VOICE200", "National calls", "_MIN",200.0, 1, 1, "VOICE100").
create_limit("INT_VOICE100", "International calls", "_MIN",100.0, 1, 1,"VOICE100").
create_limit("FIX_VOICE1000", "National fixed calls", "_MIN",1000.0, 1, 1,"VOICE100").
create_limit("INT_FIX_VOICE1000", "International fixed calls", "_MIN",1000.0, 1, 1,"VOICE100").
create_limit("SMS5000", "National sms", "_QTY",5000.0, 2, 5,"VOICE100").

FUNCTION fCreateSLGAnalyse RETURNS LOGICAL
   ( icClitype AS CHARACTER,
     icBillCode AS CHARACTER,
     iiCCN AS INTEGER,
     icBDest AS CHARACTER,
     icServiceLimitGroup AS CHARACTER,
     iiSLGAType AS INTEGER ):

   FIND FIRST SLGAnalyse EXCLUSIVE-LOCK WHERE
      SLGAnalyse.Brand    = "1"        AND
      SLGAnalyse.BelongTo = TRUE       AND
      SLGAnalyse.Clitype  = icClitype  AND
      SLGAnalyse.BillCode = icBillCode AND
      SLGAnalyse.CCN      = iiCCN      AND
      SLGAnalyse.BDest    = icBDest    AND
      SLGAnalyse.Prior    = 0          AND
      SLGAnalyse.ValidTo  = DATE(12,31,2049) AND
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup AND
      SLGAnalyse.SLGAType = iiSLGAType
   NO-ERROR.

   IF NOT AVAILABLE SLGAnalyse
   THEN CREATE SLGAnalyse.

   ASSIGN
      SLGAnalyse.Brand    = "1"
      SLGAnalyse.BelongTo = TRUE
      SLGAnalyse.Clitype  = icClitype
      SLGAnalyse.BillCode = icBillCode
      SLGAnalyse.CCN      = iiCCN
      SLGAnalyse.BDest    = icBDest
      SLGAnalyse.Prior    = 0
      SLGAnalyse.ValidFrom = ldaFrom
      SLGAnalyse.ValidTo  = DATE(12,31,2049)
      SLGAnalyse.ServiceLimitGroup = icServiceLimitGroup
      SLGAnalyse.SLGAType = iiSLGAType
      .

END FUNCTION.


FUNCTION fcreateFixSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icbasegroup AS CHAR,
                                             INPUT icgroup AS CHAR):
   FIND FIRST SLGAnalyse where INDEX(SLGAnalyse.servicelimitgroup,
                                     icGroup) > 0 NO-ERROR.
   IF NOT AVAIL SLGanalyse THEN DO:                            
      FOR EACH tempSLGAnalyse no-lock where INDEX(SLGAnalyse.servicelimitgroup, 
                                        icBaseGroup) > 0 AND 
                                        SLGANalyse.clitype EQ icclitype:
         CREATE bSLGAnalyse.
         BUFFER-COPY tempSLGAnalyse TO bSLGAnalyse.
         ASSIGN
            bSLGAnalyse.servicelimitgroup = icGroup.
            
         RELEASE bSLGAnalyse.   
      END.                            
   END.

END.

FUNCTION fcreateVoiceSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icgroup AS CHAR):
   fCreateSLGAnalyse(icClitype, "10100001", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100003", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "10100005", 81, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFOTHER", 30, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "CFYOIGO", 30, "*", icGroup, 1).
END.

FUNCTION fcreateSMSSLGAnalyses RETURNS LOGICAL (INPUT icClitype AS CHAR,
                                             INPUT icgroup AS CHAR):
   fCreateSLGAnalyse(icClitype, "12100001", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100002", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100003", 51, "*", icgroup, 1).
   fCreateSLGAnalyse(icClitype, "12100004", 51, "*", icgroup, 1).
END.


DEF VAR lcListofclitypes AS CHAR.
DEF VAR liLoop AS INT.
DEF VAR lcCli AS CHAR.
lcListofclitypes = "CONT10,CONT15,CONT25,CONT26".

DO liLoop = 1 TO NUM-ENTRIES(lcListofClitypes):
   lcCli = ENTRY(liLoop,lcListOfClitypes).

   fcreateVoiceSLGAnalyses(lcCli,"VOICE5000").
   fcreateVoiceSLGAnalyses(lcCli,"VOICE200").
   fcreateVoiceSLGAnalyses(lcCli,"INT_VOICE100").
   fcreateSMSSLGAnalyses(lcCli,"SMS5000").

END.

lcListofclitypes = "CONTDSL39,CONTDSL48,CONTDSL52,CONTDSL59,CONTFH39_50,CONTFH48_50,CONTFH49_300,CONTFH52_50,CONTFH58_300,CONTFH59_50,CONTFH62_300,CONTFH69_300".

DO liLoop = 1 TO NUM-ENTRIES(lcListofClitypes):
   lcCli = ENTRY(liLoop,lcListOfClitypes).

   fcreateFixSLGAnalyses(lcCli,"CONTDSL","FIX_VOICE1000").
   fcreateFixSLGAnalyses(lcCli,"CONTDSL","INT_FIX_VOICE1000").
END.

FUNCTION fGetNextMXSeq RETURNS INTEGER ():

   DEFINE BUFFER Matrix FOR Matrix.

   FOR EACH Matrix NO-LOCK BY Matrix.MXSeq DESCENDING:
     RETURN Matrix.MXSeq + 1.
   END.

   RETURN 1.

END FUNCTION.

FUNCTION fCreateMatrix RETURNS LOGICAL
   ( icMXName  AS CHARACTER,
     icMXKey   AS CHARACTER,
     iiMXRes   AS INTEGER,
     iiPrior   AS INTEGER ):

   FIND FIRST Matrix EXCLUSIVE-LOCK WHERE
      Matrix.Brand  = "1"      AND
      Matrix.MXKey  = icMXKey  AND
      Matrix.Prior  = iiPrior  AND
      Matrix.MXName = icMXName
   NO-ERROR.

   IF NOT AVAILABLE Matrix
   THEN CREATE Matrix.
   ELSE DO:
      giMXSeq       = Matrix.MXSeq.
      RETURN TRUE.
   END.
   IF Matrix.MXSeq = 0
   THEN Matrix.MXSeq = fGetNextMXSeq().

   ASSIGN
      giMXSeq       = Matrix.MXSeq
      Matrix.Brand  = "1"
      Matrix.MXKey  = icMXKey
      Matrix.Prior  = iiPrior
      Matrix.MXName = icMXName
      Matrix.MXRes  = iiMXRes.

END FUNCTION.

FUNCTION fCreateMXItem RETURNS LOGICAL
   ( iiMXSeq   AS INTEGER,
     icMXName  AS CHARACTER,
     icMXValue AS CHARACTER):

   FIND FIRST MXItem EXCLUSIVE-LOCK WHERE
      MXItem.MXSeq  = iiMXSeq AND
      MXItem.MXName = icMXName AND
      MXItem.MXValue = icMXValue
   NO-ERROR.

   IF NOT AVAILABLE MXItem
   THEN CREATE MXItem.

   ASSIGN
      MXItem.MXSeq   = iiMXSeq
      MXItem.MXName  = icMXName
      MXItem.MXValue = icMXValue.

END FUNCTION.

fCreateMatrix("Convergent 5GB  mobile", "PERCONTR", 1, 40).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTDSL59", "PERCONTR", 1, 44).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH59_50", "PERCONTR", 1, 45).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH69_300", "PERCONTR", 1, 46).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTDSL39", "PERCONTR", 1, 47).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTDSL52", "PERCONTR", 1, 48).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH39_50", "PERCONTR", 1, 49).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH49_300", "PERCONTR", 1, 50).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH52_50", "PERCONTR", 1, 51).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONTFH62_300", "PERCONTR", 1, 52).
fCreateMXItem(giMXSeq, "PerContract", "FIX_VOICE1000").
fCreateMXItem(giMXSeq, "PerContract", "INT_FIX_VOICE1000").

fCreateMatrix("CONT26", "PERCONTR", 1, 42).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").

fCreateMatrix("CONT10", "PERCONTR", 1, 43).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").

fCreateMatrix("Per.contract usage", "PERCONTR", 1, 41).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").

fCreateMatrix("Per.contract usage", "PERCONTR", 1, 29).
fCreateMXItem(giMXSeq, "PerContract", "VOICE200").
fCreateMXItem(giMXSeq, "PerContract", "VOICE5000").
fCreateMXItem(giMXSeq, "PerContract", "INT_VOICE100").
fCreateMXItem(giMXSeq, "PerContract", "SMS5000").
