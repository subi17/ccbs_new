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
/*Ilkka muuta:*/


create_tmritem("CONT9_DATA_IN,CONTDSL40",14).
create_tmritem("CONT9_DATA_IN,CONTFH40_50",14).
create_tmritem("CONT9_DATA_IN,CONTFH50_300",14).


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



fcreateSLGAnalyse("CONTDSL45","CONTDSL40",ldaFrom,"CONTDSL40",liMode,
                  "CONTS2GB","CONT9").
fcreateSLGAnalyse("CONTFH45_50","CONTFH40_50",ldaFrom,"CONTFH40_50",liMode,
                  "CONTS2GB","CONT9").
fcreateSLGAnalyse("CONTFH55_300","CONTFH50_300",ldaFrom,"CONTFH50_300",liMode,
                  "CONTS2GB","CONT9").

