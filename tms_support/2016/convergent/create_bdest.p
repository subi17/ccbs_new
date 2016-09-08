DEF VAR ldafrom AS DATE INIT 09/07/16.


FUNCTION create_bdest RETURNS CHAR (INPUT ictariffcode AS CHAR):
   DEF VAR liBDCount AS INT.
   DEF VAR lcBDestList AS CHAR INIT "VOICE_IN,VOICE_OUT".
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

FUNCTION create_tmritem RETURNS CHAR (INPUT lcitem as CHAR,
                                      INPUT liruleseq as INT):
   FIND FIRST TMRItemValue WHERE 
              TMRItemValue.tmruleseq = 14 AND
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

create_bdest("CONTDSL45").
create_bdest("CONTDSL55").
create_bdest("CONTFH45_50").
create_bdest("CONTFH55_50").
create_bdest("CONTFH55_300").
create_bdest("CONTFH65_300").

create_tmritem("CONTDSL45_DATA_IN,CONTDSL45",14).
create_tmritem("CONTDSL55_DATA_IN,CONTDSL55",14).
create_tmritem("CONTFH45_50_DATA_IN,CONTFH45_50",14).
create_tmritem("CONTFH55_50_DATA_IN,CONTFH55_50",14).
create_tmritem("CONTFH55_300_DATA_IN,CONTFH55_300",14).
create_tmritem("CONTFH65_300_DATA_IN,CONTFH65_300",14).
create_tmritem("CONTS2GB_DATA_IN,CONTDSL45",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH45_50",14).
create_tmritem("CONTS2GB_DATA_IN,CONTFH55_300",14).
create_tmritem("CONTS10GB_DATA_IN,CONTDSL55",14).
create_tmritem("CONTS10GB_DATA_IN,CONTFH55_50",14).
create_tmritem("CONTS10GB_DATA_IN,CONTFH65_300",14).

create_tmritem("GPRSDATA_DATA*,CONTDSL45",33).
create_tmritem("GPRSDATA_DATA*,CONTDSL55",33).
create_tmritem("GPRSDATA_DATA*,CONTFH45_50",33).
create_tmritem("GPRSDATA_DATA*,CONTFH55_50",33).
create_tmritem("GPRSDATA_DATA*,CONTFH55_300",33).
create_tmritem("GPRSDATA_DATA*,CONTFH65_300",33).

create_tmritem("CONTDSL45_VOICE_IN,CONTDSL45",34).
create_tmritem("CONTDSL55_VOICE_IN,CONTDSL55",34).
create_tmritem("CONTFH45_50_VOICE_IN,CONTFH45_50",34).
create_tmritem("CONTFH55_50_VOICE_IN,CONTFH55_50",34).
create_tmritem("CONTFH55_300_VOICE_IN,CONTFH55_300",34).
create_tmritem("CONTFH65_300_VOICE_IN,CONTFH65_300",34).
create_tmritem("CONTS2GB_VOICE_IN,CONTDSL45",34).
create_tmritem("CONTS2GB_VOICE_IN,CONTFH45_50",34).
create_tmritem("CONTS2GB_VOICE_IN,CONTFH55_300",34).
create_tmritem("CONTS10GB_VOICE_IN,CONTDSL55",34).
create_tmritem("CONTS10GB_VOICE_IN,CONTFH55_50",34).
create_tmritem("CONTS10GB_VOICE_IN,CONTFH65_300",34).

CREATE bitemgroup.
ASSIGN bitemgroup.bigname = "Convergent Monthly fee"
       bitemgroup.brand = "1"
       bitemgroup.bigroup = "46"
       bitemgroup.grouptype = 0
       bitemgroup.invoiceorder = 31.

CREATE bitemgroup.
ASSIGN bitemgroup.bigname = "Fixed voice"
       bitemgroup.brand = "1"
       bitemgroup.bigroup = "47"
       bitemgroup.grouptype = 0
       bitemgroup.invoiceorder = 32.

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



