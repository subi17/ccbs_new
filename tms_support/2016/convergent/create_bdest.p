
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
            BDest.FromDate = TODAY
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
             TMRItemValue.fromdate = TODAY
             TMRItemValue.todate = 12/31/49
             TMRItemValue.tmruleseq = liruleseq.
   END.
END.

create_bdest("CONTDSL10").
create_bdest("CONTFH10").
create_bdest("CONTDSL15").
create_bdest("CONTFH15").

create_tmritem("CONTDSL10_DATA_IN,CONTDSL10",14).
create_tmritem("CONTDSL15_DATA_IN,CONTDSL15",14).
create_tmritem("CONTFH10_DATA_IN,CONTFH10",14).
create_tmritem("CONTFH15_DATA_IN,CONTFH15",14).
create_tmritem("CONTF2GB_DATA_IN,CONTFH10",14).
create_tmritem("CONTS5GB_DATA_IN,CONTFH15",14).

create_tmritem("GPRSDATA_DATA*,CONTDSL10",33).
create_tmritem("GPRSDATA_DATA*,CONTDSL15",33).
create_tmritem("GPRSDATA_DATA*,CONTFH10",33).
create_tmritem("GPRSDATA_DATA*,CONTFH15",33).

create_tmritem("CONTDSL10_VOICE_IN,CONTDSL10",34).
create_tmritem("CONTDSL15_VOICE_IN,CONTDSL15",34).
create_tmritem("CONTFH10_VOICE_IN,CONTFH10",34).
create_tmritem("CONTFH15_VOICE_IN,CONTFH15",34).
create_tmritem("CONTF2GB_VOICE_IN,CONTDSL10",34).
create_tmritem("CONTS5GB_VOICE_IN,CONTDSL15",34).


FOR EACH TMRItemValue WHERE TMRItemValue.tmruleseq = 34:
   DISP TMRItemValue.CounterItemValues.

END.
