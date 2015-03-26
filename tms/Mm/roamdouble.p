/* ----------------------------------------------------------------------
  MODULE .......: ROAMDOUBLE.P
  TASK .........: Delete duplicated RoamCDRs
  APPLICATION ..: TMS
  AUTHOR .......: kl
  CREATED ......: 25.04.07
  CHANGED ......: 15.05.07 kl RoamGPRS

  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{detailvalue.i}

DEFINE INPUT PARAMETER pdaFrom AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pdaTo   AS DATE NO-UNDO.

DEFINE BUFFER bufDouble1 FOR RoamCDR.
DEFINE BUFFER bufDouble2 FOR RoamGPRS.

DEF VAR liVer AS INT NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO. 
DEF VAR liEventSubType AS INT NO-UNDO EXTENT 5.
DEF VAR liOrigCDRType AS INT NO-UNDO EXTENT 5.
DEF VAR liDestImei AS INT NO-UNDO EXTENT 5.
      
DO i = 2 TO 5:
  liEventSubType[i] = fGetPosition("010" + STRING(i) + "MM","Event subtype").
  liOrigCDRType[i] = fGetPosition("010" + STRING(i) + "MM","Original cdr type").
  liDestImei[i] = fGetPosition("010" + STRING(i) + "MM","Destination IMEI").
END.

FOR EACH RoamCDR NO-LOCK WHERE
         RoamCDR.DateStart >= pdaFrom AND
         RoamCDR.DateStart <= pdaTo:

   liVer = LOOKUP(RoamCDR.Version,",0102,0103,0104,0105").
   IF liVer < 2 THEN NEXT.

   IF CAN-FIND(FIRST bufDouble1 NO-PREFETCH WHERE
                     bufDouble1.DateStart = RoamCDR.DateStart AND
                     bufDouble1.TimeStart = RoamCDR.TimeStart AND
                     bufDouble1.CLI       = RoamCDR.CLI       AND
               RECID(bufDouble1) NE RECID(RoamCDR)) THEN DO:

      FOR EACH bufDouble1 EXCLUSIVE-LOCK WHERE
               bufDouble1.DateStart = RoamCDR.DateStart AND
               bufDouble1.TimeStart = RoamCDR.TimeStart AND
               bufDouble1.CLI       = RoamCDR.CLI       AND
         RECID(bufDouble1) NE RECID(RoamCDR):

         IF bufDouble1.GsmBnr = RoamCdr.GsmBnr AND
            bufDouble1.EventType = RoamCdr.EventType THEN DO:

            IF ENTRY(liEventSubType[liVer],RoamCDR.CSV,"|")
               = ENTRY(liEventSubType[liVer],bufDouble1.CSV,"|") AND
               
               ENTRY(liOrigCDRType[liVer],RoamCDR.CSV,"|")
               = ENTRY(liOrigCDRType[liVer],bufDouble1.CSV,"|") AND
               
               ENTRY(liDestImei[liVer],RoamCDR.CSV,"|") 
               = ENTRY(liDestImei[liVer],bufDouble1.CSV,"|") THEN DO:
         
               DELETE bufDouble1.

            END.

         END.

      END.

   END.

END.

FOR EACH RoamGPRS NO-LOCK WHERE
         RoamGPRS.DateStart >= pdaFrom AND
         RoamGPRS.DateStart <= pdaTo:

   IF CAN-FIND(FIRST bufDouble2 NO-PREFETCH WHERE
                     bufDouble2.DateStart  = RoamGPRS.DateStart  AND
                     bufDouble2.TimeStart  = RoamGPRS.TimeStart  AND
                     bufDouble2.CallIdNum  = RoamGPRS.CallIdNum  AND
                     bufDouble2.PartRecNum = RoamGPRS.PartRecNum AND
                     bufDouble2.GGSNAddr   = RoamGPRS.GGSNAddr   AND
               RECID(bufDouble2) NE RECID(RoamGPRS)) THEN DO:

      FOR EACH bufDouble2 EXCLUSIVE-LOCK WHERE
               bufDouble2.DateStart  = RoamGPRS.DateStart  AND
               bufDouble2.TimeStart  = RoamGPRS.TimeStart  AND
               bufDouble2.CallIdNum  = RoamGPRS.CallIdNum  AND
               bufDouble2.PartRecNum = RoamGPRS.PartRecNum AND
               bufDouble2.GGSNAddr   = RoamGPRS.GGSNAddr   AND
         RECID(bufDouble2) NE RECID(RoamGPRS):

         DELETE bufDouble2.

      END.

   END.

END.

