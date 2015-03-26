&IF "{&ISTC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ISTC_I YES

{date.i}
{timestamp.i}

DEF TEMP-TABLE ttMsOwner NO-UNDO
   FIELD CustNum          AS INT
   FIELD MsSeq            AS INT
   FIELD CLI              AS CHAR
   FIELD CLIType          AS CHAR
   FIELD TariffBundle     AS CHAR
   FIELD PeriodFrom       AS DEC
   FIELD PeriodTo         AS DEC
   FIELD FromDate         AS DATE
   FIELD ToDate           AS DATE
   INDEX MsSeqCust CustNum MsSeq ToDate DESC.

FUNCTION fGetISTCDate RETURNS DATE
   (INPUT iiMsSeq AS INT,
    INPUT iiCustnum AS INT,
    INPUT idaPeriod AS DATE):
   
   DEF VAR ldeFromTs   AS DEC NO-UNDO.
   DEF VAR ldeEndTs    AS DEC NO-UNDO.
   DEF VAR ldaISTCDate AS DATE NO-UNDO. 

   DEF BUFFER MsOwner FOR MsOwner.

   ASSIGN ldeFromTs = YEAR(idaPeriod) * 10000 + MONTH(idaPeriod) * 100 + 1
          ldeEndTs  = fMake2Dt(fLastDayOfMonth(idaPeriod),86399).
   
   IF iiCustnum > 0 THEN
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq    = iiMsSeq   AND
              MsOwner.TsBeg   >= ldeFromTs AND
              MsOwner.TsBeg   <= ldeEndTs  AND
              MsOwner.PayType  = FALSE     AND
              MsOwner.Custnum  = iiCustnum AND
              MsOwner.CLIEvent BEGINS "iS" NO-ERROR.
   ELSE
   FIND FIRST MsOwner NO-LOCK WHERE
              MsOwner.MsSeq    = iiMsSeq   AND
              MsOwner.TsBeg   >= ldeFromTs AND
              MsOwner.TsBeg   <= ldeEndTs  AND
              MsOwner.PayType  = FALSE     AND
              MsOwner.CLIEvent BEGINS "iS" NO-ERROR.

   IF NOT AVAIL MsOwner THEN RETURN ?.

   fTs2Date(MsOwner.TsBegin, OUTPUT ldaISTCDate).

   RETURN ldaISTCDate.
END.

FUNCTION fIsiSTCAllowed RETURNS LOGICAL (INPUT iiMsSeq AS INT):

   RETURN (fGetISTCDate(iiMsSeq,0,TODAY) = ?).

END FUNCTION. /* FUNCTION fIsiSTCAllowed RETURNS LOGICAL */

FUNCTION fGetMsOwnerTempTable RETURNS LOG (INPUT iiInvCust           AS INT,
                                           INPUT idaFromDate         AS DATE,
                                           INPUT idaToDate           AS DATE,
                                           INPUT ilCheckTariffBundle AS LOG,
                                           INPUT ilPayType           AS LOG):

   DEF VAR lciSTCCLIType            AS CHAR NO-UNDO.
   DEF VAR lciSTCTariffBundle       AS CHAR NO-UNDO.
   DEF VAR ldeiSTCFromTs            AS DEC  NO-UNDO.
   DEF VAR ldePeriodFrom            AS DEC  NO-UNDO.
   DEF VAR ldePeriodTo              AS DEC  NO-UNDO.
   DEF VAR ldaFromDate              AS DATE NO-UNDO.
   DEF VAR liTime                   AS INT  NO-UNDO.

   DEF BUFFER bMsOwner              FOR MsOwner.
   DEF BUFFER bbMsOwner             FOR MsOwner.

   ASSIGN ldePeriodFrom = fMake2Dt(idaFromDate,0)
          ldePeriodTo   = fMake2Dt(idaToDate,86399).

   FOR EACH bMsOwner NO-LOCK WHERE
            bMsOwner.InvCust  = iiInvCust     AND
            bMsOwner.TsBeg   <= ldePeriodTo   AND
            bMsOwner.TsEnd   >= ldePeriodFrom AND
            bMsOwner.PayType  = ilPayType
   BREAK BY bMsOwner.MsSeq
         BY bMsOwner.TsEnd DESC:
      IF FIRST-OF(bMsOwner.MsSeq) THEN DO:
         ASSIGN lciSTCCLIType = ""
                lciSTCTariffBundle = ""
                ldeiSTCFromTs = 0.

         CREATE ttMsOwner.
         ASSIGN ttMsOwner.CustNum    = bMsOwner.InvCust
                ttMsOwner.MsSeq      = bMsOwner.MsSeq
                ttMsOwner.CLI        = bMsOwner.CLI
                ttMsOwner.CLIType    = bMsOwner.CLIType
                ttMsOwner.TariffBundle = bMsOwner.TariffBundle
                ttMsOwner.PeriodFrom = ldePeriodFrom
                ttMsOwner.PeriodTo   = ldePeriodTo
                ttMsOwner.FromDate   = idaFromDate
                ttMsOwner.ToDate     = idaToDate.

         FIND FIRST bbMSOwner WHERE 
                    bbMSOwner.MsSeq    = bMsOwner.MsSeq AND
                    bbMsOwner.TsBeg   >= ldePeriodFrom  AND
                    bbMsOwner.TsBeg   <= ldePeriodTo    AND
                    bbMsOwner.PayType  = FALSE          AND
                    bbMSOwner.CLIEvent BEGINS "iS" NO-LOCK NO-ERROR.
         IF AVAIL bbMSOwner THEN DO:

            fSplitTS(bbMsOwner.TsBeg,OUTPUT ldaFromDate,OUTPUT liTime).

            ASSIGN ttMsOwner.PeriodFrom = bbMsOwner.TsBegin
                   ttMsOwner.FromDate   = ldaFromDate
                   lciSTCCLIType = bbMSOwner.CLIType
                   lciSTCTariffBundle = bbMSOwner.TariffBundle
                   ldeiSTCFromTs = fSecOffSet(bbMsOwner.TsBeg,-1).
         END.
      END.

      IF LAST-OF(bMsOwner.MsSeq) THEN DO:
         IF lciSTCCLIType > "" THEN DO:
            IF bMsOwner.CLIType <> lciSTCCLIType OR
               (ilCheckTariffBundle = TRUE AND
                bMsOwner.TariffBundle <> lciSTCTariffBundle) THEN DO:
               CREATE ttMsOwner.
               ASSIGN ttMsOwner.CustNum    = bMsOwner.InvCust
                      ttMsOwner.MsSeq      = bMsOwner.MsSeq
                      ttMsOwner.CLI        = bMsOwner.CLI
                      ttMsOwner.CLIType    = bMsOwner.CLIType
                      ttMsOwner.TariffBundle = bMsOwner.TariffBundle
                      ttMsOwner.PeriodFrom = ldePeriodFrom
                      ttMsOwner.PeriodTo   = ldeiSTCFromTs
                      ttMsOwner.FromDate   = idaFromDate
                      ttMsOwner.ToDate     = (ldaFromDate - 1).
            END.
            ELSE
               ASSIGN ttMsOwner.PeriodFrom = ldePeriodFrom
                      ttMsOwner.FromDate   = idaFromDate.
         END.
      END.
   END.

   RETURN TRUE.

END FUNCTION.

&ENDIF
