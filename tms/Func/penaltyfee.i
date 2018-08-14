&IF "{&PENALTYFEE_I}" NE "YES"
&THEN
&GLOBAL-DEFINE PENALTYFEE_I YES
/* penaltyfee.i     18.06.08/aam
   
   penalty fee calculation formula
*/   

{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fixedfee.i}
{Mm/fbundle.i}
{Func/fcustpl.i}
{Func/istc.i}
{Func/fixedlinefunc.i}

DEF BUFFER New2CLIType     FOR CLIType.
DEF BUFFER Old2CLIType     FOR CLIType.


FUNCTION fCalculateFactor RETURNS DEC
   (idtBegContract AS DATE,     /* contract begins */
    idtRenewalDate AS DATE,     /* extension date */
    idtEndContract AS DATE,     /* contract should have ended */
    idtEndContractOrig AS DATE, /* original contract end date */
    idtCalcDate    AS DATE,     /* when calculated */
    iiCalcMethod   AS INT):     /* full, proportional */
    
   DEF VAR ldFactor    AS DEC  NO-UNDO.
   DEF VAR liDaysLeft  AS INT  NO-UNDO.
   DEF VAR liTotalDays AS INT  NO-UNDO.

   IF idtEndContractOrig = ? THEN idtEndContractOrig = idtEndContract.
   IF idtRenewalDate NE ? THEN idtBegContract = idtRenewalDate.
      
   IF idtBegContract = ? OR idtEndContract = ? OR idtCalcDate = ? OR
      idtEndContract < idtBegContract OR
      idtCalcDate > idtEndContract OR
      TODAY < idtBegContract OR
      iiCalcMethod = 0 
   THEN ldFactor = 0.
   
   /* proportional fee, based on time left on contract */   
   ELSE DO:
      IF iiCalcMethod = 2 THEN ASSIGN
         liDaysLeft  = idtEndContract + 1 - idtCalcDate
         liTotalDays = idtEndContractOrig - idtBegContract + 1
         ldFactor = MAX(0,liDaysLeft / liTotalDays).
      
      ELSE ldFactor = 1. 
   END.
   
   /* penalty factor cannot be greater than 1 
     (can happen if termination date is before begin date) */
   IF ldFactor > 1 THEN ldFactor = 1.

   RETURN ldFactor.
   
END FUNCTION.

FUNCTION fGetReferenceTariff RETURNS CHARACTER
         (INPUT pcDCEvent      AS CHAR,
          INPUT pdtValidFrom   AS DATE,
          INPUT pdtExtentDate AS DATE,
          INPUT piMsSeq        AS INT):

   DEF VAR ldTS        AS DEC  NO-UNDO.
   DEF VAR lcCLIType   AS CHAR NO-UNDO.
   DEF VAR lcReqParam2 AS CHAR NO-UNDO.

   DEF BUFFER bufMsRequest FOR MsRequest .

   IF pdtExtentDate <> ? THEN
      ASSIGN
         ldTS        = Func.Common:mHMS2TS(pdtExtentDate,"00:00:00")
         lcReqParam2 = "update".
   ELSE
      ASSIGN
         ldTS        = Func.Common:mHMS2TS(pdtValidFrom,"00:00:00")
         lcReqParam2 = "act,recreate".

   FIND FIRST bufMsRequest WHERE
              bufMsRequest.MsSeq      = piMsseq                        AND
              bufMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
              bufMsRequest.ReqStatus  = {&REQUEST_STATUS_DONE}         AND
              bufMsRequest.DoneStamp >= ldTS                           AND
              bufMsRequest.ReqCParam3 = pcDCEvent                      AND
       LOOKUP(bufMsRequest.ReqCparam2,lcReqParam2) > 0
   NO-LOCK USE-INDEX MsSeq NO-ERROR.

   IF AVAIL bufMsRequest THEN
      ldTS = bufMsRequest.DoneStamp.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = piMsSeq AND
              MsOwner.TSBegin <= ldTS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN
      FIND LAST MsOwner WHERE
                MsOwner.MsSeq = piMsSeq AND
                MsOwner.TSBegin > ldTS NO-LOCK NO-ERROR.
   
   IF AVAIL MsOwner THEN DO:
      lcCLIType = MsOwner.CLIType.

      IF MsOwner.TariffBundle <> "" THEN 
         lcCLIType = MsOwner.TariffBundle.
   END.

   RETURN lcCLIType.
END FUNCTION.

FUNCTION fCalcPenalty RETURNS CHARACTER
   (piMsSeq            AS INTEGER,
    pcNewCLIType       AS CHARACTER
   ):

   DEF VAR ldePeriodFee             AS DEC   NO-UNDO.
   DEF VAR ldeTotalPenaltyFee       AS DEC   NO-UNDO EXTENT 2.
   DEF VAR lcPenaltyCode            AS CHAR  NO-UNDO.
   DEF VAR lcPriceList              AS CHAR  NO-UNDO.
   DEF VAR lcOrigCLIType            AS CHAR  NO-UNDO.
   DEF VAR ldaSTCDates              AS DATE  NO-UNDO EXTENT 2.
   DEF VAR liLengthAfterExtension   AS INT   NO-UNDO EXTENT 2.
   DEF VAR liLoop                   AS INT   NO-UNDO.
   DEF VAR ldePenaltyFactor         AS DEC   NO-UNDO. 
   DEF VAR ldtFrom                  AS DATE  NO-UNDO. 
   DEF VAR ldtTo                    AS DATE  NO-UNDO. 
   DEF VAR lliSTCAllowed            AS LOG   NO-UNDO.
   DEF VAR ldeNextMonthTS           AS DEC   NO-UNDO.
   DEF VAR ldeEndTS                 AS DEC   NO-UNDO.
   DEF VAR ldOriginalFee            AS DEC   NO-UNDO.
   DEF VAR ldNewFee                 AS DEC   NO-UNDO.

   ASSIGN
      ldaSTCDates[1] = TODAY + 1
      ldaSTCDates[2] = Func.Common:mLastDayOfMonth(TODAY) + 1
      ldeNextMonthTS = Func.Common:mMake2DT(ldaSTCDates[2],0)
      ldeEndTS       = Func.Common:mMake2DT(Func.Common:mLastDayOfMonth(TODAY),86399).
   
   lliSTCAllowed = fIsiSTCAllowed(INPUT piMsSeq).
   
   FIND FIRST New2CLIType WHERE
              New2CLIType.CLIType = pcNewCLIType NO-LOCK NO-ERROR.

   /* Count possible penalty fee for contract termination */
   CONTRACT_LOOP:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidFrom <= ldaSTCDates[1] AND
            DCCLI.ValidTo   >= ldaSTCDates[1] AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = Syst.Var:gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:

      ASSIGN ldNewFee = 0 ldOriginalFee = 0 ldePeriodFee = 0.
   
      IF DCCLI.Amount NE ? THEN ldePeriodFee = DCCLI.Amount.
      ELSE DO:
         lcPriceList = fFeeModelPriceList(MobSub.Custnum,
                                          MobSub.BillTarget,
                                          DayCampaign.TermFeeModel,
                                          TODAY).
         FIND FIRST FMItem NO-LOCK WHERE
                    FMItem.Brand     = Syst.Var:gcBrand       AND
                    FMItem.FeeModel  = DayCampaign.TermFeeModel AND
                    FMItem.PriceList = lcPriceList AND
                    FMItem.FromDate <= TODAY     AND
                    FMItem.ToDate   >= TODAY NO-ERROR.
         IF AVAIL FMItem THEN ldePeriodFee = FMItem.Amount.
      END.

      IF ldePeriodFee EQ 0 THEN NEXT.
         
      IF New2CLIType.PayType EQ {&CLITYPE_PAYTYPE_POSTPAID} THEN DO:

         lcOrigCLIType = fGetReferenceTariff(DCCLI.DCEvent,
                                             DCCLI.ValidFrom,
                                             DCCLI.RenewalDate,
                                             MobSub.MsSeq).
         
         FIND FIRST Old2CLIType NO-LOCK WHERE
                    Old2CLIType.CLIType = lcOrigCLIType NO-ERROR.
         IF NOT AVAILABLE Old2CLIType THEN
            RETURN SUBST("Current CLIType entry &1 not found",
                                  lcOrigCLIType).

         IF DCCLI.DCEvent BEGINS "TERM" THEN 
         DO:
            IF fIsConvergentORFixedOnly(New2CLIType.CLIType) OR fIsConvergentORFixedOnly(Old2CLIType.CLIType) THEN 
            DO:
               ASSIGN 
                  ldOriginalFee = fGetMobileLineCompareFee(Old2CLIType.CLIType, Old2CLIType.BaseBundle, DCCLI.ValidFrom)
                  ldNewFee      = fGetMobileLineCompareFee(New2CLIType.CLIType, New2CLIType.BaseBundle, TODAY)
                  .

               IF ldOriginalFee <= ldNewFee THEN
                    NEXT.
            END.
            ELSE
            DO:
               IF Old2CLIType.CompareFee <= New2CLIType.CompareFee AND
                   NOT (LOOKUP(Old2CLIType.CliType,"CONT7,CONTD9") > 0 
                        AND New2CLIType.CLIType EQ "CONT8")
               THEN NEXT.
            END.
         END.
         /* When STCed between convergent tariffs we exclude FTERM and TVTERM from termination */
         ELSE IF (DCCLI.DCEvent BEGINS "FTERM" OR DCCLI.DCEvent BEGINS "TVTERM") AND
                 fIsConvergentORFixedOnly(New2CLIType.CLIType) THEN NEXT.
      END.
         
      ldtTo = DATETIME(DCCLI.ValidTo,0).

      /* calculate a factor for the fee (full / proportional) */
      DO liLoop = 1 TO EXTENT(ldaSTCDates):
         ldePenaltyFactor =  fCalculateFactor(DCCLI.ValidFrom,
                                              DCCLI.RenewalDate,
                                              DCCLI.ValidTo,
                                              DCCLI.ValidToOrig,
                                              ldaSTCDates[liLoop],
                                              DayCampaign.TermFeeCalc).
         
         IF ldePenaltyFactor EQ 0 THEN NEXT.
         IF TRUNCATE(ldePenaltyFactor * ldePeriodFee,0) EQ 0 THEN NEXT.
          
         ASSIGN
            ldeTotalPenaltyFee[liLoop] = ldeTotalPenaltyFee[liLoop]
               + TRUNCATE(ldePenaltyFactor * ldePeriodFee,0) 
            lcPenaltyCode = lcPenaltyCode + "+TERM" WHEN
               INDEX(lcPenaltyCode,"+TERM") = 0
            ldtFrom = DATETIME(ldaSTCDates[liLoop],0)
            liLengthAfterExtension[liLoop] =
              INTERVAL(ldtTo,ldtFrom,"months") + 13
              WHEN DCCLI.DCEvent BEGINS "TERM".
      END.


   END. /* FOR EACH DCCLI NO-LOCK WHERE */

   IF ldeTotalPenaltyFee[1] <> 0.0 OR ldeTotalPenaltyFee[2] <> 0.0 THEN
      RETURN SUBST("Penalty calculated for &1 as &2 and for &3 as &4 ",
                                  ldaSTCDates[1], ldeTotalPenaltyFee[1], ldaSTCDates[2], ldeTotalPenaltyFee[2]).
   ELSE
      RETURN "".
   
END FUNCTION.
&ENDIF
