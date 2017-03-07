&IF "{&FIXEDFEE}" NE "YES"
&THEN

&GLOBAL-DEFINE FIXEDFEE YES
{Syst/commali.i}
{Syst/tmsconst.i}
{Func/coinv.i}

FUNCTION fAmtUnBilledFFItem RETURNS DECIMAL 
         (INPUT iMsSeq          AS INT,
          INPUT iCustNum        AS INT,
          INPUT idtEnd          AS DATE,
          INPUT icCalcObj       AS CHAR,
          INPUT iiPerContractID AS INT,
          OUTPUT oiFFNum        AS INT,
          OUTPUT oiOrderId      AS INT):

   DEFINE VARIABLE liLongEnd  AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liShortEnd AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liEnd      AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldAmount   AS DECIMAL NO-UNDO INITIAL 0.
   DEFINE VARIABLE i          AS INT     NO-UNDO. 

   ASSIGN liLongEnd  = YEAR(idtEnd) * 10000 + 
                       MONTH(idtEnd) * 100  +
                       DAY(idtEnd)
          liShortEnd = YEAR(idtEnd) * 100 +
                       MONTH(idtEnd).

   FOR EACH FixedFee NO-LOCK WHERE
            FixedFee.Brand       = gcBrand                 AND 
            FixedFee.HostTable   = "MobSub"                AND
            FixedFee.KeyValue    = STRING(iMsSeq)          AND
            FixedFee.CustNum     = iCustNum                AND
            FixedFee.CalcObj     = icCalcObj               AND
            FixedFee.SourceTable = "DCCLI"                 AND
            FixedFee.SourceKey   = STRING(iiPerContractID) AND
            FixedFee.InUse       = TRUE USE-INDEX HostTable:
      
      i = i + 1.

      FOR EACH FFItem OF FixedFee NO-LOCK:

         IF FFItem.Concerns[1] > 999999
         THEN liEnd = liLongEnd.
         ELSE liEnd = liShortEnd.

         IF FFItem.Concerns[1] > liEnd THEN DO:
            IF FFItem.Billed AND
               CAN-FIND(FIRST Invoice NO-LOCK WHERE
                              Invoice.Invnum = FFItem.Invnum AND
                              Invoice.InvType = 1) THEN NEXT.

            IF i = 1 THEN ASSIGN
               oiFFNum = FixedFee.FFNum
               oiOrderId = FixedFee.OrderId.
            ELSE ASSIGN
               oiFFNum = 0
               oiOrderId = 0 WHEN oiOrderId NE FixedFee.OrderId.
             
            ldAmount = ldAmount + FFItem.Amt.
         END.
      END.
   END.

   RETURN ldAmount.

END FUNCTION.

FUNCTION fGetFixedFeeInfo RETURNS LOGICAL
         (INPUT iMsSeq AS INT,
          INPUT iiCustnum AS INT,
          INPUT icCalcObj AS CHAR,
          INPUT iiPercontractId AS INT,
          INPUT idaCountFrom AS DATE,
          OUTPUT odePendingFee AS DEC,
          OUTPUT oiTotalPeriods AS INT,
          OUTPUT odePeriodFee AS DEC,
          OUTPUT odeResidualFee AS DEC,
          OUTPUT ocFinancedInfo AS CHAR,
          OUTPUT oiOrderID AS INT):

   DEF VAR ldAmount AS DECIMAL NO-UNDO INITIAL 0.
   DEF VAR liFFNum AS INT NO-UNDO. 
   DEF VAR liFFCount AS INT NO-UNDO. 
   DEF VAR liPeriodFrom AS INT NO-UNDO. 
   DEF VAR liCountPeriod AS INT NO-UNDO. 

   DEF BUFFER FixedFee FOR FixedFee.
   DEF BUFFER FFItem FOR FFItem.
   DEF BUFFER SingleFee FOR SingleFee.

   DEF VAR liPeriod AS INT NO-UNDO. 
   DEF VAR ldaDate AS DATE NO-UNDO. 
   DEF VAR lcTFBank AS CHAR NO-UNDO.

   ASSIGN
      ldaDate = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
      liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

   IF idaCountFrom NE ? THEN
      ASSIGN liPeriodFrom  = YEAR(idaCountFrom) * 10000 + 
                             MONTH(idaCountFrom) * 100  +
                             DAY(idaCountFrom).
             liCountPeriod  = YEAR(idaCountFrom) * 100 + 
                             MONTH(idaCountFrom).

   FOR EACH FixedFee NO-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand     = gcBrand   AND 
            FixedFee.Custnum   = iiCustnum AND
            FixedFee.HostTable = "MobSub"  AND
            FixedFee.KeyValue  = STRING(iMsSeq) AND
            FixedFee.EndPeriod >= liPeriod AND
            FixedFee.BillCode BEGINS "PAYTERM" AND
            FixedFee.InUse     = TRUE:

      IF icCalcObj > "" AND
         FixedFee.CalcObj NE icCalcObj THEN NEXT.

      IF iiPercontractId > 0 THEN DO:
         IF FixedFee.SourceTable NE "DCCLI" THEN NEXT.
         IF FixedFee.SourceKey NE STRING(iiPercontractId) THEN NEXT.
      END.
      
      liFFNum = 0.

      FOR EACH FFItem OF FixedFee NO-LOCK:

         IF idaCountFrom NE ? AND
            FFItem.Concerns[1] < liPeriodFrom THEN NEXT.

         ASSIGN
            oiTotalPeriods = oiTotalPeriods + 1
            odePeriodFee = FixedFee.Amt
            liFFNum = FixedFee.FFNum
            odePendingFee = odePendingFee + FFItem.Amt WHEN NOT FFItem.Billed.

      END.

      IF liFFNum > 0 THEN DO:

         liFFCount = liFFCount + 1.
         IF liFFCount EQ 1 THEN 
            ASSIGN ocFinancedInfo = FixedFee.FinancedResult
                   lcTFBank       = FixedFee.TFBank
                   oiOrderID      = FixedFee.OrderId.
         ELSE IF ocFinancedInfo NE FixedFee.FinancedResult THEN ASSIGN
            ocFinancedInfo = "UNKNOWN"
            oiOrderId = 0.
         ELSE oiOrderId = 0.
      
         FOR FIRST SingleFee NO-LOCK WHERE
                   SingleFee.Brand       = gcBrand AND
                   SingleFee.Custnum     = FixedFee.CustNum AND
                   SingleFee.HostTable   = FixedFee.HostTable AND
                   SingleFee.KeyValue    = FixedFee.KeyValue AND
                   SingleFee.SourceTable = FixedFee.SourceTable AND
                   SingleFee.SourceKey   = FixedFee.SourceKey AND
                   SingleFee.CalcObj     = "RVTERM":

             IF SingleFee.Billed = TRUE AND
                CAN-FIND (FIRST Invoice NO-LOCK WHERE
                                Invoice.InvNum  = SingleFee.InvNum AND
                                Invoice.InvType = 1) THEN NEXT.

             IF idaCountFrom NE ? AND
                SingleFee.BillPeriod < liCountPeriod THEN NEXT.

             IF NOT SingleFee.Billed THEN 
                odeResidualFee = odeResidualFee + SingleFee.Amt.
         END.
      END.
   END.
        
   IF oiTotalPeriods > 0 THEN DO:
      IF ocFinancedInfo EQ "UNKNOWN" THEN .
      ELSE IF LOOKUP(ocFinancedInfo,{&TF_STATUSES_BANK}) > 0 THEN DO:
         IF lcTFBank = {&TF_BANK_UNOE} THEN
            ocFinancedInfo = "UNO-E".
         ELSE IF lcTFBank = {&TF_BANK_SABADELL} THEN
            ocFinancedInfo = "SABADELL".
         ELSE IF lcTFBank = {&TF_BANK_CETELEM} THEN
            ocFinancedInfo = "CETELEM".
         ELSE ocFinancedInfo = "BANK".
      END.
      ELSE IF ocFinancedInfo EQ "" OR
              ocFinancedInfo BEGINS "Y" OR
              (LENGTH(ocFinancedInfo) EQ 2 AND ocFinancedInfo NE "00")
         THEN ocFinancedInfo = "YOIGO".
      ELSE ocFinancedInfo = "PENDING".
   END.

   RETURN (oiTotalPeriods > 0).

END FUNCTION.

&ENDIF
