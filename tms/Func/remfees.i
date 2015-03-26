/* remfees.i         22.12.04/aam 

   note: mobsub and customer must be in buffer when calling these
   
   changes:          12.09.05/aam better logic for recursive calls
                     02.02.06/aam don't check fixedfee.custnum,
                                  singlefee to InvCust
                     
*/

{commali.i}
{eventval.i}
{fixedfee.i}
{create_eventlog.i}

DEF TEMP-TABLE ttFeeHandle NO-UNDO
   FIELD BillCode AS CHAR
   FIELD Amt      AS DEC 
   FIELD FeeDate  AS DATE.
   
DEF TEMP-TABLE ttSingleHandle NO-UNDO
   FIELD BillCode AS CHAR
   FIELD FeeBeg   AS INT
   FIELD BillPer  AS INT
   FIELD Billed   AS DEC
   FIELD Contract AS CHAR
   FIELD VatIncl  AS LOG
   INDEX BillCode BillCode.

/* delete items from old fees */
PROCEDURE pDelFixedFee:
     
   DEF INPUT  PARAMETER icBillCode   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icFeeModel   AS CHAR NO-UNDO. 
   DEF INPUT  PARAMETER idtFeeDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idFeeAmt     AS DEC  NO-UNDO. 
   /* delete credit items also */ 
   DEF INPUT  PARAMETER ilCredFee    AS LOG  NO-UNDO. 
   /* create a credit singlefee for already billed items */
   DEF INPUT  PARAMETER ilSingleCred AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER icUserCode   AS CHAR NO-UNDO. 
   DEF INPUT  PARAMETER icMemo       AS CHAR NO-UNDO. 
   DEF OUTPUT PARAMETER odBilledAmt  AS DEC  NO-UNDO. 
   DEF OUTPUT PARAMETER oiCredQty    AS INT  NO-UNDO. 
   
   DEF VAR ldtBegDate  AS DATE NO-UNDO.
   DEF VAR ldtEndDate  AS DATE NO-UNDO.
   DEF VAR liFFNum     AS INT  NO-UNDO.
   DEF VAR liFeeBeg    AS INT  NO-UNDO.
   DEF VAR llVatIncl   AS LOG  NO-UNDO.
   DEF VAR lcContract  AS CHAR NO-UNDO. 
   DEF VAR lcFeeProd   AS CHAR NO-UNDO. 
   DEF VAR ldAmt       AS DEC  NO-UNDO. 
   DEF VAR liFeeQty    AS INT  NO-UNDO. 
   DEF VAR llRecursive AS LOG  NO-UNDO. 
   DEF VAR ldBilled    AS DEC  NO-UNDO. 
     
   ASSIGN liFeeBeg   = YEAR(idtFeeDate) * 10000 +
                       MONTH(idtFeeDate) * 100  +
                       DAY(idtFeeDate)
          liFFNum    = 0
          oiCredQty  = 0.

   IF INDEX(icBillCode,"|Recursive") > 0 
   THEN ASSIGN llRecursive = TRUE
               icBillCode  = ENTRY(1,icBillCode,"|").
   ELSE ASSIGN llRecursive = FALSE.

   IF NOT llRecursive THEN DO:
      EMPTY TEMP-TABLE ttFeeHandle.
      EMPTY TEMP-TABLE ttSingleHandle.
   END.

   FOR EACH FixedFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
            FixedFee.Brand      = gcBrand              AND
            FixedFee.HostTable  = "MobSub"             AND
            FixedFee.KeyValue   = STRING(Mobsub.MsSeq) AND 
            (IF icBillCode > ""
             THEN FixedFee.BillCode = icBillCode 
             ELSE TRUE)                                AND
            (IF icFeeModel > ""
             THEN FixedFee.FeeModel = icFeeModel
             ELSE TRUE)                                AND
            FixedFee.InUse      = TRUE  
   BREAK BY FixedFee.BillCode 
         BY FixedFee.EndPeriod DESC:
   
      /* handle only newest */
      IF NOT FIRST-OF(FixedFee.BillCode) THEN NEXT.

      /* if amount is given then it must match */
      IF idFeeAmt NE ? AND FixedFee.Amt NE idFeeAmt THEN NEXT.
      
      ASSIGN llVatIncl   = FixedFee.VatIncl
             lcContract  = FixedFee.Contract
             lcFeeProd   = FixedFee.BillCode
             ldBilled    = 0
             liFeeQty    = 0.

      FOR EACH FFItem of FixedFee EXCLUSIVE-LOCK WHERE 
               FFItem.Concerns[1] >= liFeeBeg:
               
         /* count number of credit items so that same number of items
            will be credited from new monthly fee */
         IF FFItem.Amt < 0 THEN oiCredQty = oiCredQty + 1.
         
         /* unbilled can be deleted */
         IF NOT FFItem.Billed THEN DELETE FFItem.
         
         /* billed will be credited */
         ELSE ldBilled = ldBilled + FFItem.Amt.
         
         /* what if invoice is credited, but events are not released */
      END.

      FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
      
      /* is there an item that should be divided */
      IF liFeeBeg MOD 100 NE 1         AND
         AVAILABLE FFItem              AND
         FFItem.Concerns[1] < liFeeBeg AND
         FFItem.Concerns[2] > liFeeBeg
      THEN DO:

         IF FFItem.Amt < 0 THEN oiCredQty = oiCredQty + 1.

         ASSIGN ldtBegDate = fInt2Date(FFItem.Concerns[1],1)
                ldtEndDate = fInt2Date(FFItem.Concerns[2],2).
          
         /* credit amount from change date to item's end */
         IF FFItem.Billed THEN 
         ldBilled = ldBilled + FFItem.Amt * 
                               (ldtEndDate - idtFeeDate + 1) /
                               (ldtEndDate - ldtBegDate + 1).
         
         /* change amount to be billed from item's beginning to change date */
         ELSE DO:
            FIND CURRENT FFItem EXCLUSIVE-LOCK.
         
            ASSIGN 
            FFItem.Amt         = FFItem.Amt * (idtFeeDate - ldtBegDate + 1)
                                            / (ldtEndDate - ldtBegDate + 1)
            FFItem.Concerns[2] = YEAR(idtFeeDate - 1) * 10000 +
                                 MONTH(idtFeeDate - 1) * 100  +
                                 DAY(idtFeeDate - 1).
         END. 
      END.

      /* mark fee as closed */
      FixedFee.EndPeriod = IF AVAILABLE FFITem
                           THEN FFItem.BillPer
                           ELSE FixedFee.BegPeriod.
 
      /* clean also credit fees made to same billcode */
      IF ilCredFee AND FixedFee.Amt > 0 THEN DO:
         CREATE ttFeeHandle.
         ASSIGN ttFeeHandle.BillCode = FixedFee.BillCode
                ttFeeHandle.Amt      = -1 * FixedFee.Amt
                ttFeeHandle.FeeDate  = idtFeeDate.
      END. 
                      
 
      /* some items were already billed -> credit them */
      IF ldBilled > 0 AND ilSingleCred THEN DO:
         CREATE ttSingleHandle.
         ASSIGN ttSingleHandle.BillCode = FixedFee.BillCode
                ttSingleHandle.FeeBeg   = liFeeBeg
                ttSingleHandle.BillPer  = FixedFee.EndPeriod
                ttSingleHandle.Billed   = ldBilled
                ttSingleHandle.Contract = lcContract
                ttSingleHandle.VatIncl  = llVatIncl.
      END.

      odBilledAmt = odBilledAmt + ldBilled.             
                  
   END.                        

   /* clean also credit fees made to same billcode */
   IF NOT llRecursive THEN 
   FOR EACH ttFeeHandle:
     
      RUN pDelFixedFee(ttFeeHandle.BillCode + "|Recursive",
                       "",
                       ttFeeHandle.FeeDate,
                       ttFeeHandle.Amt,
                       FALSE,  /* clean credit fees also */
                       FALSE,  /* credit singlefee for billed items */
                       icUsercode,
                       icMemo,
                       OUTPUT ldAmt,
                       OUTPUT liFeeQty).

      oiCredQty = oiCredQty + liFeeQty.

      /* reduce billed credit items from normal billed items */
      IF ilSingleCred THEN DO:
         FIND FIRST ttSingleHandle WHERE 
                    ttSingleHandle.BillCode = ttFeeHandle.BillCode NO-ERROR.
         IF AVAILABLE ttSingleHandle 
         THEN ttSingleHandle.Billed = ttSingleHandle.Billed + ldAmt.
      END.   
   END.
   
   /* single fee from already billed items */
   IF NOT llRecursive THEN 
   FOR EACH ttSingleHandle WHERE 
            ttSingleHandle.Billed > 0:

      IF TRUNCATE(ttSingleHandle.FeeBeg / 100,0) > 0 AND
         TRUNCATE(ttSingleHandle.FeeBeg / 100,0) < ttSingleHandle.BillPer
      THEN ttSingleHandle.BillPer = TRUNCATE(ttSingleHandle.FeeBeg / 100,0).
      
      RUN pCreditSingleFee(ttSingleHandle.BillCode,
                           ttSingleHandle.FeeBeg,
                           ttSingleHandle.BillPer,
                           ttSingleHandle.Billed * -1,
                           ttSingleHandle.Contract,
                           ttSingleHandle.VatIncl,
                           icUserCode,
                           icMemo).
   END. 
     
END PROCEDURE.

/* singlefee for crediting already billed fixed fees */
PROCEDURE pCreditSingleFee:

   DEF INPUT PARAMETER icBillCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiPeriod   AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiBillPer  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idAmt      AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icContract AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ilVatIncl  AS LOG  NO-UNDO. 
   DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO. 
   DEF INPUT PARAMETER icMemo     AS CHAR NO-UNDO. 
  
   IF idAmt = 0 THEN RETURN.
   
   CREATE SingleFee.
   ASSIGN SingleFee.Brand      = gcBrand
          SingleFee.FMItemId   = NEXT-VALUE(bi-seq)
          SingleFee.CustNum    = MobSub.InvCust
          SingleFee.BillTarget = MobSub.BillTarget
          SingleFee.BillCode   = icBillCode
          SingleFee.BillPer    = iiBillPer
          SingleFee.Concerns   = iiPeriod
          SingleFee.HostTable  = "MobSub"
          SingleFee.KeyValue   = STRING(MobSub.MsSeq)
          SingleFee.CalcObj    = "MSSeq" + STRING(MobSub.MsSeq)
          SingleFee.VATIncl    = ilVatIncl
          SingleFee.BillType   = "KM"
          SingleFee.Contract   = icContract
          SingleFee.Active     = TRUE
          SingleFee.Amt        = idAmt.
          SingleFee.Memo[1]    = DYNAMIC-FUNCTION("fHdrText" IN ghFunc1,
                                                  187,
                                                  Customer.Language).

   fMakeCreateEvent((BUFFER SingleFee:HANDLE),"",icUserCode,icMemo).
   
   RELEASE SingleFee.
   
END PROCEDURE.

