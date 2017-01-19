/* creasfee.p       29.01.04/aam 
   create a fee from activation or execution of some event 
   
                    08.04.04/aam ilMessage, ocInfo
                    14.04.04/aam create contract (fFeeContract)
                    28.06.04/aam info to invoice according to ServFee.InvInfo
                    07.10.04/jp  setfees.i modified
                    13.10.05/aam icMemo can contain salesman
                    25.10.05/aam calling program to calcobject
                    17.11.05/aam take salesman from mobsub if not in icMemo
                    25.06.07/aam don't use fBegPer for period
                    10.09.07  vk using iiMemo to transfer the penalty fee
                                 coeeficient
*/

{commali.i}
{timestamp.i}
{setfees.i} 
{eventval.i}
{ffeecont.i}

DEF INPUT  PARAMETER iiCustNum     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiMSSeq       AS INT  NO-UNDO. 
DEF INPUT  PARAMETER idtDate       AS DATE NO-UNDO. 
DEF INPUT  PARAMETER icServType    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icServKey     AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiEventType   AS INT  NO-UNDO.
DEF INPUT  PARAMETER idPrice       AS DEC  NO-UNDO. /* new */
DEF INPUT  PARAMETER icMemo        AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER ilMessage     AS LOG  NO-UNDO. 
DEF INPUT  PARAMETER icUserCode    AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER icFeeMemo     AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER iiOrderId     AS INT  NO-UNDO. 
DEF INPUT  PARAMETER icSourceTable AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER icSourceKey   AS CHAR NO-UNDO. 
DEF OUTPUT PARAMETER ocInfo        AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttBillTarget NO-UNDO
   FIELD BillTarget AS INT.
   
DEF VAR liBillTarget     AS INT  NO-UNDO.
DEF VAR lcCalcObj        AS CHAR NO-UNDO.
DEF VAR liText           AS INT  NO-UNDO. 
DEF VAR lcSalesman       AS CHAR NO-UNDO.
DEF VAR lcCoefficient    AS CHAR NO-UNDO.
DEF VAR ldFromStamp      AS DEC  NO-UNDO.
DEF VAR ldToStamp        AS DEC  NO-UNDO.
DEF VAR llPostpone       AS LOG  NO-UNDO.
DEF VAR ldtDatePostpone  AS DATE NO-UNDO.
DEF VAR liTextPos        AS INT  NO-UNDO.

IF idtDate = ? THEN idtDate = TODAY.

ASSIGN lcCoefficient = ""
       llPostpone    = FALSE.

/* memo can contain calcobj and salesman */
liText = NUM-ENTRIES(icMemo,"¤").
IF liText > 3 THEN lcCoefficient = ENTRY(4,icMemo,"¤").
IF liText > 2 THEN lcSalesman    = ENTRY(3,icMemo,"¤").

IF liText > 1 THEN ASSIGN lcCalcObj = ENTRY(2,icMemo,"¤")
                          icMemo    = ENTRY(1,icMemo,"¤").

liTextPos = NUM-ENTRIES(icFeeMemo,"¤").
IF liTextPos = 2 AND ENTRY(2,icFeeMemo,"¤") = "Postpone" THEN
   llPostpone = TRUE.

ASSIGN
   ldFromStamp = fMake2Dt(idtDate,0)
   ldToStamp   = fMake2Dt(idtDate,86399).
      
IF iiMSSeq > 0 THEN DO:
   IF iiCustNum > 0 THEN DO:
      FIND FIRST MsOwner USE-INDEX MsSeq WHERE
                 MsOwner.MSSeq   = iiMSSeq AND
                 MsOwner.CustNum = iiCustNum  AND
                 MsOwner.TSEnd >= ldFromStamp AND
                 MsOwner.TSBeg <= ldToStamp NO-LOCK NO-ERROR.
      IF AVAILABLE MsOwner THEN DO:
         FIND FIRST MobSub WHERE MobSub.MsSeq = iiMSSeq NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub AND MobSub.CustNum = MsOwner.CustNum THEN DO:
            IF lcSalesman = "" THEN lcSalesman = MobSub.Salesman.           
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST MobSub WHERE MobSub.MsSeq = iiMSSeq NO-LOCK NO-ERROR.
      IF AVAILABLE MobSub THEN DO:
         FIND FIRST MsOwner USE-INDEX MsSeq WHERE
                    MSOwner.MsSeq = MobSub.MSSeq AND
                    MsOwner.CustNum = MobSub.CustNum  AND
                    MsOwner.TSEnd >= ldFromStamp AND
                    MsOwner.TSBeg <= ldToStamp NO-LOCK NO-ERROR.
         IF lcSalesman = "" THEN lcSalesman = MobSub.Salesman.           
      END.              
   END.
   
   IF NOT AVAILABLE MsOwner THEN DO:
      ocInfo = "ERROR:Unknown subscriber (" + STRING(iiMSSeq) + ")".
      IF ilMessage THEN 
      MESSAGE ocInfo
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   IF iiCustNum = 0 THEN iiCustNum = MSOwner.CustNum.          
   
   CREATE ttBillTarget.
   ttBillTarget.BillTarget = MSOwner.BillTarget.
   
END.

/* go through customer's each billtarget and use the first one that
   fees can be created to */
ELSE FOR EACH BillTarget NO-LOCK WHERE
              BillTarget.CustNum = iiCustNum:
   CREATE ttBillTarget.
   ttBillTarget.BillTarget = BillTarget.BillTarget.
END.

IF lcCalcObj = "" THEN lcCalcObj = PROGRAM-NAME(2).

IF lcCoefficient NE "" THEN lcCalcObj = lcCalcObj + "¤" + lcCoefficient.
FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   ocInfo = "ERROR:Unknown customer (" + STRING(iiCustNum) + ")".
   IF ilMessage THEN 
   MESSAGE ocInfo
   VIEW-AS ALERT-BOX.
   RETURN.
END.

/* icServType = "FeeModel" -> feemodel given directly (in icServKey), 
   otherwise go through service fee definition */
   
ocInfo = "ERROR:No service fee definition available".
   
IF icServType = "FeeModel" AND iiEventType = 9 THEN DO:

   RUN pCreateFees(icServKey,
                   idPrice).
END.

ELSE 
FOR FIRST ServFee NO-LOCK WHERE           
          ServFee.Brand     = gcBrand     AND
          ServFee.ServType  = icServType  AND
          ServFee.ServKey   = icServKey   AND
          ServFee.EventType = iiEventType AND
          ServFee.FromDate <= idtDate     AND
          ServFee.ToDate   >= idtDate     AND
          ServFee.FeeModel > "":
      
   /* info to invoice */
   liText = 0. 
   liText = INTEGER(ServFee.InvInfo) NO-ERROR.
   IF liText > 0 THEN DO:
      icMemo = icMemo + (IF icMemo > "" THEN "¤" ELSE "") + 
               DYNAMIC-FUNCTION("fHdrText" IN ghFunc1,
                                liText,
                                Customer.Language).
   END.
      

   RUN pCreateFees(ServFee.FeeModel,
                   idPrice).
END.
            
                      
PROCEDURE pCreateFees:

   DEF INPUT PARAMETER icFeeModel AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idPrice    AS DEC  NO-UNDO.
    
   DEF VAR lcContract    AS CHAR NO-UNDO. 
   DEF VAR liCreated     AS INT  NO-UNDO. 

   /* YDR-1584 */
   IF MONTH(idtDate) = 12 
   THEN ldtDatePostpone = DATE(1,1,YEAR(idtDate) + 1).
   ELSE ldtDatePostpone = DATE(MONTH(idtDate) + 1,1,YEAR(idtDate)).

   FOR EACH ttBillTarget 
   BY ttBillTarget.BillTarget:

      /* contract */
      lcContract = fFeeContract(gcBrand,
                                iiCustNum,
                                lcSalesman,  /* if empty then
                                                take salesman from user */
                                idtDate,
                                "Fee creation").
   
      /* fee */
      liCreated = fMakeSetfees (icFeeModel,
                                iiCustNum,
                                iiMSSeq,
                                ttBillTarget.BillTarget,  
                                lcCalcObj,   
                                icMemo,
                                IF llPostpone 
                                THEN YEAR(ldtDatePostpone) * 100 + MONTH(ldtDatePostpone)
                                ELSE YEAR(idtDate) * 100 + MONTH(idtDate),
                                IF llPostpone
                                THEN ldtDatePostpone
                                ELSE idtDate,
                                idPrice,                /* price from feemodel */
                                lcContract,       /* contract */
                                icUserCode,
                                icFeeMemo,            /* active */
                                iiOrderId,
                                icSourceTable,
                                icSourceKey).
                 
      /* if several billtargets to be checked then leave when one
         is succesful */
      IF liCreated > 0 THEN LEAVE.
         
   END.
      
   IF ilMessage THEN 
      MESSAGE liCreated "fees have been created for customer" iiCustNum SKIP
              "from fee model" icFeeModel "."
      VIEW-AS ALERT-BOX
      TITLE " Fees Created ".
   
   ocInfo = STRING(liCreated) + " fees created from " + icFeeModel.
      
END PROCEDURE.


