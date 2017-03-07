/* ----------------------------------------------------------------------
  MODULE .......: create_charge_comp.p
  TASK .........: Create Changes and Compensation Request
  APPLICATION ..: 
  AUTHOR .......: rafaeldv
  CREATED ......: 
  CHANGED ......: 
                  
  Version ......: 
  ---------------------------------------------------------------------- */

{Func/timestamp.i}
{Func/fmakemsreq.i}
{Syst/tmsconst.i}
{Func/ftaxdata.i}

DEF INPUT  PARAMETER  icSource   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER  iiMsSeq    AS INT NO-UNDO.
DEF INPUT  PARAMETER  icUserCode AS CHAR  NO-UNDO.
DEF INPUT  PARAMETER  idCharge   AS DECIMAL NO-UNDO.
DEF INPUT  PARAMETER  icBEventId  AS CHAR NO-UNDO.  /* Postpaid = FeeModel , Prepaid = FeeModel,BillCode */
DEF INPUT  PARAMETER  iiOrigRequest AS INT NO-UNDO.
DEF OUTPUT PARAMETER  oiReqId    AS INT NO-UNDO.


FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
IF NOT AVAIL MobSub THEN DO:
   RETURN ERROR "MobSub not found".
END.
   
FIND Customer WHERE Customer.CustNum = MobSub.InvCust NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   RETURN ERROR "Invoice customer data not available".
END.
 
/* prepaid subscription */
IF MobSub.PayType THEN DO:

    /* check  current balance in case of charges */
    DEFINE VARIABLE ldCharge  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ldCurrBal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ldTaxPerc AS DECIMAL INITIAL 0 NO-UNDO.
    DEFINE VARIABLE lcTaxZone AS CHAR NO-UNDO.

    /* in prepaid we need to invert the sign  of the amount
       and give it in cents */
    ldCharge = -1 * idCharge * 100. 
    lcTaxZone = fRegionTaxZone(Customer.Region).
    
    /*create the  PrePaidRequest */
    CREATE PrePaidRequest.

    ASSIGN
          PrePaidRequest.TSRequest   = fMakeTS()
          PrePaidRequest.UserCode    = icUserCode 
          PrePaidRequest.Brand       = gcBrand
          PrePaidRequest.PPRequest   =  NEXT-VALUE(PrePaidReq)
          PrePaidRequest.PPStatus    = 0
          PrePaidRequest.Request     = "AdjustmentTRequest"
          PrePaidRequest.CLI         = MobSub.CLI
          PrePaidRequest.MsSeq       = MobSub.MsSeq
          PrePaidRequest.ReqCParam1  = icBEventId
          PrePaidRequest.TopUpAmt    = ldCharge.


     IF ldCharge < 0 THEN DO:  /* charge */ 
         
         /* apply tax percent acording to taxzone */ 
          PrePaidRequest.VatAmt     = ldCharge * ldTaxPerc / 100.   
          PrePaidRequest.TaxZone    = lcTaxZone. 
          PrePaidRequest.Source     = "CHARGE".
         /* include prefix */
         PrePaidRequest.PPReqPrefix = "978".

     END.
     ELSE DO: /* compensantion */

         PrePaidRequest.VatAmt      = 0. 
         PrePaidRequest.PPReqPrefix = "979". 
         PrePaidRequest.Source      = "COMP". 
     END.

    oiReqId = PrePaidRequest.PPRequest.          

END. /* ------------------------------------------------------------------------------- */

ELSE DO:

    /* postpaid subscription */
    /* create MsRequest */


    oiReqId = fChargeCompRequest(0,
                       icUserCode,
                       MobSub.MsSeq,
                       MobSub.CLI,
                       MobSub.CustNum,
                       idCharge,
                       icBEventId,
                       iiOrigRequest,
                       icSource).

END.

RETURN "".

