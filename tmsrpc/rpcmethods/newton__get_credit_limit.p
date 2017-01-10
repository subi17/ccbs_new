/** YDR-2376
 * Get Credit Limit Details of the Customer
 *
 * @input: custnum;int;mandatory;id of customer 
 * @output: data;struct 
 * @data
          creditlimit;double;optional;
          pendingbalance;double;optional;
          withdrawn;double;optional;
          availablebalance;double;optional;
*/
{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{tmsconst.i}

/* Input parameters */
DEF VAR pcStruct       AS CHAR NO-UNDO. 
DEF VAR pcCustNum      AS INT  NO-UNDO.
/* Output parameters */
DEF VAR top_struct     AS CHAR NO-UNDO.
/* Local variables */
DEF VAR ldeLimitAmt    AS DEC  NO-UNDO.
DEF VAR ldePendingFees AS DEC  NO-UNDO.
DEF VAR ldeWithdrawn   AS DEC  NO-UNDO.
DEF VAR liPeriod       AS INT  NO-UNDO.
DEF VAR ldaDate        AS DATE NO-UNDO.

DEFINE BUFFER bMobSub FOR MobSub.
DEFINE BUFFER bTermMobSub FOR TermMobSub.

IF validate_request(param_toplevel_id, "int") = ? THEN RETURN.

pcCustNum = get_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = pcCustNum NO-ERROR.

IF NOT AVAIL Customer THEN
   RETURN appl_err("customer_not_found").

top_struct = add_struct(response_toplevel_id, "").

ASSIGN
   ldaDate  = DATE(MONTH(TODAY), 1, YEAR(TODAY)) - 1
   liPeriod = YEAR(ldaDate) * 100 + MONTH(ldaDate).

FIND FIRST Limit NO-LOCK WHERE
           Limit.CustNum   = Customer.Custnum        AND
           Limit.LimitType = {&LIMIT_TYPE_RISKLIMIT} AND
           Limit.ToDate   >= TODAY NO-ERROR.
IF AVAILABLE Limit THEN DO:
   
   ldeLimitAmt = Limit.LimitAmt.

   MOBSUB_LOOP:
   FOR EACH bMobSub NO-LOCK WHERE
            bMobSub.Brand   = gcBrand AND
            bMobSub.CustNum = Customer.CustNum:
      RUN pCalcTermFee(bMobSub.MsSeq).
   END.

   TERMMOBSUB_LOOP:
   FOR EACH bTermMobSub NO-LOCK WHERE
            bTermMobSub.Brand   = gcBrand AND
            bTermMobSub.CustNum = Customer.CustNum:
      RUN pCalcTermFee(bTermMobSub.MsSeq).
   END.

   FOR EACH Order NO-LOCK WHERE
            Order.Brand   = gcBrand          AND
            Order.CustNum = Customer.CustNum AND
      LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0,
      FIRST OfferItem NO-LOCK WHERE
            OfferItem.Brand       = gcBrand        AND
            OfferItem.Offer       = Order.Offer    AND
            OfferItem.ItemType    = "PerContract"  AND
            OfferItem.ItemKey     BEGINS "PAYTERM" AND
            OfferItem.EndStamp   >= Order.CrStamp  AND
            OfferItem.BeginStamp <= Order.CrStamp:

      ldeWithdrawn = ldeWithdrawn + OfferItem.Amount.

      FOR FIRST FMItem NO-LOCK WHERE
                FMItem.Brand     = gcBrand           AND
                FMItem.FeeModel  = OfferItem.ItemKey AND
                FMItem.ToDate   >= TODAY             AND
                FMItem.FromDate <= TODAY:
          ldeWithdrawn = ldeWithdrawn + (FMItem.Amount * FMItem.FFItemQty).
      END.
   END.

END.

add_double(top_struct, "creditlimit"     , ldeLimitAmt).
add_double(top_struct, "pendingbalance"  , ldePendingFees).
add_double(top_struct, "withdrawn"       , ldeWithdrawn).
add_double(top_struct, "availablebalance", (ldeLimitAmt - ldePendingFees - ldeWithdrawn)).

PROCEDURE pGetTermFee:
   DEFINE INPUT PARAM iiMsSeq AS INT NO-UNDO.
   
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq    = iiMsSeq       AND
            DCCLI.ValidTo >= ldaDate       AND
           (DCCLI.DCEvent BEGINS "PAYTERM" OR
            DCCLI.DCEvent BEGINS "RVTERM"):
      FOR FIRST FixedFee NO-LOCK WHERE
                FixedFee.Brand       = gcBrand                     AND
                FixedFee.Custnum     = Customer.Custnum            AND
                FixedFee.HostTable   = "MobSub"                    AND
                FixedFee.KeyValue    = STRING(iiMsSeq)             AND
                FixedFee.EndPeriod  >= liPeriod                    AND
                FixedFee.SourceTable = "DCCLI"                     AND
                FixedFee.SourceKey   = STRING(DCCLI.PerContractID) AND
               (FixedFee.BillCode BEGINS "PAYTERM"                 OR
                FixedFee.BillCode BEGINS "RVTERM"):

         FOR EACH FFItem OF FixedFee NO-LOCK:
            IF FFItem.Billed AND
              (FFItem.BillPeriod <= liPeriod OR
               CAN-FIND(FIRST Invoice WHERE
                              Invoice.InvNum  = FFItem.InvNum AND
                              Invoice.InvType = 1)) THEN NEXT.
            ldePendingFees = ldePendingFees + FFItem.Amt.
         END.

         IF FixedFee.BillCode BEGINS "PAYTERM" THEN
         FOR FIRST SingleFee NO-LOCK WHERE
                   SingleFee.Brand       = gcBrand              AND
                   SingleFee.Custnum     = FixedFee.CustNum     AND
                   SingleFee.HostTable   = FixedFee.HostTable   AND
                   SingleFee.KeyValue    = FixedFee.KeyValue    AND
                   SingleFee.SourceTable = FixedFee.SourceTable AND
                   SingleFee.SourceKey   = FixedFee.SourceKey   AND
                   SingleFee.CalcObj     = "RVTERM":
            IF SingleFee.Billed = TRUE AND
               CAN-FIND (FIRST Invoice NO-LOCK WHERE
                               Invoice.InvNum  = SingleFee.InvNum AND
                               Invoice.InvType = {&INV_TYPE_NORMAL}) THEN NEXT.
               ldePendingFees = ldePendingFees + SingleFee.Amt.
         END.
      END.
   END.
END PROCEDURE.

