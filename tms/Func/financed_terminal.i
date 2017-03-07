/* ----------------------------------------------------------------------
  MODULE .......: terminal_financing.i 
  TASK .........: Terminal financing related functions
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 10.06.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
&IF "{&FINANCED_TERMINAL_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE FINANCED_TERMINAL_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

FUNCTION fOrderContainsFinancedTerminal RETURNS CHAR
   (INPUT iiOrderId  AS INT,
    INPUT icDCEvent AS CHAR):

   DEF BUFFER Order FOR Order.
   DEF BUFFER OrderCustomer FOR OrderCustomer.
      
   FIND Order NO-LOCK WHERE
        Order.Brand  = gcBrand AND
        Order.OrderID = iiOrderId NO-ERROR.
   IF NOT AVAIL Order THEN
      RETURN {&TF_STATUS_YOIGO}.

   IF icDCEvent = "RVTERM12" THEN DO:

      IF CAN-FIND(FIRST SingleFee WHERE
                        SingleFee.Brand       = gcBrand AND
                        SingleFee.Custnum     = Order.CustNum AND
                        SingleFee.HostTable   = "MobSub" AND
                        SingleFee.KeyValue    = STRING(Order.MsSeq) AND
                        SingleFee.OrderId     = iiOrderId AND
                        SingleFee.BillCode    = "RVTERM1EF") THEN
                     /* YPR-3565 */
                     /* LOOKUP(SingleFee.BillCode,
                               {&TF_BANK_RVTERM_BILLCODES}) > 0) THEN */
         RETURN {&TF_STATUS_WAITING_SENDING}.
   END.
   ELSE IF icDCEvent BEGINS "PAYTERM" THEN DO:
   
      FOR FIRST OrderCustomer NO-LOCK WHERE
                OrderCustomer.Brand = gcBrand AND
                OrderCustomer.Order = iiOrderId AND
                OrderCustomer.RowType = 1:
      
         IF NOT OrderCustomer.Profession > "" THEN RETURN  {&TF_STATUS_YOIGO}.
         IF LOOKUP(OrderCustomer.CustIdType,"NIF,NIE") = 0 THEN
            RETURN {&TF_STATUS_YOIGO}.

         IF CAN-FIND(FIRST OfferItem NO-LOCK WHERE
                           OfferItem.Brand       = gcBrand        AND
                           OfferItem.Offer       = Order.Offer    AND
                           OfferItem.ItemType    = "PerContract"  AND
                           OfferItem.ItemKey BEGINS "PAYTERM"     AND
                           OfferItem.EndStamp   >= Order.CrStamp  AND
                           OfferItem.BeginStamp <= Order.CrStamp) THEN DO:
            RETURN (IF INDEX(Order.OrderChannel,"POS") > 0
                    THEN {&TF_STATUS_WAITING_SENDING}
                    ELSE {&TF_STATUS_HOLD_SENDING}).
         END.
      END.
   END.

   RETURN {&TF_STATUS_YOIGO}.
END.

&ENDIF