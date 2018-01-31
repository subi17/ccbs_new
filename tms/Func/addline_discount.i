&IF "{&ADDLINE_DISCOUNT_I}" NE "YES"
&THEN
&GLOBAL-DEFINE ADDLINE_DISCOUNT_I YES
{Func/fixedlinefunc.i}
{Mc/dpmember.i}

FUNCTION fCloseAddLineDiscount RETURNS LOGICAL
   (iiCustNum AS INT,
    iiMsSeq   AS INT,
    icCLIType AS CHAR,
    idtDate   AS DATE):
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum = iiCustNum NO-ERROR.

   IF NOT fCheckExistingConvergent(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS}),
                     iiMsSeq,
                     idtDate,
                     FALSE).
   IF NOT fCheckExisting2PConvergent(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_20}),
                     iiMsSeq,
                     idtDate,
                     FALSE).

   /* Additional Line with mobile only ALFMO-5 */
   IF NOT fCheckExistingMobileOnly(Customer.CustIDType,Customer.OrgID,icCLIType) THEN
      fCloseDiscount(ENTRY(LOOKUP(icCLIType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}),
                     iiMsSeq,
                     idtDate,
                     FALSE).

   RETURN TRUE.

END FUNCTION.

&ENDIF