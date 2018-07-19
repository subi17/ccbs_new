/* ----------------------------------------------------------------------
  MODULE .......: profunc.i
  TASK .........: Functions for handling Yoigo PRO related functionality
                  Reference: YPRO-project
  APPLICATION ..: tms
  AUTHOR .......:
  VERSION.......:
  CREATED ......: 24.5.2017
  CHANGED ......:
  ------------------------------------------------------------------------*/

&IF "{&YOIGOPROFUNC_I}" NE "YES"
&THEN
&GLOBAL-DEFINE YOIGOPROFUNC_I YES

{Func/fixedlinefunc.i}
{Func/cparam2.i}

/* check pro */
FUNCTION fIsPro RETURNS LOGICAL
   (icCategory AS CHAR):

   DEF BUFFER CustCat FOR CustCat.

   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand EQ Syst.Var:gcBrand AND
              CustCat.Category EQ icCategory NO-ERROR.
              
   IF AVAIL CustCat AND Custcat.pro THEN RETURN TRUE.
   RETURN FALSE.
END.

/* check self employee */
FUNCTION fIsSelfEmpl RETURNS LOGICAL
   (icCategory AS CHAR):

   DEF BUFFER CustCat FOR CustCat.

   FIND FIRST CustCat NO-LOCK WHERE
              CustCat.Brand EQ Syst.Var:gcBrand AND
              CustCat.Category EQ icCategory NO-ERROR.

   IF AVAIL CustCat AND INDEX(custcat.catname, "self") > 0 THEN RETURN TRUE.
   RETURN FALSE.
END.

FUNCTION fGetSegment RETURNS CHAR
   (iiCustNum AS INT,
    iiorderId AS INT):

   DEF BUFFER bCustomer FOR Customer.
   DEF BUFFER bOrderCustomer FOR OrderCustomer.
   DEF BUFFER CustCat FOR CustCat.

   DEF VAR lcCategory AS CHAR NO-UNDO.

   IF iiCustNum > 0 THEN
   FIND FIRST bCustomer NO-LOCK  WHERE
              bCustomer.CustNum EQ iiCustNum
              NO-ERROR.

   IF AVAIL bCustomer THEN lcCategory = bCustomer.category.
   ELSE IF iiOrderid > 0 THEN DO:
      FIND FIRST bOrdercustomer WHERE
                 bOrdercustomer.brand EQ Syst.Var:gcBrand AND
                 bOrdercustomer.orderid EQ iiorderid AND
                 bOrdercustomer.rowtype EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
                 NO-LOCK NO-ERROR.
      IF AVAIL bOrdercustomer THEN lcCategory = bOrdercustomer.category.
   END.

   IF lcCategory > "" THEN DO:
      FIND FIRST CustCat NO-LOCK WHERE
                 CustCat.Brand = Syst.Var:gcBrand AND
                 CustCat.Category = lcCategory
                 NO-ERROR.
      IF AVAIL CustCat THEN
         RETURN CustCat.Segment.
   END.
   RETURN "Consumer".
END.

FUNCTION fGetSVAOffer RETURNS CHARACTER
    (icCliType AS CHAR,
     icDCEvent AS CHAR):

    DEF VAR ldeCurrentTS AS DECI NO-UNDO.

    DEFINE BUFFER bf_DiscOfferItem FOR OfferItem.

    ASSIGN ldeCurrentTS = Func.Common:mMakeTS().
    
    FOR EACH Offer WHERE Offer.Brand       = Syst.Var:gcBrand AND 
                         Offer.Active      = True             AND
                         Offer.ToDate     >= TODAY            AND 
                         Offer.Offer_type = "extra_offer"     NO-LOCK,
        FIRST OfferItem WHERE OfferItem.Brand     = Syst.Var:gcBrand     AND 
                              OfferItem.Offer     = Offer.Offer          AND 
                              OfferItem.ItemType  = "OptionalBundleItem" AND
                              OfferItem.ItemKey   = icDCEvent            AND 
                              OfferItem.EndStamp >= ldeCurrentTS         NO-LOCK,
        FIRST OfferCriteria WHERE OfferCriteria.Brand        = Syst.Var:gcBrand      AND 
                                  OfferCriteria.Offer        = Offer.Offer           AND 
                                  OfferCriteria.CriteriaType = "CLIType"             AND
                                  OfferCriteria.BeginStamp  <= ldeCurrentTS          AND 
                                  LOOKUP(icCliType, OfferCriteria.IncludedValue) > 0 AND 
                                  OfferCriteria.EndStamp    >= ldeCurrentTS          NO-LOCK:

        FIND FIRST bf_DiscOfferItem WHERE bf_DiscOfferItem.Brand     = Syst.Var:gcBrand AND 
                                          bf_DiscOfferItem.Offer     = Offer.Offer      AND 
                                          bf_DiscOfferItem.ItemType  = "DiscountPlan"   AND
                                          bf_DiscOfferItem.EndStamp >= ldeCurrentTS     NO-LOCK NO-ERROR.
        IF AVAIL bf_DiscOfferItem AND bf_DiscOfferItem.ItemKey <> "" THEN 
            RETURN bf_DiscOfferItem.Offer.
    END.

    RETURN "".  

END FUNCTION.

/*Function returns TRUE if the order exsists and it is done from PRO channel.*/
FUNCTION fIsProOrder RETURNS LOGICAL
   (iiOrderID AS INT):

   DEF BUFFER Order FOR Order.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand EQ  Syst.Var:gcBrand AND
              Order.OrderID EQ iiOrderID NO-ERROR.

   IF INDEX(Order.orderchannel,"PRO") > 0 THEN
      RETURN TRUE.
   ELSE RETURN FALSE.
   
END.

/*Function returns True if a tariff can be defined as 2P tariff.
NOTE: False is returned in real false cases and also in error cases. */
FUNCTION fIs3PTariff RETURNS LOGICAL
   (icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.

   FIND FIRST CLIType NO-LOCK WHERE
              CLIType.Brand EQ Syst.Var:gcBrand AND
              CLIType.CliType EQ icCLIType NO-ERROR.
   IF AVAIL CliType AND
            CliType.TariffType EQ {&CLITYPE_TARIFFTYPE_CONVERGENT}  THEN
      RETURN TRUE.

   RETURN FALSE.
END.


/*STC is restricted from Prepaid to postpaid and 2P*/
FUNCTION fValidateProSTC RETURNS CHAR
   (iiCustomer AS INT,
    icCurrCLIType AS CHAR,
    icNewCLIType AS CHAR):

   DEF BUFFER bCurr FOR CLIType.
   DEF BUFFER bNew FOR CLIType.
   DEF BUFFER Customer FOR Customer.
   DEF BUFFER mobsub FOR mobsub.

   DEF VAR ll3PFound AS LOGICAL NO-UNDO.
   
   FIND FIRST Customer NO-LOCK WHERE
              Customer.CustNum EQ iiCustomer NO-ERROR.
   IF NOT AVAIL Customer THEN RETURN "Customer not found".
   IF NOT fIsPro(Customer.Category) THEN RETURN "". /*No PRO logic needed*/

   FIND FIRST bCurr NO-LOCK WHERE
              bCurr.Brand EQ Syst.Var:gcBrand AND
              bCurr.Clitype EQ icCurrCLIType NO-ERROR.
   IF NOT AVAIL bCurr THEN RETURN "Incorrect CLIType".

   FIND FIRST bNew NO-LOCK WHERE
              bNew.Brand EQ Syst.Var:gcBrand AND
              bNew.Clitype EQ icNewCLIType NO-ERROR.
   IF NOT AVAIL bNew THEN RETURN "Incorrect CLIType".

   IF bCurr.PayType EQ {&CLITYPE_PAYTYPE_PREPAID} THEN RETURN "". /*No PRO logic for prepaid*/

   IF bNew.Paytype EQ {&CLITYPE_PAYTYPE_PREPAID} THEN 
      RETURN "STC to Prepaid is not allowed for Pro customer".
   IF fIsFixedOnly(bNew.Clitype) AND NOT fIs3PTariff(bCurr.Clitype)  THEN DO:
      ll3PFound = FALSE.
      FOR EACH Mobsub WHERE
               Mobsub.brand EQ Syst.Var:gcBrand AND
               Mobsub.custnum EQ iiCustomer:
         IF NOT fIs3PTariff(MobSub.clitype) THEN NEXT.
         ELSE ll3PFound = TRUE.
      END.
      IF NOT ll3PFound THEN RETURN "STC to 2P is not allowed for Pro customer".  /* STC to pro allowed from mobile to 2P and if there is still convergent left and YPPI-5 3P to 2P */
   END.   
   RETURN "".
END.

/*tested, ok*/
/*Function seeks COFF order for given Msrequest.
If the order is not found the function returns an error code.*/
FUNCTION fFindCOFFOrder RETURNS CHAR
   (iiMsSeq AS INT):
   DEF BUFFER bOrder FOR Order.

   FOR EACH bOrder NO-LOCK WHERE
            bOrder.MsSeq EQ iiMsSeq BY CrStamp DESC:
      IF bOrder.OrderType EQ {&ORDER_TYPE_ACC} THEN NEXT.
      IF fIsConvergenceTariff(bOrder.CLIType) THEN
         RETURN STRING(bOrder.OrderId).
   END.

   RETURN "ERROR: Order not found for mobsub " + STRING(iiMsSeq).
END.

FUNCTION fGetProFeemodel RETURNS CHAR
   (INPUT icCliType AS CHAR):

   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER DayCampaign FOR DayCampaign.

   FOR FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = icCliType AND
             CLIType.FixedBundle > "",
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = CLIType.FixedBundle:
      RETURN DayCampaign.FeeModel.
   END.
   FOR FIRST CLIType NO-LOCK WHERE
             CLIType.Brand = Syst.Var:gcBrand AND
             CLIType.CLIType = icCliType,
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = CLIType.clitype:
      RETURN DayCampaign.FeeModel.
   END.
   RETURN "".
END.

FUNCTION fgetActiveReplacement RETURNS CHAR (INPUT icClitype AS CHAR,
                                             INPUT icParam   AS CHAR):
   DEF VAR lcSubsMappings AS CHAR NO-UNDO.
   DEF VAR lcMappedSubs AS CHAR NO-UNDO.
   DEF VAR lcSubsFrom AS CHAR NO-UNDO.
   DEF VAR lcSubsTo AS CHAR NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO.

   lcSubsMappings = fCParamC(icParam).

   DO liloop = 1 TO NUM-ENTRIES(lcSubsMappings,"|"):
      ASSIGN
         lcMappedSubs = ENTRY(liloop, lcSubsMappings,"|")
         lcSubsFrom = ENTRY(1,lcMappedSubs,"=")
         lcSubsTo = ENTRY(2,lcMappedSubs,"=").
      IF LOOKUP(icClitype,lcSubsFrom) GE 1 THEN RETURN lcSubsTo.
   END.
   RETURN "".
END.

/*SVA of Yoigo PRO*/
/*This function provides a dirty solution.
Later in YPRO project we will consider if it is reason to make 
1) tmsparam to configure the SVAs
2) own table to configure SVA needs
3) make wider solution safe mapping table for the services */
FUNCTION fIsSVA RETURNS LOGICAL
   (INPUT icService AS CHAR,
    OUTPUT oiParams AS INT):

   DEFINE BUFFER bf_DayCampaign FOR DayCampaign.

   ASSIGN oiParams = (IF icService EQ "FAXTOEMAIL" THEN 2 
                      ELSE IF icService EQ "OFFICE365" THEN 1
                      ELSE 0).
   FIND FIRST bf_DayCampaign WHERE bf_DayCampaign.Brand   = Syst.Var:gcBrand AND 
                                   bf_DayCampaign.DCEvent = TRIM(icService)  NO-LOCK NO-ERROR.
   IF AVAIL bf_DayCampaign AND bf_DayCampaign.BundleTarget = {&DC_BUNDLE_TARGET_SVA} THEN 
       RETURN TRUE.

   RETURN FALSE.
END.

&ENDIF


