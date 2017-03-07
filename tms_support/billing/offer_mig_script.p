/* ----------------------------------------------------------------------
  MODULE .......: offer_mig_script.p
  TASK .........: Offer Migration Script to add bundle items to support
                  IPL Migration Phase-2
  APPLICATION ..: 
  AUTHOR .......: Vikas
  CREATED ......: 31/05/2011
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "Qvantel".
{Func/timestamp.i}

DEFINE VARIABLE liNumEntries    AS INTEGER   NO-UNDO.
DEFINE VARIABLE liCount         AS INTEGER   NO-UNDO.
DEFINE VARIABLE liTestCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLastCriSeq    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcIncludedValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCLIType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeCurrStamp    AS DECIMAL   NO-UNDO.

DEFINE BUFFER bOfferCriteria    FOR OfferCriteria.
DEFINE BUFFER bbOfferCriteria   FOR OfferCriteria.
DEFINE BUFFER bGetOfferCriteria FOR OfferCriteria.

DEFINE STREAM slog.

ldeCurrStamp = fMakeTS().

OUTPUT STREAM slog TO "/apps/yoigo/tms_support/billing/offer_mig_ipl_20110810.xls" append.

PUT STREAM slog UNFORMATTED "Offer ID" CHR(9)
                            "Remark"   SKIP.

EACH_OFFER:
FOR EACH Offer WHERE
         Offer.Brand  = gcBrand AND
         Offer.Active = TRUE    AND
         Offer.ToDate >= TODAY  NO-LOCK,
   FIRST OfferCriteria WHERE
         OfferCriteria.Brand        = gcBrand       AND
         OfferCriteria.Offer        = Offer.Offer   AND
         OfferCriteria.CriteriaType = "CLITYPE"     AND
         OfferCriteria.BeginStamp  <= ldeCurrStamp  AND
         OfferCriteria.EndStamp    >= ldeCurrStamp  NO-LOCK:

    IF LOOKUP(OfferCriteria.IncludedValue, "CONTRD1,CONTRD2,CONTRD3") = 0
    THEN NEXT EACH_OFFER.

    ASSIGN lcIncludedValue = OfferCriteria.IncludedValue
           liNumEntries    = NUM-ENTRIES(lcIncludedValue).

    DO liCount = 1 To liNumEntries:
       lcCLIType = ENTRY(liCount,lcIncludedValue).
       CASE lcCLIType:
          WHEN "CONTRD1" THEN
             RUN pCreateOfferItem(INPUT "CONTDATA",
                                  INPUT OfferCriteria.Offer,
                                  INPUT "BundleItem").
          WHEN "CONTRD2" THEN
             RUN pCreateOfferItem(INPUT "CONTD2",
                                  INPUT OfferCriteria.Offer,
                                  INPUT "BundleItem").
          WHEN "CONTRD3" THEN
             RUN pCreateOfferItem(INPUT "MDUB",
                                  INPUT OfferCriteria.Offer,
                                  INPUT "BundleItem").
          OTHERWISE NEXT.
       END. /* CASE lcCLIType: */
    END. /* DO liCount = 1 To liNumEntries: */

    FOR EACH bGetOfferCriteria NO-LOCK
        BY bGetOfferCriteria.OfferCriteriaId DESC:
       liLastCriSeq = (bGetOfferCriteria.OfferCriteriaID + 1).
       LEAVE.
    END. /* FOR EACH bGetOfferCriteria NO-LOCK */

    DO TRANSACTION:
       FIND bOfferCriteria WHERE
            ROWID(bOfferCriteria) = ROWID(OfferCriteria)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF LOCKED bOfferCriteria OR NOT AVAILABLE bOfferCriteria THEN DO:
          PUT STREAM slog UNFORMATTED
              Offer.Offer CHR(9)
              "OfferCriteria record is locked or not available" SKIP.
          NEXT EACH_OFFER.
       END. /* IF LOCKED bOfferCriteria OR NOT AVAILABLE bOfferCriteria */
       liTestCount = liTestCount + 1.
       CREATE bbOfferCriteria.
       BUFFER-COPY bOfferCriteria EXCEPT OfferCriteriaId TO bbOfferCriteria.
       ASSIGN bOfferCriteria.EndStamp         = ldeCurrStamp
              bbOfferCriteria.IncludedValue   = "CONTRD"
              bbOfferCriteria.OfferCriteriaID = liLastCriSeq
              bbOfferCriteria.BeginStamp      = (ldeCurrStamp + 0.00001).
       PUT STREAM slog UNFORMATTED
           Offer.Offer CHR(9)
           "OfferCriteria record is successfully updated to CLITYPE=CONTRD" SKIP.
       FIND CURRENT bOfferCriteria NO-LOCK NO-ERROR.
       FIND CURRENT bbOfferCriteria NO-LOCK NO-ERROR.
    END. /* DO TRANSACTION: */
END. /* FOR EACH Offer WHERE */

Message liTestCount view-as alert-box.

PROCEDURE pCreateOfferItem:
    DEFINE INPUT PARAMETER icBundleName   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icOfferID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icItemType     AS CHARACTER NO-UNDO.

    DEFINE VARIABLE liLastSeq             AS INTEGER   NO-UNDO.

    DEFINE BUFFER bOfferItem              FOR OfferItem.

    FOR EACH bOfferItem NO-LOCK BY bOfferItem.OfferItemID DESC:
       liLastSeq = bOfferItem.OfferItemID.
       LEAVE.
    END. /* FOR EACH bOfferItem NO-LOCK BY bOfferItem.OfferItemID */

    IF NOT CAN-FIND(FIRST OfferItem WHERE
                          OfferItem.Brand    = gcBrand      AND
                          OfferItem.Offer    = icOfferID    AND
                          OfferItem.ItemType = icItemType   AND
                          OfferItem.ItemKey  = icBundleName AND
                          OfferItem.BeginStamp <= ldeCurrStamp AND
                          OfferItem.EndStamp   >= ldeCurrStamp) THEN DO:
       CREATE OfferItem.
       ASSIGN OfferItem.Brand        = gcBrand
              OfferItem.Offer        = icOfferID
              OfferItem.OfferItemID  = (liLastSeq + 1)
              OfferItem.BeginStamp   = (ldeCurrStamp + 0.00001)
              OfferItem.ItemType     = icItemType
              OfferItem.ItemKey      = icBundleName
              OfferItem.Amount       = 0.0
              OfferItem.VatIncl      = YES
              OfferItem.DispInUI     = YES
              OfferItem.DispOnInvoice = YES NO-ERROR.
              OfferItem.EndStamp     = fMake2DT(12/31/2049,86399) NO-ERROR.
       IF ERROR-STATUS:ERROR THEN
          PUT STREAM slog UNFORMATTED
              icOfferID CHR(9)
              "Error occurred during adding offeritem for " +
              icItemType + "=" + icBundleName SKIP.
    END. /* IF NOT CAN-FIND(FIRST OfferItem WHERE */

END PROCEDURE. /* PROCEDURE pCreateOfferItem: */

OUTPUT STREAM slog CLOSE.

