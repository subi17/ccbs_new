/* ----------------------------------------------------------------------
  MODULE .......: offer_mig_script.p
  TASK .........: Offer IPL8 Migration Script to add bundle items to support
  APPLICATION ..: 
  AUTHOR .......: Vikas
  CREATED ......: 12/09/2011
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "Qvantel".
{timestamp.i}

DEFINE VARIABLE liTestCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE liLastSeq       AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeCurrStamp    AS DECIMAL   NO-UNDO.

DEFINE BUFFER bOfferItem    FOR OfferItem.
DEFINE BUFFER bbOfferItem   FOR OfferItem.
DEFINE BUFFER bGetOfferItem FOR OfferItem.

DEFINE STREAM slog.

ldeCurrStamp = fMakeTS().

OUTPUT STREAM slog TO "/apps/yoigo/tms_support/testing/offer_ipl8_mig.xls" append.

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
         OfferCriteria.EndStamp    >= ldeCurrStamp  AND
         OfferCriteria.IncludedValue = "CONTRD" NO-LOCK:

    FIND FIRST OfferItem WHERE
               OfferItem.Brand    = gcBrand      AND
               OfferItem.Offer    = Offer.Offer  AND
               OfferItem.ItemType = "BundleItem" AND
               OfferItem.ItemKey  = "MDUB"       AND
               OfferItem.BeginStamp <= ldeCurrStamp AND
               OfferItem.EndStamp   >= ldeCurrStamp NO-LOCK NO-ERROR.
    IF NOT AVAIL OfferItem THEN NEXT EACH_OFFER.

    FOR EACH bGetOfferItem NO-LOCK BY bGetOfferItem.OfferItemID DESC:
       liLastSeq = bGetOfferItem.OfferItemID.
       LEAVE.
    END. /* FOR EACH bGetOfferItem NO-LOCK BY bGetOfferItem.OfferItemID */

    DO TRANSACTION:
       FIND bOfferItem WHERE
            ROWID(bOfferItem) = ROWID(OfferItem)
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       IF LOCKED bOfferItem OR NOT AVAILABLE bOfferItem THEN DO:
          PUT STREAM slog UNFORMATTED
              Offer.Offer CHR(9)
              "OfferItem record is locked or not available" SKIP.
          NEXT EACH_OFFER.
       END. /* IF LOCKED bOfferCriteria OR NOT AVAILABLE bOfferCriteria */
       liTestCount = liTestCount + 1.
       CREATE bbOfferItem.
       BUFFER-COPY bOfferItem EXCEPT OfferItemID TO bbOfferItem.
       ASSIGN bOfferItem.EndStamp     = ldeCurrStamp
              bbOfferItem.ItemKey     = "CONTD3"
              bbOfferItem.OfferItemID = (liLastSeq + 1)
              bbOfferItem.BeginStamp  = (ldeCurrStamp + 0.00001).
       PUT STREAM slog UNFORMATTED
           Offer.Offer CHR(9)
           "OfferItem record is successfully updated to BundleItem=CONTD3" SKIP.
       FIND CURRENT bbOfferItem NO-LOCK NO-ERROR.
       FIND CURRENT bbOfferItem NO-LOCK NO-ERROR.
    END. /* DO TRANSACTION: */
END. /* FOR EACH Offer WHERE */

Message liTestCount view-as alert-box.

OUTPUT STREAM slog CLOSE.

