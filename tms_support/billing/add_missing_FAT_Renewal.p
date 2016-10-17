{commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{tmsconst.i}
{timestamp.i}

DEF VAR ldeAlreadyFatAmt AS DEC  NO-UNDO.
DEF VAR ldeBundleFromTS  AS DEC  NO-UNDO.
DEF VAR ldeBundleENDTS   AS DEC  NO-UNDO.
DEF VAR ldeFirstMonthFee AS DEC  NO-UNDO.
DEF VAR lcftgrp          AS CHAR NO-UNDO.
DEF VAR lcnewftgrp       AS CHAR NO-UNDO.
DEF VAR lcError          AS CHAR NO-UNDO.
DEF VAR lcBundleId       AS CHAR NO-UNDO.
DEF VAR icount           AS INT  NO-UNDO.
DEF VAR llsimulate       AS LOG  NO-UNDO.
DEF VAR llFATAllowed     AS LOG  NO-UNDO.
DEF VAR ldBundleDate     AS DATE NO-UNDO.
DEF VAR liBundleTime     AS INT  NO-UNDO.
DEF VAR ldOrderDate      AS DATE NO-UNDO.
DEF VAR liOrderTime      AS INT  NO-UNDO.
DEF VAR liperiod         AS INT  NO-UNDO.

OUTPUT TO "/apps/yoigo/tms_support/billing/add_missing_renewal_FAT.xls".

PUT UNFORMATTED "Order Id"                           CHR(9)
                "MSISDN"                             CHR(9)
                "Order CrStamp"                      CHR(9)
                "Bundle Id"                          CHR(9)
                "Bundle FromStamp"                   CHR(9)
                "Bundle EndStamp"                    CHR(9)
                "Ex. FAT Grp"                        CHR(9)
                "Ex. FAT Amt Left"                   CHR(9)
                "Is FAT Allowed?"                    CHR(9)
                "New FAT Grp"                        CHR(9)
                "Remark"                             SKIP.

llsimulate = false.

for each Order where
         Order.Brand = "1" and
         Order.OrderChannel begins "renewal" AND
         Order.CrStamp >= 20110701.00000     AND
         NOT Order.PayType NO-LOCK:

   /* Only check delivered orders */
   FIND FIRST OrderTimeStamp WHERE
              OrderTimeStamp.Brand   = "1"   AND
              OrderTimeStamp.OrderID = Order.OrderID AND
              OrderTimeStamp.RowType = 2 NO-LOCK NO-ERROR.
   IF NOT AVAIL OrderTimeStamp THEN NEXT.

   IF order.cli = "600208651" THEN NEXT.

   ASSIGN ldeAlreadyFatAmt = 0
          lcftgrp          = ""
          lcBundleId       = ""
          ldeBundleFromTS  = 0
          ldeBundleENDTS   = 0
          lcnewftgrp       = ""
          lcError          = ""
          llFATAllowed     = FALSE
          ldBundleDate     = ?
          liBundleTime     = 0
          ldOrderDate      = ?
          liOrderTime      = 0.

   FIND FIRST OrderAction WHERE
              OrderAction.Brand = "1" AND
              OrderAction.OrderId = Order.OrderID AND
              OrderAction.ItemType = "BundleItem" AND
              OrderAction.ItemKey BEGINS "MDUB" NO-LOCK NO-ERROR.
   IF NOT AVAIL OrderAction THEN
      FIND OfferItem WHERE
           OfferItem.Brand = "1" AND
           OfferItem.Offer = Order.Offer AND
           OfferItem.ItemType = "BundleItem" AND
           OfferItem.ItemKey BEGINS "MDUB" AND
           OfferItem.EndStamp >= Order.CrStamp AND
           OfferItem.BeginStamp <= Order.CrStamp NO-LOCK NO-ERROR.
   IF AVAIL OfferItem THEN lcBundleId = OfferItem.ItemKey.
   ELSE IF AVAIL OrderAction THEN lcBundleId = OrderAction.ItemKey.

   IF lcBundleId > "" THEN DO:
      FOR FIRST ServiceLimit NO-LOCK WHERE
                ServiceLimit.GroupCode = lcBundleId,
          FIRST MServiceLimit NO-LOCK WHERE
                MServiceLimit.MsSeq   = Order.MsSeq AND
                MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
                MServiceLimit.EndTS   > Order.CrStamp:
          ASSIGN ldeBundleFromTS = MServiceLimit.FromTS
                 ldeBundleENDTS  = MServiceLimit.EndTS.
      END. /* FOR FIRST ServiceLimit NO-LOCK WHERE */

      IF ldeBundleFromTS = Order.CrStamp THEN
         llFATAllowed = TRUE.
      ELSE IF ldeBundleFromTS > 0 THEN DO:
         fSplitTS(Order.CrStamp, ldOrderDate, liOrderTime).
         fSplitTS(ldeBundleFromTS, ldBundleDate, liBundleTime).
         IF ldOrderDate = ldBundleDate THEN llFATAllowed = TRUE.
      END.

      IF lcBundleId = "MDUB" THEN lcnewftgrp = "BONO8CPACT".
      ELSE IF lcBundleId = "MDUB2" THEN lcnewftgrp = "BONO15CPACT".

      FOR EACH fatime NO-LOCK where
               fatime.brand = "1" and 
               fatime.msseq = order.msseq and
               fatime.cli   = order.cli and
               fatime.ftgrp = lcnewftgrp and
              (fatime.period = 201107 OR fatime.period = 201108)
          use-index mobsub:
          ASSIGN ldeAlreadyFatAmt = ldeAlreadyFatAmt + fatime.Amt
                 lcftgrp = fatime.ftgrp
                 llFATAllowed = FALSE.
      END. /* FOR EACH fatime NO-LOCK where */

      icount = icount + 1.
      status default STRING(icount).

      IF NOT llsimulate AND llFATAllowed THEN DO:
         RUN creafat.p (Order.CustNum,
                        Order.MsSeq,
                        lcnewftgrp,
                        "",
                        "",
                        ?, /* amount */
                        0, /* percent */
                        ?, /* vat incl. */
                        201108,
                        999999,
                        OUTPUT lcError).

         IF lcError = "" THEN lcError = "FAT is added successfully".
      END. /* IF llsimulate AND llFATAllowed THEN DO: */

      PUT UNFORMATTED
                STRING(Order.OrderID)                CHR(9)
                Order.CLI                            CHR(9)
                STRING(Order.CrStamp)                CHR(9)
                lcBundleId                           CHR(9)
                STRING(ldeBundleFromTS)              CHR(9)
                STRING(ldeBundleENDTS)               CHR(9)
                lcftgrp                              CHR(9)
                STRING(ldeAlreadyFatAmt)             CHR(9)
                STRING(llFATAllowed)                 CHR(9)
                lcnewftgrp                           CHR(9)
                lcError                              SKIP.
   END.
END.

OUTPUT CLOSE.
disp icount.





