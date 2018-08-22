&IF "{&UPSELLCOUNT_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE UPSELLCOUNT_I YES

{Syst/commali.i}
{Syst/tmsconst.i}

FUNCTION fGetUpSellCount RETURNS INT
   (INPUT icDCEvent AS CHAR,
    INPUT iiMsSeq AS INT,
    INPUT iiCustnum AS INT,
    OUTPUT ocError AS CHAR):
   
   DEF VAR liUpSellCount AS INT NO-UNDO. 
   DEF VAR ldeMonthBegin AS DEC NO-UNDO.
   DEF VAR ldeMonthEnd   AS DEC NO-UNDO.
   DEF VAR ldTS          AS DEC NO-UNDO.

   DEF BUFFER bServiceLimit  FOR ServiceLimit.
   DEF BUFFER bMServiceLimit FOR MServiceLimit.
   DEF BUFFER DayCampaign    FOR DayCampaign.

   ASSIGN ldeMonthBegin = Func.Common:mHMS2TS(DATE(MONTH(TODAY),1,YEAR(TODAY)),"00:00:00")
          ldeMonthEnd   = Func.Common:mHMS2TS(Func.Common:mLastDayOfMonth(TODAY),"23:59:59")
          ldTS          = Func.Common:mMakeTS().

   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = icDCEvent,
       FIRST ServiceLimit NO-LOCK WHERE 
             ServiceLimit.GroupCode  = DayCampaign.DCEvent AND 
             ServiceLimit.ValidFrom <= TODAY  AND 
             ServiceLimit.ValidTo   >= TODAY:

        IF icDCEvent BEGINS "DSS" THEN DO:

           FOR EACH MServiceLPool WHERE
                    MServiceLPool.Custnum = iiCustnum AND
                    MServiceLPool.SLSeq   = ServiceLimit.SLSeq AND
                    MServiceLPool.EndTS  <= ldeMonthEnd AND
                    MServiceLPool.FromTS >= ldeMonthBegin NO-LOCK:

               liUpSellCount = liUpSellCount + 1.
               /* don't change error text (ext.upsell_bundles_get) */
               IF liUpSellCount >= DayCampaign.InstanceLimit THEN DO:
                  ocError = SUBST("Upsell limit exceeded",
                                   DayCampaign.InstanceLimit).
                  LEAVE.
               END.
           END. /* FOR EACH MServiceLPool WHERE */
        END.
        ELSE IF icDCEvent = {&TARJ_UPSELL} THEN DO:
           FOR EACH MServiceLimit WHERE 
                    MServiceLimit.MSSeq    = iiMsSeq AND
                    MServiceLimit.DialType = ServiceLimit.DialType AND
                    MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND 
                    MServiceLimit.EndTS <= ldeMonthEnd AND
                    MServiceLimit.EndTS > ldeMonthBegin AND
                    MServiceLimit.FromTs >= ldeMonthBegin:

              liUpSellCount = liUpSellCount + 1.
           END.
        END.
        ELSE IF icDCEvent = "TARJ7_UPSELL" THEN DO:
           FOR FIRST bMServiceLimit WHERE
                     bMServiceLimit.MSSeq    = iiMsSeq AND
                     bMServiceLimit.DialType = ServiceLimit.DialType AND
                     bMServiceLimit.SlSeq    = ServiceLimit.SlSeq AND 
                     bMServiceLimit.EndTS   >= ldTS AND
                     bMServiceLimit.FromTS  <= ldTS NO-LOCK:

              FOR EACH MServiceLPool WHERE 
                       MServiceLPool.MSSeq   = iiMsSeq AND
                       MServiceLPool.SlSeq   = ServiceLimit.SlSeq AND 
                       MServiceLPool.EndTS  <= bMServiceLimit.EndTS AND
                       MServiceLPool.FromTs >= bMServiceLimit.FromTS NO-LOCK:
                 liUpSellCount = liUpSellCount + 1.
              END.
           END.
        END.
        ELSE
           FOR EACH MServiceLPool WHERE
                    MServiceLPool.MsSeq = iiMsSeq AND
                    MServiceLPool.SLSeq = ServiceLimit.SLSeq AND
                    MServiceLPool.EndTS <= ldeMonthEnd AND
                    MServiceLPool.FromTS >= ldeMonthBegin NO-LOCK:

               liUpSellCount = liUpSellCount + 1.
               /* don't change error text (ext.upsell_bundles_get) */
               IF liUpSellCount >= DayCampaign.InstanceLimit THEN DO:
                  ocError = SUBST("Upsell limit exceeded",
                                   DayCampaign.InstanceLimit).
                  LEAVE.
               END.
           END. /* FOR EACH MServiceLPool WHERE */
   END.
   
   RETURN liUpSellCount.

END FUNCTION.

&ENDIF