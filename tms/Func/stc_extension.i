&IF "{&STC_EXTENSION_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE STC_EXTENSION_I YES

{commali.i}
{penaltyfee.i}
{fcustpl.i}
{tmsconst.i}
{cparam2.i}

FUNCTION fCanExtendTerminalContract RETURNS LOGICAL
   (BUFFER Mobsub FOR MobSub,
    INPUT  idaSTCDate AS DATE,
    INPUT  icNewType AS CHAR,
    OUTPUT ocError      AS CHAR):
   
   DEF VAR liCount AS INT NO-UNDO. 
   DEF VAR ldtFrom AS DATETIME NO-UNDO.
   DEF VAR ldtTo AS DATETIME NO-UNDO.
   DEF VAR liMonths AS INT NO-UNDO. 
   DEF VAR lcFusionTariffs AS CHAR NO-UNDO. 
   DEF VAR liMaxMonths AS INT NO-UNDO. 
     
   lcFusionTariffs = fCParamC("FUSION_SUBS_TYPE").

   DEF BUFFER DCCLI FOR DCCLI.
   DEF BUFFER DayCampaign FOR DayCampaign.

   CONTRACT_LOOP:
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.MsSeq = MobSub.MsSeq AND
            DCCLI.ValidTo   >= idaSTCDate AND
            DCCLI.ValidFrom <= idaSTCDate AND
            DCCLI.DCEvent BEGINS "TERM" AND
            DCCLI.CreateFees = TRUE,
      FIRST DayCampaign WHERE
            DayCampaign.Brand = gcBrand AND
            DayCampaign.DCEvent = DCCLI.DCEvent AND
            DayCampaign.DCType = {&DCTYPE_DISCOUNT} AND
            DayCampaign.TermFeeModel NE "" AND
            DayCampaign.TermFeeCalc > 0 NO-LOCK BY DCCLI.ValidFrom DESC:
      
      liCount = liCount + 1.

      IF liCount > 1 THEN LEAVE CONTRACT_LOOP.

      ASSIGN
         ldtFrom = DATETIME(idaSTCDate,0)
         ldtTo = DATETIME(DCCLI.ValidTo,0)
         liMonths = INTERVAL(ldtTo,ldtFrom,"months") + 1
         liMaxMonths = (IF LOOKUP(icNewType,lcFusionTariffs) > 0
                        THEN 36 ELSE 30).
      
      IF liMonths + 12 > liMaxMonths THEN DO:
         ocError = SUBST("Contract extension not allowed. Extended contract length is more than &1 months.",liMaxMonths).
         RETURN FALSE.
      END.
   END.

   RETURN TRUE.

END.

&ENDIF
