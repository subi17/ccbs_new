

/* return the amount of charge or compensation for a mobsub */
{Syst/commali.i}
{Func/timestamp.i}
{Syst/tmsconst.i}

FUNCTION fMonthLoaded RETURN DECIMAL 
   (icSource AS CHAR, icCLI AS CHAR, ilPayType AS LOG):

   DEF VAR ldTS1     AS DEC  NO-UNDO.
   DEF VAR ldTS2     AS DEC  NO-UNDO.
   DEF VAR ldLoaded  AS DEC  NO-UNDO.
  
   fMonthlyStamps(TODAY,
                  OUTPUT ldTS1, 
                  OUTPUT ldTS2).
   
   ldLoaded = 0.

   IF ilPaytype THEN DO:
      /* prepaid --------------------------------------------------------------------*/
       FOR EACH PrePaidRequest WHERE
            PrePaidRequest.Brand      = gcBrand   AND
            PrePaidRequest.CLI        = icCLI    AND
            PrePaidRequest.Source     = icSource AND
            PrePaidRequest.PPStatus   < 3        AND
            PrePaidRequest.TSRequest >= ldTS1    AND
            PrePaidRequest.TSRequest <= ldTS2   NO-LOCK:

         ldLoaded = ldLoaded + PrePaidRequest.TopUpAmt / 100.

      END.
      
      ldLoaded = -1 * ldLoaded.
     
   END.
   ELSE DO:
      /* postpaid --------------------------------------------------------------*/

      FOR EACH MsRequest WHERE
            MsRequest.Brand      = gcBrand  AND
            MsRequest.ReqType    =  {&REQTYPE_CHARGE_AND_COMPENSATION} AND
            MsRequest.CLI        = icCLI    AND           
            MsRequest.ReqStatus <> 3 AND
            MsRequest.ActStamp >= ldTS1   AND
            MsRequest.ActStamp <= ldTS2 NO-LOCK :

           IF  (icSource = "CHARGE" AND MsRequest.ReqDParam1 < 0 ) OR
               ( icSource = "COMP" AND MsRequest.ReqDParam1 > 0  )
            THEN NEXT.

           ldLoaded = ldLoaded + MsRequest.ReqDParam1.
      
       END.

   END.


   RETURN ldLoaded.

END FUNCTION.

/* RPCs use this. Do not change error texts */
FUNCTION fCheckChargeLimits RETURNS CHAR
(  icCLI AS CHARACTER,
   ilPayType AS LOGICAL,
   ideCharge AS DEC,
   ideChargeLimit AS DEC):

   DEF VAR ldeLoaded AS DECIMAL NO-UNDO.
   DEF VAR ldeCurrBal AS DECIMAL NO-UNDO.

   ldeLoaded = fMonthLoaded(
               "CHARGE",
               icCLI,
               ilPayType).

   IF ideCharge + ldeLoaded > ideChargeLimit THEN
      RETURN "Charge exceeds monthly limit".

   IF ilPaytype = TRUE THEN DO:
      ldeCurrBal = 0.
      RUN Gwy/balancequery.p(icCLI).
      ldeCurrBal = INT(RETURN-VALUE) / 100 NO-ERROR.
      IF ldeCurrBal < ideCharge THEN RETURN "Charge exceeds balance".
   END.

   RETURN "".

END FUNCTION. 
