/* mobcdr_rate.i 

   changes:        10.01.07/aam fInvSeq(); MsSeq based invseq 
*/
   
{Rate/iprange.i}
{Func/msisdn_prefix.i}
{Func/invseq.i}

FUNCTION fGetIPTariffZone RETURNS CHAR
   (INPUT pcIP AS CHAR):

   DEF VAR liIP AS DEC.
   
   IF pcIP EQ "" THEN RETURN "".
   liIp = fIpToLong(pcIP).
   IF liIp EQ -1 THEN RETURN "".

   FIND FIRST ttIPRange NO-LOCK WHERE
              ttIPRange.FirstIP <= liIp AND
              ttIPRange.LastIP >= liIp NO-ERROR.
   
   IF AVAIL ttIPRange THEN RETURN "EU".
   ELSE RETURN "NONEU".
END.

DEF VAR oierrorCode AS I NO-UNDO.
FUNCTION fAnalBsub RETURNS LOGICAL

  (INPUT   cust-a     AS INTEGER,   
   INPUT   b_sub      AS CHARACTER,
   INPUT   b_type     AS INTEGER,
   OUTPUT  b_dest     AS CHARACTER, 
   OUTPUT  r_dest     AS CHARACTER,
   OUTPUT  b_ccn      AS INTEGER,   
   OUTPUT  b_dg-code  AS CHARACTER,
   OUTPUT  b_foc      AS LOGICAL,   
   OUTPUT  b_PNP      AS LOGICAL,
   OUTPUT  b_prodcode AS CHARACTER, 
   OUTPUT  b_calltype AS INTEGER,
   OUTPUT  oiErrCode  AS INT):

   b_calltype = ?.

   DEF VAR mod_bsub                  AS C    NO-UNDO.
   DEF VAR b                         AS I    NO-UNDO.
   DEF VAR dest_recid                AS RE   NO-UNDO INIT ?.
   DEF VAR lcbnet                    AS CHAR NO-UNDO .
   DEF VAR liOrigBType               AS INT  NO-UNDO.
   DEF VAR lcRoamGPRSZone            AS CHAR NO-UNDO. 
   DEF VAR lcBRoamZone               AS CHAR NO-UNDO.
   DEF VAR lcARoamZone               AS CHAR NO-UNDO.

   ASSIGN
      b_foc       = FALSE  
      mod_bsub    = trim(b_sub)
      liOrigBType = b_type
      dest_recid  = ?
      lcbnet      = "".

   CASE ttCall.SpoCMT:
      WHEN 81   THEN b_CallType =  4.
      WHEN 1081 THEN DO:
         IF b_type EQ 4 THEN
            b_CallType =  50. /* fixed to fixed */
         ELSE
            b_CallType =  1. /* fixed to mobile */
      END.

      WHEN 1002 THEN b_CallType =  1.
      WHEN 1008 THEN b_CallType =  1.
      WHEN 1063 THEN b_CallType =  1.
      WHEN 1064 THEN b_CallType =  1.
      WHEN 1066 THEN DO: 
         IF ttCall.DateSt >= 05/09/2018 THEN DO: /* temporary 9.5.2018 */
            /* YDR-2876 duration conditions changed 05/2018 */
            IF ttCall.BillDur <= 20 THEN 
                       b_callType = 23.
            ELSE       b_callType = 24.
            /* Temporary until network starts to cut CDRs / YTS-12982 */
            IF ttCall.BillDur > 620 AND 
               ttCall.GsmBnr BEGINS "118" THEN ttCall.BillDur = 620.
         END.
         ELSE DO: /* temporary, old functionality */
            b_CallType =  1.
            IF ttCall.BillDur <= 11 THEN b_callType = 23.
            ELSE b_callType = 24.
         END. /* temporary */
      END.
      WHEN 1074 THEN b_CallType =  1.
      WHEN 1075 THEN b_CallType =  1.
      WHEN 1    THEN b_CallType =  4.
      WHEN 30   THEN b_CallType =  12.
      WHEN 51   THEN b_CallType =  5.
      WHEN 53   THEN b_CallType =  5.
      WHEN 54   THEN b_CallType =  5. 
      WHEN 61   THEN b_CallType =  5.
      WHEN 71   THEN b_CallType =  5.
      WHEN 73   THEN b_CallType =  14.
      WHEN 78   THEN b_callType =  13.
      WHEN 41   THEN DO:
                     b_CallType =  6.
      END.     
      WHEN 90   THEN b_CallType =  7. 
      WHEN 91   THEN b_CallType =  7. 
      WHEN 92   THEN b_CallType =  7.
      WHEN 93   THEN b_CallType =  7.
      WHEN 94   THEN b_CallType =  10.
      WHEN 95   THEN b_CallType =  10.
      WHEN 96   THEN b_CallType =  10.
      WHEN 97   THEN b_CallType =  10.
      WHEN 105  THEN b_CallType =  10.
      WHEN 106  THEN b_CallType =  10.
      WHEN 66 THEN DO:
         IF ttCall.DateSt >= 05/09/2018 THEN DO: /* temporary 9.5.2018 */
            /* YDR-2876 duration conditions changed */
            IF ttCall.BillDur <= 20 THEN 
                        b_callType =  20.
            ELSE        b_callType =  21.
            /* Temporary until network starts to cut CDRs / YTS-12982 */
            IF ttCall.BillDur > 620 AND
               ttCall.GsmBnr BEGINS "118" THEN ttCall.BillDur = 620.
         END.   
         ELSE DO: /* temporary, old functionality */
             IF       ttCall.BillDur <= 11     THEN b_callType = 20.
             ELSE  IF ttCall.BillDur <= 70 OR
                      ttCall.DateSt >= 10/21/9 THEN b_callType = 21.
             ELSE                                   b_callType = 22.
         END. /* temporary */
      END.
      OTHERWISE      b_callType =   4.
   END.

   /*****************
    Special handling  for bdest (= variable mod_bsub)
   *****************/

   IF ttCall.RoutingNumber ne "" AND Mod_bsub NE "622FF" THEN DO:
   
      {Mm/mnpchk.i}
   
      IF lcbnet ne "" THEN mod_bsub = lcbnet.
   END.

   CASE ttCall.SpoCMT:
   
      WHEN 2 THEN DO:
         IF TENANT-NAME("common") EQ {&TENANT_YOIGO} THEN
            mod_bsub  = "INTERNATIONAL".
      END. 
      WHEN 1002 THEN mod_bsub  = "INTERNATIONAL".
      WHEN 3  THEN ASSIGN Mod_bsub  = "ROAMINT"   b_type = 0.
      WHEN 4  THEN ASSIGN Mod_bsub  = "ROAMLOCAL" b_type = 1.
      WHEN 7  THEN ASSIGN Mod_bsub  = "RT" b_type = 0.
      WHEN 51 THEN DO:     
         IF b_type = 1 THEN DO:
            IF TENANT-NAME("common") EQ {&TENANT_YOIGO} THEN
               mod_bsub = "INTERNATIONAL".
         END.
         ELSE IF b_type = 4 THEN b_type = 4.
         ELSE IF b_type = 7 THEN b_type = 7. /* YOT-1684 */
         ELSE IF b_type = 5 AND
            TENANT-NAME("common") EQ {&TENANT_YOIGO} THEN DO:

            IF ttCall.IMSI2 BEGINS "21404" 
               THEN Mod_Bsub = "YOIGO". /*YDR-1499*/
            ELSE IF b_type = 5 AND ttCall.IMSI2 BEGINS "214" 
               THEN Mod_Bsub = "OTHER". /*YDR-1499*/

         END.
         ELSE b_type = 5.
      END.
      WHEN 53 THEN ASSIGN Mod_bsub  = "ROAMSMS"  b_type = 0.
      WHEN 54 THEN DO:
         /* YOT-1793 */
         IF b_type EQ 1 THEN DO:
            fGetBRoamZone
              (INPUT  ttCall.Gsmbnr,
               OUTPUT lcBRoamZone).
            IF lcBRoamZone EQ "ROAM_EU" THEN
               ASSIGN Mod_bsub = "ROAMSMS_EU" b_type = 0.
            ELSE Mod_bsub = "ROAMSMS_EUINT".
         END.
         ELSE ASSIGN Mod_bsub  = "ROAMSMS_EU" b_type = 0.
      END.
      WHEN 93 THEN Mod_bsub  = "GPRS".
      WHEN 90 THEN DO:
         lcRoamGPRSZone = fGetIPTariffZone(lcSGSNAddress).
         IF lcRoamGPRSZone EQ "EU" THEN mod_bsub = "ROAMGPRS_EU".
         ELSE mod_bsub = "ROAMGPRS".
      END.
      WHEN 92 THEN mod_bsub = "ROGPRS".
      WHEN 91 THEN mod_bsub = "ROGPRS_EU".
      WHEN 78 THEN DO:
         ASSIGN 
            Mod_bsub      = "WAPVAS" 
            ttCall.Gsmbnr = "".
      END.
      WHEN 72 THEN Mod_bsub  = "VAS".
      WHEN {&GB_CCN} THEN Mod_bsub = "GOOGLE".
      WHEN 41 THEN DO:
         IF ttCall.Btype = 1 AND 
            ttCall.Gsmbnr begins ttcall.bpref THEN
             mod_bsub = Substring(ttcall.gsmbnr,length(ttcall.bpref) + 1).
         ELSE IF ttCall.Btype = 1 THEN mod_bsub = ttcall.gsmbnr.
      
      END.      

      WHEN 95 THEN mod_bsub = "ROAMMMSMO_EU".
      WHEN 96 THEN mod_bsub = "ROAMMMSMT_EU".
      WHEN 105 THEN mod_bsub = "ROAMMMSMO".
      WHEN 106 THEN mod_bsub = "ROAMMMSMT".
                           
      WHEN 97 THEN DO:
         mod_bsub  = "INTERNATIONAL".
      END. 
   END.
 
   /* YTS-11600 */
   IF ttCall.SpoCmt EQ 104 OR /* 94+b-type=98=>104 */
     (ttCall.SpoCmt EQ 95 AND ttCall.Btype EQ 98) OR
     (ttCall.SpoCmt EQ 105 AND ttCall.Btype EQ 98) THEN DO:
      IF ttCall.Gsmbnr ne "-" THEN ttCall.xsub = ttCall.Gsmbnr.
      ASSIGN
         ttCall.Gsmbnr = "-"
         mod_bsub      = "EMAIL".
   END.
   
   IF ttCall.SpoCMT  = 3 OR 
      ttCall.SpoCMT  = 4 OR
      ttCall.SpoCMT  = 7 THEN DO:

      lcARoamZone = "".
      
      /* voice MT comes in MM format where NetworkOwner is not available,
         so use MSRN */
      IF ttCall.SpoCMT = 7 AND LOOKUP(ttCall.MSCID,"TAP3,NRTRDE") = 0 THEN DO:
         IF lcMSRN = "" THEN 
            lcMSRN = fGetMcdrDtlValue(ttCall.Datest,
                                      ttCall.Dtlseq,
                                      "MSRN").
         lcServiceName = lcMSRN.
                                      
         fGetRoamZones_MSRN(INPUT  ttCall.Gsmbnr,
                            INPUT  lcMSRN,
                            OUTPUT lcARoamZone,
                            OUTPUT lcBRoamZone).
      END.
                   
      ELSE DO:
         IF lcNetworkowner = ""
         THEN lcNetworkowner = fGetMcdrDtlValue(ttCall.Datest,
                                                ttCall.Dtlseq,
                                                "Network Owner").
         IF ttCall.SpoCMT = 7
         THEN lcServiceName = lcNetworkowner.

         fGetRoamZones
           (INPUT  ttCall.Gsmbnr,
            INPUT  lcNetworkowner,
            OUTPUT lcARoamZone,
            OUTPUT lcBRoamZone).
      END.

      IF lcaRoamZone = "ROAM_EU" THEN DO:
         /* CC=4 are always local calls even if b-type = 1 */
         IF ttCall.Spocmt EQ 4 OR
            (lcBRoamZone  = "ROAM_EU" OR liOrigBType NE 1)
            THEN Mod_bsub = "ROAM_EU".
            ELSE Mod_bsub = "ROAM_EUINT".
      END.
      ELSE DO: 
         IF ttCall.Spocmt = 4 OR 
            ttCall.Spocmt = 7 THEN Mod_bsub = Mod_bsub.
         ELSE IF liOrigBType NE 1 THEN mod_bsub = "ROAM_INTEU".
         ELSE IF lcBRoamZone = "ROAM_EU" THEN Mod_bsub = "ROAM_INTEU" .
         ELSE Mod_bsub = "ROAMINT" .
      END.

      IF Mod_bsub =  "ROAM_EU" AND ttCall.gsmbnr = "633800800" then
      mod_bsub = ttCall.Gsmbnr.
      
      IF mod_bsub EQ "ROAM_EU" AND
         liOrigBType EQ 1 AND
         (ttCall.Spocmt EQ 3 OR
          ttCall.Spocmt EQ 4) THEN DO:

         FIND FIRST BDest NO-LOCK WHERE
                    BDest.Brand = lcRateBrand AND
                    BDest.BDest = "ROAM_EU" AND
                    BDest.ToDate >= ttCall.DateSt AND
                    BDest.FromDate <= ttCall.DateSt NO-ERROR.
         
         IF ttCall.Gsmbnr > "" AND AVAIL BDest THEN
         DO b = LENGTH(ttCall.Gsmbnr) TO 1 BY -1:

            FIND FIRST BDestTrans NO-LOCK  WHERE
                       BDestTrans.BDestID = BDest.BDestID AND
                       BDestTrans.TranslateNumber = SUBSTRING(ttCall.Gsmbnr,1,b) AND
                       BDestTrans.Todate >= ttCall.DateST AND
                       BDestTrans.FromDate <= ttCall.DateST NO-ERROR.
            IF NOT AVAIL BDestTrans THEN NEXT.

            IF BDestTrans.MinLength > 0 AND
               LENGTH(ttCall.Gsmbnr) < BDestTrans.MinLength THEN NEXT.

            IF BDestTrans.MaxLength > 0 AND
               LENGTH(ttCall.Gsmbnr) > BDestTrans.MaxLength THEN NEXT.

            mod_bsub = BDestTrans.BDest.
            LEAVE.
         END.
      END.
      
      /* destination type is not used with roaming */
      FIND FIRST BDest WHERE 
                 BDest.Brand = lcRateBrand AND 
                 BDest.BDest = mod_bsub AND
                 BDest.ToDate >= ttCall.DateSt AND
                 BDest.FromDate <= ttCall.DateSt NO-LOCK NO-ERROR.

      IF AVAIL Bdest THEN ASSIGN
         b_dest     = (IF BDest.RateBDest NE "" THEN Bdest.RateBDest
                       ELSE Bdest.Bdest)
         r_dest     = Bdest.Bdest
         b_ccn      = Bdest.CCN   
         b_dg-code  = BDest.DiscGroup
         ttCall.CaseType = "R".
                           
      LEAVE.
   END.

   /*************************************
   * This calling customer  did EITHER  *     
   * NOT have any 'own group' nos.      *
   *                                    *
   * OR it DID ->                       *
   *                                    *
   * b_sub was overridden due TO OG     *
   * Thus analyse B-sub from right      *
   * TO left; gradually                 *
   *************************************/
   IF b_type = 1 AND 
      ttCall.SpoCMT NE 51 AND
      ttCall.SpoCMT NE 54 AND
      ttCall.SpoCMT NE 95 AND
      ttCall.SpoCMT NE 105 THEN DO:
   
      DO b = LENGTH(b_sub) TO 1 BY -1:
         IF CAN-FIND(FIRST BDest WHERE
                     BDest.Brand    = msowner.Brand     AND
                     BDest.BDest    = SUBSTR(b_sub,1,b) AND
                     BDest.DestType = b_Type            AND
                     BDest.ToDate  >= ttCall.DateSt) THEN DO:
                  
            FIND FIRST BDest WHERE
               BDest.Brand      = msowner.Brand     AND
               BDest.BDest      = SUBSTR(b_sub,1,b) AND
               BDest.DestType   = b_Type            AND
               BDest.ToDate   >= ttCall.DateSt      AND
               BDest.FromDate <= ttCall.DateSt
            NO-LOCK NO-ERROR.
                 
            IF AVAILABLE BDest THEN DO:
               dest_recid = RECID(BDest).
               LEAVE.
            END.   
         END.
      END.
   END.

   IF TENANT-NAME("common") EQ {&TENANT_YOIGO} AND
      ttCall.Spocmt NE 3 AND 
      ttCall.Spocmt NE 4 AND 
      fIsYoigoCLI(mod_bsub) THEN DO:

      IF mod_bsub BEGINS "622" AND
      (b_sub ne "622" AND 
       b_sub ne "622622622" AND
       b_sub ne "622FF") THEN mod_bsub = "YOIGO".
      ELSE IF mod_bsub BEGINS "633" AND
      (b_sub ne "633"       AND
       b_sub ne "633633633" AND 
       b_sub ne "633800800" AND
       b_sub ne "633633632") THEN mod_bsub = "YOIGO".
      ELSE IF mod_bsub BEGINS "722" THEN mod_bsub = "YOIGO".
   END.          

   /* YDR-1642: added logic to divide Prepaid data package CDRs
      in a case of TARJ7 / TARJ9.
   */
   IF TTCall.PPFlag = 1 AND ttCall.SpoCMT EQ 93 THEN DO:
      /* TARJ7 case */
      IF ttCall.ServiceClass = {&SC_TARJ7_INSIDE_DATABUNDLE1} 
      OR ttCall.ServiceClass = {&SC_TARJ7_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ7".
      /* TARJ9 case */
      ELSE IF ttCall.ServiceClass = {&SC_TARJ9_INSIDE_DATABUNDLE1}
      OR ttCall.ServiceClass = {&SC_TARJ9_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ9".
      /* TARJ10 case */
      ELSE IF ttCall.ServiceClass = {&SC_TARJ10_INSIDE_DATABUNDLE1}
      OR ttCall.ServiceClass = {&SC_TARJ10_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ10".
      /* TARJ11 case */
      ELSE IF ttCall.ServiceClass = {&SC_TARJ11_INSIDE_DATABUNDLE1}
      OR ttCall.ServiceClass = {&SC_TARJ11_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ11".
      /* TARJ12 case */
      ELSE IF ttCall.ServiceClass = {&SC_TARJ12_INSIDE_DATABUNDLE1}
      OR ttCall.ServiceClass = {&SC_TARJ12_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ12".
      /* TARJ13 case */
      ELSE IF ttCall.ServiceClass = {&SC_TARJ13_INSIDE_DATABUNDLE1}
      OR ttCall.ServiceClass = {&SC_TARJ13_INSIDE_DATABUNDLE2} THEN
         mod_bsub = "GPRSDATA_TARJ13".
   END.

   IF dest_recid = ? OR 
      dest_recid = 0 THEN 
   DO b = LENGTH(mod_bsub) TO 1 BY -1.

      IF CAN-FIND(FIRST BDest WHERE 
                  BDest.Brand    = msowner.Brand        AND 
                  BDest.BDest    = SUBSTR(mod_bsub,1,b) AND
                  BDest.DestType = b_Type               AND
                  BDest.ToDate  >= ttCall.DateSt) THEN DO:  

         FIND FIRST BDest WHERE 
            BDest.Brand      = msowner.Brand        AND 
            BDest.BDest      = SUBSTR(mod_bsub,1,b) AND 
            BDest.DestType   = b_Type               AND 
            BDest.ToDate    >= ttCall.DateSt        AND
            BDest.FromDate  <= ttCall.DateSt        
            NO-LOCK NO-ERROR.

         IF AVAILABLE BDest THEN DO:
            dest_recid = RECID(BDest). /* RECID FOR CASE 'GENERAL B' */
            LEAVE.
         END.   
      END.   
   END.

   /************************************
   * IF dest_recid has now any VALUE   *
   * it means that either a private OR *
   * a common B-destination has been   *
   * found.  Read in that Bdest AND   *
   * get assiciated FIELD values.      *
   ************************************/

   IF dest_recid NE ? THEN DO:

      FIND FIRST BDest WHERE RECID(BDest) = dest_recid NO-LOCK.

      IF BDest.BDest NE "ROAM_EU" AND
         CAN-FIND(FIRST BDestTrans NO-LOCK WHERE
                        BDestTrans.BDestId EQ BDest.BdestId) THEN DO:
         
         IF lcTranslatedAddress EQ "" THEN 
            lcTranslatedAddress = fGetMcdrDtlValue(
                                      ttCall.Datest,
                                      ttCall.Dtlseq,
                                      "Translated address").

         FIND FIRST BDestTrans NO-LOCK WHERE
                    BDestTrans.BDestId = BDest.BDestId AND
                    BDestTrans.TranslateNumber = lcTranslatedAddress AND 
                    BDestTrans.ToDate >= ttCall.DateSt AND
                    BDestTrans.FromDate <= ttCall.DateSt AND
                    BDestTrans.BDest = BDest.BDest NO-ERROR.

         IF AVAIL BDestTrans THEN DO:

            mod_bsub = BDestTrans.BDest + BDestTrans.RatingZone.

            IF BDestTrans.RatingZone EQ "B" THEN DO:
               IF ttCall.BillDur <= 11 THEN mod_bsub = mod_bsub + "1".
               ELSE mod_bsub = mod_bsub + "2".
            END.

            FIND FIRST Bdest NO-LOCK WHERE 
                       Bdest.Brand = msowner.Brand AND
                       BDest.BDest = mod_bsub AND
                       Bdest.DestType = b_Type AND
                       BDest.Todate >= ttCall.DateSt AND
                       Bdest.FromDate <= ttCall.DateSt NO-ERROR.

            IF NOT AVAIL BDest THEN dest_recid = ?.
            ELSE dest_recid = RECID(BDest).

         END.
         ELSE IF CAN-FIND(FIRST BDestTrans NO-LOCK WHERE
                                BDestTrans.BDestId EQ BDest.BdestId AND
                                BDestTrans.ToDate >= ttCall.DateSt AND
                                BDestTrans.FromDate <= ttCall.DateSt AND
                                BDestTrans.BDest = BDest.BDest) THEN ASSIGN
            dest_recid = ?
            oiErrCode  = {&CDR_TRANSLATED_ADDRESS_NOT_FOUND}.

      END.
   
      IF dest_recid NE ? THEN ASSIGN
         b_dest     = (IF BDest.RateBDest NE "" THEN BDest.RateBDest
                       ELSE BDest.Bdest)   /* Classified B-destination */
         r_dest     = BDest.Bdest
         b_ccn      = BDest.CCN   /* consequtive country number       */
         b_dg-code  = BDest.DiscGroup
         b_foc      = TRUE WHEN BDest.Free = TRUE. /* free of charge ? */
         
   END. 

   IF dest_recid EQ ? THEN  /* unknown destination (!)  */
      ASSIGN 
      b_dest     = ""
      b_ccn      = 0
      b_dg-code  = ?.
   
END.     
