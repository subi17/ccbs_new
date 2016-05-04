/* man_rate2.i


   changes:         31.05.04/aam ServRid & MPMRid
                    26.10.04 jp   pnpgroup
                    04.01.05 aam CreditType, Balance and NotifyNumber 
                                 from SubSer
                    25.01.05 jp  kumppani             
                    10.03.05 aam InvCust to fChkMobsubLimit
                    28.09.05 aam use brand with BillItem
                    18.07.06 kl bnet analysis fixed (?)
                    11.01.07/aam MsSeq based fInvSeq (in mobcdr_rate.i)
                    26.01.07/aam use RateCCN when BDest.Class=2 (freephone),
                                 'roamint' and 'roamlocal' changed the other
                                 way round
*/
{error_codes.i}
  
   /* Is this double call */    
   IF Mobcdr.ErrorCode = errorcode OR
     (MobCDR.ERrorcode > 8002 AND
      Mobcdr.ERrorcode < 8050 ) THEN NEXT.
                             
   EMPTY TEMP-TABLE ttCall.
   BUFFER-COPY mobcdr to ttCall.

   llChanged = FALSE.
   
   /* Never touch billed calls */
   IF ttCall.invseq ne 0  AND 
      ttCall.ErrorCode = 0 THEN DO:
      IF CAN-FIND(FIRST invseq where 
                        invseq.invseq = ttCall.invseq AND
                        invseq.Billed = TRUE) THEN NEXT.
   END.
   
   ASSIGN ttCall.ErrorCode = 0.

   &IF "{&CDR_RERATE_I}" EQ "YES" 
   &THEN
   ASSIGN
      ttCall.RerateID = liRerateSeq 
      ttCall.RerateTS = fMakeTS().
   fRerateCDRBefore (MobCDR.RateCCN,
                     MobCDR.ErrorCode,
                     MobCDR.Amount,
                     MobCDR.DateSt).
   &ENDIF

   anal:
   REPEAT WITH FRAME MobCDR:   
   
      
      ASSIGN 
         ttCall.ErrorCode = 0
         old_price        = ttCall.Amount
         mi-no            = ttCall.CLI
         oiErrorCode      = 0
         lcMSRN           = ""
         lcTranslatedAddress = "".

      IF ttCall.DateSt = 6/10/2014 AND
         ttCall.Spocmt = 93 AND
         ttCall.MSCID  = "POSTD" AND
         ttCall.ReadDate < 6/15/2014 THEN
         oiErrorCode = 8040.

      IF oiErrorCode = 0 THEN 
         oiErrorCode = fCallCaseCheck(ttCall.SpoCMT,ttCall.DateSt).

      IF oiErrorCode = 0 THEN 
         fTicketCheck(INPUT "BTYPE",STRING(ttCall.bType),
                     OUTPUT oiErrorCode).

      IF oiERrorCode   = 0  AND
         ttCall.SpoCMT = 51 AND
         ttCall.GsmBnr = "" THEN DO:
                           
         ASSIGN oiErrorCode = {&CDR_ERROR_UNKNOWN_GSM_B_NUMBER}.
                                   
      END.

      IF oiErrorCode = 0 AND LOOKUP(STRING(ttCall.spocmt),"7,33") > 0 AND
         ttCall.MSCID NE "TAP3" THEN DO:
         lcMSRN = fGetMcdrDtlValue(ttCall.Datest,
                                   ttCall.Dtlseq,
                                   "MSRN").
         IF lcMSRN = "" THEN oiErrorCode = 9012.
      END.

      IF oiErrorCode = 0 AND ttCall.spocmt = 90 THEN DO:
         lcSGSNAddress = fGetMcdrDtlValue(ttCall.Datest,
                                   ttCall.Dtlseq,
                                   "SGSN address").
      END.
 
      /* roaming voice mail forwarding */
      IF oiErrorCode = 0 AND ttCall.DateSt >= 7/1/10 THEN DO:
         
         /* phase1 (plan B): 
            - mark all CC 7 (roaming MT) to error
            - mark all CC 3/4 (roaming MO) with VM b-number to error   */
         IF ttCall.DateSt < 7/22/10 OR 
            (ttCall.DateSt = 7/22/10 AND ttCall.TimeSt < 36000) THEN DO:   
            IF ttCall.SpoCMT = 7 THEN oiErrorCode = {&CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MT}.
            ELSE IF LOOKUP(STRING(ttCall.SpoCMT),"3,4") > 0 THEN DO:
               IF LOOKUP(ttCall.GsmBnr,"633,633633633,632,633633632,") > 0 THEN 
               oiErrorCode = {&CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MO}.
            END.
         END.
         
         /* phase 2 (plan A): 
           - mark CCs 32 and 33 to error */
         IF ttCall.SpoCMT = 32 THEN oiErrorCode = {&CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MO}.
         ELSE IF ttCall.SpoCMT = 33 THEN oiErrorCode = {&CDR_ERROR_ROAMING_CALL_FORWARDING_VMS_MT}.
      END.   
       
      IF oiErrorCode > 0 THEN DO:
         ASSIGN
         ttCall.CustNum    = liUnkCust
         ttCall.ErrorCode  = oiErrorCode
         ttCall.Invseq     = 0.

         fBCopy().
         LEAVE anal.
      END.
      
      /**********************************
      * Reset rate and discount fields  *
      **********************************/

      ASSIGN
         ttCall.DiscFP        = 0
         ttCall.Fdisc         = 0
         ttCall.StartCharge   = 0
         ttCall.TotDisc       = 0
         ttCall.Amount        = 0
         ttCall.RefPrice      = 0
         ttCall.BDest         = ""
         ttCall.InvCust       = 0
         ttCall.CustNum       = 0
         ttCall.AgrCust       = 0
         ttCall.ccn           = 0
         ttCall.BillCode      = ""
         ttCall.clitype       = ""
         ttCall.DCEvent       = ""
         ttCall.DCType        = ""
         MobSubLimit          = 0
         ldeGrossAmt          = 0
         ldeRoamarg           = 0 
         CreditType           = 0 
         rate-plcode          = ""
         lcNotifyNumber       = ""
         lcNetWorkOwner       = "".

      llRoamind = IF ttCall.RoamingInd = 1 THEN TRUE ELSE FALSE.

      IF oiErrorCode = 0 THEN
      fTicketCheck(INPUT "GSMBNR", STRING(ttCall.gsmbnr),
                   OUTPUT oiERrorCode).
                               
      /* Build a Time Stamp from Call's Start Date & Time, i.e.
         YYYYMMDD.sssss  where sssss = no. of seconds from midnight */
      
      CallTimeStamp = YEAR(ttCall.Datest)  * 10000 + 
                      Month(ttCall.DateST) *   100 +
                      DAY(ttCall.Datest)           +
                      (ttCall.TimeStart / 100000).
      
      IF oiErrorCode = 0 THEN DO:
         IF LOOKUP(STRING(ttCall.Spocmt),"3,4,7,17,32") = 0 OR
            (LOOKUP(STRING(ttCall.SpoCMT),"3,7") > 0 AND 
             ttCall.MSCID = "PRE" AND ttCall.PPFlag = 1) THEN DO:
            fTicketCheck(INPUT "MSOWNER", 
                         STRING(ttCall.CLI),
                         OUTPUT oiERrorCode).
         END.                
         ELSE DO:
            IF  LOOKUP(STRING(ttCall.Spocmt),"3,4,32") > 0 THEN 
               fTicketCheck(INPUT "IMSI", 
                            STRING(ttCall.IMSI), 
                            OUTPUT oiErrorCode).
            ELSE                                             
               fTicketCheck(INPUT "IMSI", 
                            STRING(ttCall.IMSI2),
                            OUTPUT oiErrorCode).
         END.                   
      END.

      /* gprs without any data */
      IF oiErrorCode = 0 AND  
         LOOKUP(STRING(ttCall.SpoCMT),"90,91,92,93") > 0 AND 
         ttCall.DataIn + ttCall.DataOut = 0 THEN DO:
         /* prepaid */
         IF ttCall.PPFlag > 0 THEN DO:
            IF ttCall.Charge < 0.01 THEN 
               oiErrorCode = {&CDR_ERROR_NATIONAL_GPRS_ZERO_DATA}.
         END.   
         /* postpaid */
         ELSE DO:
            IF ttCall.SpoCMT = 93 THEN 
               oiErrorCode = {&CDR_ERROR_NATIONAL_GPRS_ZERO_DATA}.
            ELSE oiErrorCode = {&CDR_ERROR_ROAMING_GPRS_ZERO_DATA}.
         END.   
      END.   
      
      IF oiErrorCode = 0 AND
         msowner.CLIType EQ "CONTM2" AND
         ttCall.EventType EQ "CALL" THEN 
         oiErrorCode = {&CDR_ERROR_NON_BILLABLE_CONTM2_CALL}.

      IF oiErrorCode > 0 THEN DO:
         ASSIGN
            ttCall.ErrorCode = oiErrorCode
            ttCall.InvSeq    = 0.
            
         fBCopy().
         LEAVE anal.
      END.

      /************************************
      * Find the mobsub record and set    *
      * 'alive' flag                      *
      ************************************/

      IF llSaldoReminder AND 
         CAN-FIND(FIRST mobsub WHERE mobsub.msseq = msowner.msseq) 
      THEN DO:
      
         CreditType = fCreditTypeValue(MsOwner.MsSeq,
                                       OUTPUT MobSubLimit).
         IF MobSubLimit NE 0 THEN 
            lcNotifyNumber = fNotifyNbrValue(MsOwner.MsSeq).
      END. 
      
      /************************************
      * A-Sub was now recognized.  Update *
      * associated customer data fields   *
      ************************************/
        
      ASSIGN
         ttCall.CustNum     = msowner.CustNum            
         ttCall.InvCust     = Msowner.InvCust
         ttCall.AgrCust     = MsOwner.AgrCust
         ttCall.BillTarget  = msowner.BillTarget  /* Invoicing Target */
         ttCall.MSSeq       = msowner.MSSeq   /* Sequence FOR a Subscrip   */
         ttCall.CLIType     = msowner.CLIType  /* MobSub connection type  */
         lcrateBrand        = msowner.Brand .

      IF Msowner.Paytype = TRUE THEN DO:
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_WRONG_PAY_TYPE} 
            ttCall.Invseq    = 0 .
         
         fBCopy().
         LEAVE.
      END.   
      
      /*******************************
      * CALLING PART IS NOW ANALYSED *
      *                              *
      * NEXT ANALYSE CALLED PART     *
      * -> destination               *
      * -> product code              *
      * -> discount group            *
      * -> CCN                       *
      *******************************/

      /***************************
      * Mobile Originating Call  *
      * OR Call Forwarding  OR   *
      * Roaming terminated Call  *
      ***************************/
      fanalbsub( INPUT  ttCall.CustNum,
                 INPUT  ttCall.GsmBnr,
                 INPUT  ttCall.btype ,
                 OUTPUT b_dest,
                 OUTPUT r_dest,
                 OUTPUT b_ccn,
                 OUTPUT b_dg-code,
                 OUTPUT b_foc,
                 OUTPUT b_PNP,
                 OUTPUT b_prodcode,
                 OUTPUT b_asubtype,
                 OUTPUT oiErrorCode).

      IF b_dest = "" OR b_dest = ? OR oiErrorCode NE 0 THEN DO:
            /* Unknown destination */
         ASSIGN
            ttCall.ErrorCode = oiErrorCode WHEN oiErrorCode NE 0
            TTCall.ErrorCode = IF ttCall.SpoCMT = 200
                               THEN {&CDR_ERROR_UNKNOWN_VOIP_B_DESTINATION}
                               ELSE {&CDR_ERROR_UNKNOWN_B_DESTINATION}
                               WHEN oiErrorCode EQ 0
            ttCall.Invseq    = 0.
         fBCopy().
         LEAVE.
      END.

      /************************************
      * B-sub was identified: NEXT        *
      * UPDATE B-sub information on MobCDR  *
      ************************************/

      ASSIGN
         ttCall.BDest     = b_dest    /* Classified Destination       */
         ttCall.ccn       = b_ccn     /* Consecutive Country No.      */
         ttCall.BillCode  = b_prodcode /* ONLY WHEN PNP */
         ttCall.dialtype  = b_asubtype . /* JP*/ 
     
      IF ttCall.Charge = 0 THEN
         ttCall.Charge    = ttCall.pulses * ldPulseRate.
      IF ttCall.Charge = ? THEN ttCall.Charge = 0.
          /* SOME SPECIAL CASES */
     
      /***********
      *  RATING  *
      ***********/
         
      ASSIGN 
         lidialtype = ttCall.Dialtype
         liCCN      = ttCall.RateCCN.

      IF ttCall.Spocmt = 66 THEN DO:
        
         liccn = 0.
         FOR FIRST BDest NO-LOCK WHERE
                   BDest.Brand  = gcBrand AND
                   BDest.Bdest  = ttCall.BDest AND
                   BDest.DestType = ttCall.BType AND
                   BDest.Class  = 2       AND
                   BDest.ToDate >= ttCall.DateSt AND
                   BDest.FromDate <= ttCall.DateSt,
             FIRST RateCCN NO-LOCK WHERE
                   RateCCN.BDestID  = Bdest.BDestID AND
                   RateCCN.DialType = 4 /* ttCall.DialType */ :
           
            ASSIGN
               liCCN      = RateCCN.CCN
               liDialType = 4.
         END.

         IF liCCN = 0 THEN 
            liCCN = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
              
         IF liCCN = 0 THEN liCCN = ttCall.SpoCmt.   
      END.

      IF liCCN = 0 OR liCCN = 999 OR ttCall.BType = 2 THEN 
          liCCN = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).

      IF liCCN = 69 AND ttCall.BillDur > 11 THEN DO:
         IF CAN-FIND(FIRST ttDuration WHERE 
                           ttDuration.CallCase = "61" AND
                           ttDuration.BDest    = ttCall.BDest AND
                           ttDuration.FromDate <= ttCall.DateSt) THEN 
            liCCN = 61.
      END.

      /* YDR-1853 */
      IF ttCall.Spocmt = 63 AND ttCall.BillDur <= 20 THEN 
         ASSIGN lidialtype = 20
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype).
      ELSE IF ttCall.Spocmt = 63 AND ttCall.BillDur > 20 THEN 
         ASSIGN lidialtype = 21
                liCCN      = fRateCCN(ttCall.bdest,ttCall.BType,lidialtype). 
      
      ASSIGN                   
           c_time       = ttCall.timestart
           x-time       = ttCall.timestart + ttCall.billdur
           c_day        = ttCall.datest 
           c_dur        = ttCall.billdur
           asub-cust    = ttCall.CustNum
           rate-cust    = msowner.CustNum
           asubtype     = lidialtype
           libilltarget = ttCall.billtarget
           c_bppref     = "MOB"
           bsubs        = b_Dest 
           ttCall.RateCCN = liCCN.

 
      /* GPRS as Data amount based (bytes) */
      IF lidialtype = 7 THEN
         c_dur = ttCall.DataIN + ttCall.DataOut.

      /* duration may be only partly billable */
      ELSE IF CAN-FIND(FIRST ttDuration WHERE
                       ttDuration.CallCase = STRING(ttCall.RateCCN)) THEN DO:
         c_dur = fBillableDuration(STRING(ttCall.RateCCN),
                                   ttCall.BDest,
                                   ttCall.DateSt,
                                   ttCall.BillDur).
      END.
   
      fTariff().

      IF rc = 0 THEN DO:
        
         /* final check if still billable */ 
         IF ttCall.ErrorCode = 0 THEN 
            ttCall.ErrorCode = fFinalBillableCheck().

         /* if cdr is going to be moved from error to billable then check for
            doubles (GsmBnr and BillDur may have been changed here) */
         IF ttCall.ErrorCode = 0 AND MobCDR.ErrorCode > 0 THEN DO:
            fIsDoubleCall("rerate",RECID(MobCDR)).
         END.

         IF ttCall.ErrorCode > 0 THEN DO:
            ttCall.InvSeq = 0.
            fBCopy().
            LEAVE anal.
         END.   
 
         /* data, voice and other packages */
         fPackageCalculation().
            
         IF ttCall.ErrorCode > 0 THEN DO:
            ttCall.InvSeq = 0.
            fBCopy().
            LEAVE anal.
         END.   
         
         /* PRERATED TICKET */
         IF LOOKUP(STRING(ttCall.SpoCMT),"72,73,78," +
                   STRING({&GB_CCN})) > 0 THEN
            bPrice = ttCall.ccharge.
           
         ASSIGN               
            ttCall.GrossAmt     = bprice /* + ttCall.charge */ 
            ttCall.startCharge  = base
            ttCall.refprice     = bprice
            ttCall.VatIncl      = llVatIncl
            ttCall.DiscType     = DiscType
            ttCall.Fdisc        = DiscVal
            ttCall.DiscFP       = discpr
            ttCall.CurrUnit     = llCurrUnit
            ttCall.TotDisc      = ttCall.FDisc
            ttCall.Amount       = bprice - ttCall.TotDisc
            ttCall.ErrorCode    = 0
            TotValue            = TotValue + ttCall.GrossAmt
            ttCall.TariffNum    = ttTariff.TariffNum
            ttCall.ServRid      = lcServRid
            ttCall.MPMRid       = lcMPMRid.

      END.  /* rc=0 */

      ELSE IF rc = 9 THEN DO:  /* RC NE 0 */
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_NO_BILLING_TARGET_FOUND} 
            ttCall.InvSeq    = 0 .
      END.      /* RC=9 */
      ELSE IF rc = 8 THEN DO:  /* RC NE 0 */
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}
            ttCall.InvSeq    = 0 .
      END.      /* RC=8 */
      ELSE IF rc = 7 THEN DO:  /* RC NE 0 */
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PREF_FOUND}
            ttCall.InvSeq    = 0 .
      END.     /* RC=7 */
      ELSE IF rc = 5 THEN DO:
         ASSIGN
            ttCall.ErrorCode = {&CDR_ERROR_NO_TARIFF_FOUND}
            ttCall.InvSeq    = 0 .
      END.
 
      /* Update PremiumNumber Operator information only for VOICE */
      IF ttCall.MSCID <> "CCGW" THEN DO:
         FIND FIRST BillItem WHERE
                    BillItem.Brand    = gcBrand AND
                    BillItem.BillCode = ttCall.BillCode NO-LOCK NO-ERROR.
         IF AVAILABLE BillItem AND BillItem.BIGroup = "6" THEN
            ttCall.ServiceName = fGetPremiumServiceName(ttCall.GsmBnr,
                                                        ttCall.DateSt).
      END. /* IF ttCall.MSCID <> "CCGW" THEN DO: */

      IF ttCall.ErrorCode = 0 THEN 
         ttCall.invseq = fInvSeq(ttCall.AgrCust,
                                 ttCall.InvCust,
                                 ttCall.MsSeq,
                                 ttCall.DateSt,
                                 ttCall.PPFlag). 
      ELSE ttCall.InvSeq = 0.
 
      IF ttCall.invseq ne 0 then DO:

         f{&CounterHandling}CallCount(ttCall.MsSeq,
                                      YEAR(ttCall.Datest) * 100 + 
                                         MONTH(ttCall.Datest),
                                      ttCall.Amount).
               
         IF MobSubLimit  ne 0 THEN DO:
            fChkMobsubLimit(INPUT ttCall.InvCust,
                            INPUT ttCall.cli,
                            INPUT ttCall.MSSeq,
                            INPUT lcNotifyNumber,
                            INPUT ttCall.datest,
                            INPUT DECIMAL(mobsubLimit),
                            INPUT credittype).
         END.
      END.   
         
      IF (ttCall.Addbpref = "151" OR 
          ttCall.AddbPref = "152") AND
         ttCall.BType ne 1 THEN DO:
        
         ttCall.billCode  = "151".
      END.

      fBCopy().
      LEAVE.
             
   END. /* anal */

   PAUSE 0.
   
   totalchanges = totalchanges + (mobcdr.amount - old_price).   
   
   &IF "{&CDR_RERATE_I}" EQ "YES" 
   &THEN
   fRerateCDRAfter (ttCall.RateCCN,
                    ttCall.ErrorCode,
                    ttCall.Amount,
                    llChanged).
   &ENDIF

   IF NOT bbatch THEN DO:
      PAUSE 0.
      
      IF count mod 50 = 0 then do:
         DISP                    
         ttCall.Datest
         ttCall.InvCust      format "zzzzzzzz9"
         ttCall.CLI          format "x(11)"
         ttCall.BillCode     format "x(4)"         column-label "Prod"
         old_price           format "zz9.99"
         ttcall.Amount       format "zz9.99"
         ttCall.bdest        format "x(14)" 
         ttCall.ErrorCode    format "zzz9" 
         WITH TITLE "  ANALYSIS OF MOBILE CDRS "  
            OVERLAY CENTERED ROW 2 13 down frame log.
         
         put screen row 1 col 2 string(count).
         
         PUT SCREEN ROW 18 COL 12 "Total changes(qty):" + STRING(lichangedqty)
            +  "        Current  price:" + STRING(mobcdr.amount,"-zz,zz9.999").
         PUT SCREEN ROW 19 COL 40 "Previous price:" +
            STRING(old_price,"-zz,zz9.999").
         PUT SCREEN ROW 20 COL 40 "Total changes :" +
            STRING(totalchanges,"-zz,zzz.999").
      END.
   
      ASSIGN count = count + 1.
   END.

END.  /* foreach mobcdr */

/* counters from temp-tables to db */
&IF "{&CounterHandling}" = "TempTable" 
&THEN
fTemp2SaldoCounter().
fTemp2DCCounter().
fTemp2ServiceLCounter().
&ENDIF

IF lrRerateLog NE ? THEN DO:
   fFinalizeRerateLog(liCustChanged).
   liCustChanged = 0.
END.

END.  /* do */

IF NOT bbatch THEN DO: 
   MESSAGE
   "Totally" count "Mobile call Records were"  SKIP
   "analysed during this run."
   "Duration: " etime
   VIEW-AS ALERT-BOX.
   HIDE FRAME log NO-PAUSE.
END.

LEAVE main.

END. /* MAIN */

/* clear screen */
IF NOT SESSION:BATCH THEN DO:
   HIDE FRAME main NO-PAUSE.
   HIDE MESSAGE NO-PAUSE.
END.

&IF "{&PersistentRun}" = "yes"
&THEN
END PROCEDURE.
&ENDIF 
    
