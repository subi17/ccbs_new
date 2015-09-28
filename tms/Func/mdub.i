/* mdub.i 
   add / remove service used by mdub 
*/
&IF "{&mdub}" NE "YES"
&THEN

&GLOBAL-DEFINE mdub YES

{timestamp.i}
{fmakemsreq.i}
{service.i}
{tmsconst.i}
{cparam2.i}
{fbundle.i}
{fbtc.i}

DEF VAR ldaNextMonthActDate AS DATE NO-UNDO.
DEF VAR ldNextMonthActStamp AS DEC  NO-UNDO.

ASSIGN ldaNextMonthActDate = (fLastDayOfMonth(TODAY) + 1)
       ldNextMonthActStamp = fMake2Dt(ldaNextMonthActDate,0).

FUNCTION fGetActiveMDUB RETURNS CHAR (INPUT ideActStamp  AS DEC):
    
   DEF VAR lcBundle         AS CHAR NO-UNDO. 
   DEF VAR i                AS INT  NO-UNDO.
   DEF VAR liNumEntries     AS INT  NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.

   ASSIGN lcBONOContracts = fCParamC("BONO_CONTRACTS")
          liNumEntries    = NUM-ENTRIES(lcBONOContracts).

   IF ideActStamp = 0 OR ideActStamp = ? THEN
      ideActStamp = fMakeTS().

   DO i = 1 TO liNumEntries:
      lcBundle = ENTRY(i,lcBONOContracts).
      /* check if exist any bono contract valid to the future */   
      FOR EACH ServiceLimitGroup NO-LOCK WHERE 
               ServiceLimitGroup.Brand     = gcBrand AND
               ServiceLimitGroup.GroupCode = lcBundle,
          EACH ServiceLimit NO-LOCK WHERE 
               ServiceLimit.GroupCode = ServiceLimitGroup.GroupCode AND 
               ServiceLimit.ValidFrom <= TODAY AND 
               ServiceLimit.ValidTo   >= TODAY:

          IF CAN-FIND(FIRST MServiceLimit WHERE 
                            MServiceLimit.MSSeq    = MobSub.MsSeq          AND
                            MServiceLimit.DialType = ServiceLimit.DialType AND
                            MServiceLimit.SlSeq    = ServiceLimit.SlSeq    AND 
                            MServiceLimit.EndTS   >= ideActStamp) THEN 
          RETURN ServiceLimit.GroupCode.  
      END.
   END.
   RETURN "".
END FUNCTION.

FUNCTION fPendingMDUBActReq RETURNS LOGICAL :
   
   DEF VAR lcBundle         AS CHAR NO-UNDO. 
   DEF VAR i                AS INT  NO-UNDO.
   DEF VAR liNumEntries     AS INT  NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.

   ASSIGN lcBONOContracts = fCParamC("BONO_CONTRACTS")
          liNumEntries    = NUM-ENTRIES(lcBONOContracts).

   DO i = 1 TO liNumEntries:
      lcBundle = ENTRY(i,lcBONOContracts).

      IF CAN-FIND(FIRST MsRequest WHERE
                        MsRequest.MsSeq      = MobSub.MsSeq  AND
                        MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND
                        MsRequest.ReqCParam3 = lcBundle AND
                  LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
      THEN RETURN TRUE.

   END.
   IF fPendingRequest(MobSub.MsSeq,10) THEN RETURN TRUE.
  RETURN FALSE.
END FUNCTION.

FUNCTION fServPackagesActive RETURNS LOGICAL :

   FIND FIRST CTServPac WHERE
              CTServPac.Brand   = gcBrand AND
              CTServPac.CLIType = MobSub.CLIType AND
              CTServPac.ServPac = "SHAPER" AND  
              CTServPac.ToDate >= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CTServPac THEN RETURN FALSE.

   FIND FIRST CTServPac WHERE
              CTServPac.Brand   = gcBrand AND
              CTServPac.CLIType = MobSub.CLIType AND
              CTServPac.ServPac = "HSDPA" AND  
              CTServPac.ToDate >= TODAY NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CTServPac THEN RETURN FALSE.

   RETURN TRUE.
END FUNCTION.

FUNCTION fAllowMDUBActivation RETURNS LOGICAL :

   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.

   lcBONOContracts = fCParamC("BONO_CONTRACTS").

   /* should not exist any MDUB valid to the future */
   IF fGetActiveMDUB(INPUT fMakeTS()) > "" THEN RETURN FALSE.
   /* should not exist any pending request for MDUB */
   IF fPendingMDUBActReq() THEN RETURN FALSE.
   /* check service package definition exist for SHAPER and HSDPA */
   IF NOT fServPackagesActive() THEN RETURN FALSE. 
   /* Check ongoing BONO BTC */
   IF fOngoingBTC(MobSub.MsSeq,lcBONOContracts,FALSE) THEN RETURN FALSE.

   RETURN TRUE.
END FUNCTION. 

FUNCTION fActivateMDUBService RETURN LOGICAL
   ( INPUT icServPac AS CHAR,
     INPUT idtActDate AS DATE):
    
   DEFINE VARIABLE liNumComp AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
   RUN pCopyPackage(MobSub.CLIType,
                    icServPac,
                    "",
                    MobSub.MSSeq,
                    idtActDate,
                    ?,    /* all changed ones, force it  */
                    FALSE,   /*  create fees */
                    TRUE,   /* solog (provisioning) */
                    0,
                    FALSE,
                    OUTPUT liNumComp). 
   IF liNumComp = 0 THEN    
       RETURN FALSE.

   RETURN TRUE.
END FUNCTION. 

FUNCTION fActivateMDUBPerContract RETURNS LOGICAL 
   (INPUT icPerContract AS CHAR,
    INPUT icSource AS CHAR,
    INPUT icCreator AS CHAR,
    INPUT iiOrigRequest AS INT,
    INPUT idActTS   AS DECIMAL):

   DEFINE VARIABLE liRequest AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
  
   liRequest = fPCActionRequest(MobSub.MsSeq,
                                icPerContract,
                                "act",
                                idActTS,
                                TRUE, /* create fee */
                                icSource,
                                icCreator,
                                iiOrigRequest,
                                TRUE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
   IF liRequest = 0 THEN 
      RETURN FALSE.

   RETURN TRUE.
END FUNCTION.

FUNCTION fPendingMDUBTermReq RETURNS LOGICAL :
   
   DEF VAR lcBundle         AS CHAR NO-UNDO. 
   DEF VAR i                AS INT  NO-UNDO.
   DEF VAR liNumEntries     AS INT  NO-UNDO.
   DEF VAR lcBONOContracts  AS CHAR NO-UNDO.

   ASSIGN lcBONOContracts = fCParamC("BONO_CONTRACTS")
          liNumEntries    = NUM-ENTRIES(lcBONOContracts).

   DO i = 1 TO liNumEntries:
      lcBundle = ENTRY(i,lcBONOContracts).
      
      IF CAN-FIND(FIRST MsRequest WHERE
                        MsRequest.MsSeq      = MobSub.MsSeq  AND
                        MsRequest.ReqType    = {&REQTYPE_CONTRACT_TERMINATION} AND
                        MsRequest.ReqCParam3 = lcBundle AND
                  LOOKUP(STRING(MsRequest.ReqStatus),{&REQ_INACTIVE_STATUSES}) = 0)
      THEN RETURN TRUE.
   END.
   RETURN FALSE.
END FUNCTION.


FUNCTION fAllowMDUBTermination RETURNS LOGICAL :
   
   /* should exist any data bundle contract valid to the future */   
   IF fGetActiveMDUB(INPUT ldNextMonthActStamp) EQ "" THEN RETURN FALSE.

   /* should not exist any pending request for MDUB */
   IF fPendingMDUBTermReq() THEN RETURN FALSE.

   RETURN TRUE.
END FUNCTION.

FUNCTION fTerminateMDUBService RETURNS LOGICAL
   (INPUT icServPac AS CHAR,
    INPUT idtReqDate AS DATE):

   DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldTermTS AS DECIMAL NO-UNDO.
   
   ldTermTS = fHMS2TS(fLastDayOfMonth(idtReqDate) ,"23:59:59").

   FOR FIRST ServPac NO-LOCK WHERE
             ServPac.Brand   = gcBrand AND
             ServPac.ServPac = icServPac,
        EACH ServEl NO-LOCK WHERE
             ServEl.Brand   = gcBrand AND
             ServEl.ServPac = ServPac.ServPac,
       FIRST SubSer NO-LOCK WHERE
             SubSer.MsSeq   = MobSub.MsSeq AND
             SubSer.ServCom = ServEl.ServCom:

         IF SubSer.SSStat > 0 THEN DO:
            liRequest = fServiceRequest(MobSub.MsSeq,
                                        SubSer.ServCom,
                                        0,
                                        SubSer.SSParam,
                                        ldTermTS,
                                        "",
                                        TRUE,      /* fees */
                                        TRUE,      /* sms */          
                                        "",
                                        "",
                                        0,
                                        FALSE,
                                        OUTPUT lcError).
                 
            IF liRequest = 0 THEN                              
               RETURN FALSE.
         END.       
      END.

   RETURN TRUE.
END FUNCTION.

FUNCTION fTerminateMDUBPerContract RETURNS LOGICAL 
         (INPUT icPerContract AS CHAR,
          INPUT icSource AS CHAR,
          INPUT icCreator AS CHAR,
          INPUT iiOrigRequest AS INT,
          INPUT idtReqDate AS DATE):

   DEFINE VARIABLE llDo AS LOGICAL NO-UNDO INITIAL FALSE.
   DEFINE VARIABLE liRequest AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldTS AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldTermTS AS DECIMAL NO-UNDO.

   /* define termination request stamp  */ 
   ldTS = fMakeTS().

   FOR EACH MServiceLimit NO-LOCK WHERE
            MServiceLimit.MSSeq = MobSub.MsSeq AND
            MServiceLimit.EndTS > ldTS,
      FIRST ServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
            ServiceLimit.SlSeq = MServiceLimit.SlSeq AND
            ServiceLimit.GroupCode = icPerContract:
      llDo = TRUE.
      LEAVE.
   END.
   IF NOT llDo THEN RETURN TRUE. /* nothing has to be done */ 
          
   ldTermTS = fHMS2TS(fLastDayOfMonth(idtReqDate) ,"23:59:59").

   liRequest = fPCActionRequest(MobSub.MsSeq,
                                icPerContract,
                                "term",
                                ldTermTS,
                                TRUE, /* create fee if is defined*/
                                icSource,
                                icCreator,
                                iiOrigRequest,
                                FALSE,
                                "",
                                0,
                                0,
                                OUTPUT lcError).
   RETURN (liRequest > 0).       

END FUNCTION.

FUNCTION fMDUBFixedFeeAmt RETURNS DECIMAL
   (INPUT icBundle AS CHAR):

   DEFINE VARIABLE ldaOrderDate AS DATE NO-UNDO.
   DEFINE VARIABLE liOrderTime AS INTEGER NO-UNDO. 
   DEFINE VARIABLE ldAmt AS DECIMAL NO-UNDO.

   fSplitTS(Order.CrStamp,
            OUTPUT ldaOrderDate,
            OUTPUT liOrderTime).
 
   FOR FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = gcBrand AND 
             DayCampaign.DCEvent = icBundle,
       FIRST FMItem NO-LOCK WHERE
             FMItem.Brand     = gcBrand AND
             FMItem.FeeModel  = DayCampaign.FeeModel AND
             FMItem.ToDate   >= ldaOrderDate AND
             FMItem.FromDate <= ldaOrderDate:
               ldAmt = FMItem.Amount. 
   END.
   RETURN ldAmt.
END FUNCTION.

FUNCTION fMDUBInOrder RETURNS CHARACTER 
   (INPUT piOrderId AS INT,
    OUTPUT pdFeeAmt AS DEC):

   DEF VAR lcBundle AS CHAR NO-UNDO. 

   FIND FIRST Order WHERE 
              Order.Brand   = gcBrand AND
              Order.OrderID = piOrderId NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE Order THEN RETURN "".
   
   lcBundle = fGetDataBundleInOrderAction(INPUT Order.OrderID,
                                          INPUT "BONO").
   
   IF lcBundle EQ "" THEN RETURN "".
  
   pdFeeAmt = fMDUBFixedFeeAmt(lcBundle).

   RETURN lcBundle.

END FUNCTION.

&ENDIF

