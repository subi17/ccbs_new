/* service.i     10.12.04/aam
  
   procedures for handling mobile services
   ffeecont.i needed (fFeeContract)
   
   changes:      24.08.05/aam ilSolog 
                 29.11.06/aam create subsers when ilSolog=false, otherwise
                              make requests
                 19.12.06/aam use ServCom.ScPosition for HLR order 
                              (inside a package)
                 15.03.07 kl  FIRST CTServPac OF CTServEL changed
                 27.03.07/aam use CTServPac.ToDate,
                              star foreach from CTServPac in pCopyPackage
*/

&IF "{&service_i}" NE "YES"
&THEN

&GLOBAL-DEFINE service_i YES

{Func/ffeecont.i}
{Func/fdss.i}
{Func/fmakemsreq.i}
{Func/sharperconfid.i}
{Mm/ongoing_bundle.i}

DEF TEMP-TABLE ttServCom NO-UNDO
   FIELD ServPac AS CHAR 
   FIELD ServCom AS CHAR
   FIELD DefValue AS INT
   FIELD DefParam AS CHAR
   INDEX ServCom ServCom.

DEF TEMP-TABLE ttServAttr NO-UNDO
   FIELD ServCom AS CHAR
   FIELD ServAttr AS CHAR
   FIELD DefValue AS CHAR
   FIELD ChgAllowed AS LOG
   INDEX ServCom ServCom ServAttr.

FUNCTION fIsBBAllowed RETURNS LOGIC
   (iiMsSeq AS INT,
    ideTS   AS DEC):

   DEF BUFFER MobSub FOR MobSub.

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN RETURN FALSE.

   /* BB is always allowed for TARJ6 */
   IF Mobsub.CliType = "TARJ6" THEN RETURN TRUE.

   /* BB is not allowed */
   IF MobSub.CLIType = "TARJRD1" THEN RETURN FALSE.

   IF fGetActOngoingDataBundles(MobSub.MsSeq,ideTS) = "" THEN DO:
      IF Mobsub.TariffBundle <> "CONTS15" OR
         fGetActiveDSSId(MobSub.CustNum,ideTS) <> "DSS2"
      THEN RETURN FALSE.
   END.

   RETURN TRUE.

END FUNCTION.

PROCEDURE pChangedBBStatus:
   DEF INPUT PARAMETER iiBBStat    AS INT  NO-UNDO.
   DEF INPUT PARAMETER ideActStamp AS DEC  NO-UNDO.
   DEF INPUT PARAMETER icSource    AS CHAR NO-UNDO.
   DEF PARAMETER BUFFER bMsRequest FOR MsRequest.
   DEF PARAMETER BUFFER bMobSub    FOR MobSub.

   DEF VAR liRequest               AS  INT  NO-UNDO.
   DEF VAR lcError                 AS  CHAR NO-UNDO.

   IF ideActStamp = 0 OR ideActStamp = ? THEN ideActStamp = Func.Common:mMakeTS().

   FOR FIRST SubSer WHERE
             SubSer.ServCom = "BB"    AND
             SubSer.MsSeq   = bMobSub.MsSeq AND
             SubSer.SsDate <= TODAY   NO-LOCK:
      IF SubSer.SSStat = 1 THEN DO:
         liRequest = fServiceRequest(bMobSub.MsSeq,
                                     SubSer.ServCom,
                                     iiBBStat,     /* ON/Suspend */
                                     (IF iiBBStat = 1 THEN "4"
                                      ELSE ""),    /* Modify the profile */
                                     ideActStamp,
                                     "",
                                     FALSE, /* fees */
                                     FALSE, /* sms */
                                     "",
                                     icSource,
                                     bMsRequest.MsRequest, /* father request */
                                     FALSE,
                                     OUTPUT lcError).
         IF liRequest = 0 THEN
            Func.Common:mWriteMemo("MobSub",
                            STRING(bMobSub.MsSeq),
                            bMobSub.CustNum,
                            "BB",
                            "Black Berry change request failed; " +
                            lcError).
      END. /* IF SubSer.SSStat = 1 THEN DO: */
   END. /* FOR FIRST SubSer WHERE SubSer.ServCom = "BB" AND */

END PROCEDURE. /* PROCEDURE pChangedBBStatus: */


/* copy default services from clitype to mobsub */
PROCEDURE pDefaultServices:


   DEF INPUT  PARAMETER icCLIType  AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiMSSeq    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idtDate    AS DATE NO-UNDO. 
   DEF INPUT  PARAMETER ilOnlyNew  AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER ilSetFees  AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER ilSolog    AS LOG  NO-UNDO. 
   DEF OUTPUT PARAMETER oiCopied   AS INT  NO-UNDO.

   RUN pCopyPackage(icCLIType,
                    "",
                    "",
                    iiMSSeq,
                    idtDate,
                    ilOnlyNew,
                    ilSetFees,
                    ilSolog,
                    0,
                    FALSE,
                    OUTPUT oiCopied).
  
END PROCEDURE.

/* additional services (outside the basic profile) for a new subscription */
FUNCTION fProfileExtention RETURNS LOGIC
   (icCLIType AS CHAR, 
    iiMsSeq   AS INT,
    idtDate   AS DATE,
    ilSetFees AS LOG):
   
   DEF VAR lcResult  AS CHAR NO-UNDO.
   DEF VAR ldActTime AS DEC  NO-UNDO.
   DEF VAR liValue   AS INT  NO-UNDO.

   DEF BUFFER bServCust  FOR OrderCustomer.
   DEF BUFFER bServOrder FOR Order.
   
   IF idtDate = TODAY 
   THEN ldActTime = Func.Common:mMakeTS().
   ELSE ldActTime = Func.Common:mMake2DT(idtDate,10800).

   /* go through all clitype level packages which are type 2 */
   FOR EACH CTServPac NO-LOCK WHERE
            CTServPac.Brand     = Syst.Var:gcBrand      AND
            CTServPac.CLIType   = icCLIType    AND
            CTServPac.ServType  = 2            AND 
            CTServPac.FromDate <= idtDate      AND
            CTServPac.ToDate   >= idtDate 
   BREAK BY CTServPac.ServPac
         BY CTServPac.FromDate DESC:
         
      /* use newest */
      IF NOT FIRST-OF(CTServPac.ServPac) THEN NEXT.
      
      FOR EACH CTServEl WHERE
               CTServEl.Brand     = Syst.Var:gcBrand            AND
               CTServEl.CLIType   = CTServPac.CLIType  AND
               CTServEl.ServPac   = CTServPac.ServPac  AND
               CTServEl.FromDate >= CTServPac.FromDate AND
               CTServEl.FromDate <= CTServPac.ToDate   AND
               CTServEl.FromDate <= idtDate,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand   = Syst.Var:gcBrand AND
               ServCom.ServCom = CTServEl.ServCom
      BREAK BY ServCom.ScPosition  /* order from component */
            BY CTServEl.ServCom
            BY CTServEl.FromDate DESC:

         /* use newest */
         IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.
 
         liValue = CTServEl.DefValue.

         /* special handling for language */     
         IF CTServEl.ServCom = "LANG" THEN 
         FOR FIRST bServOrder NO-LOCK WHERE
                   bServOrder.MsSeq = iiMsSeq AND
                   bServOrder.OrderType < 2,
             FIRST bServCust OF bServOrder NO-LOCK WHERE
                   bServCust.RowType = bServOrder.UserRole:
                   
            IF bServCust.Language > "" THEN 
               liValue = INTEGER(bServCust.Language) NO-ERROR.
         END.
            
         /* is the current value same */
         FIND FIRST SubSer WHERE 
                    SubSer.MsSeq    = iiMsSeq  AND
                    SubSer.ServCom  = CTServEl.ServCom NO-LOCK NO-ERROR.
         IF NOT AVAILABLE SubSer                OR 
            SubSer.SSStat  NE liValue OR
            SubSer.SSParam NE CTServEl.DefParam
         THEN DO:
            fServiceRequest (iiMsSeq ,     
                             CTServEl.ServCom,
                             liValue,
                             CTServEl.DefParam,
                             ldActTime,
                             "",                /* SalesMan */ 
                             ilSetFees,
                             TRUE,             /* SMS */ 
                             "",
                             "",
                             0,
                             FALSE,
                             OUTPUT lcResult ).
         END. 
      END.
      
   END.
   
END FUNCTION.

FUNCTION fIsPackageOn RETURNS LOGIC
   (iiMsSeq   AS INT,
    icCLIType AS CHAR,
    icPackage AS CHAR,
    idtDate   AS DATE):

   DEF VAR llAlreadyOn AS LOGIC NO-UNDO.
   
   llAlreadyOn = TRUE.
   
   FOR EACH CTServPac NO-LOCK WHERE
            CTServPac.Brand     = Syst.Var:gcBrand   AND
            CTServPac.CLIType   = icCLIType AND
            CTServPac.ServPac   = icPackage AND  
            CTServPac.FromDate <= idtDate   AND
            CTServPac.ToDate   >= idtDate 
   BREAK BY CTServPac.FromDate DESC:
         
      /* use newest */
      IF NOT FIRST(CTServPac.FromDate) THEN LEAVE.
      
      CheckElements:
      FOR EACH CTServEl WHERE
               CTServEl.Brand     = Syst.Var:gcBrand            AND
               CTServEl.CLIType   = CTServPac.CLIType  AND
               CTServEl.ServPac   = CTServPac.ServPac  AND
               CTServEl.FromDate >= CTServPac.FromDate AND
               CTServEl.FromDate <= CTServPac.ToDate   AND
               CTServEl.FromDate <= idtDate
      BREAK BY CTServEl.ServCom
            BY CTServEl.FromDate DESC:

         /* use newest */
         IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.
 
         /* is the current value same */
         FIND FIRST SubSer WHERE 
                    SubSer.MsSeq    = iiMsSeq  AND
                    SubSer.ServCom  = CTServEl.ServCom NO-LOCK NO-ERROR.
         IF NOT AVAILABLE SubSer                OR 
            SubSer.SSStat  NE CTServEl.DefValue OR
            SubSer.SSParam NE CTServEl.DefParam
         THEN DO:
            llAlreadyOn = FALSE.
            LEAVE CheckElements.
         END.

         FOR EACH CtServAttr OF CTServEl NO-LOCK WHERE
                  CtServAttr.FromDate <= idtDate
         BREAK BY CTServAttr.ServAttr
               BY CTServAttr.FromDate DESC:
    
            /* use newest */
            IF NOT FIRST-OF(CTServAttr.ServAttr) THEN NEXT.
         
            /* check if value has changed */
            FIND FIRST SubSerPara WHERE
                       SubSerPara.MSSeq     = iiMSSeq             AND
                       SubSerPara.ServCom   = CTServEl.ServCom    AND
                       SubSerPara.ParaName  = CTServAttr.ServAttr
             NO-LOCK NO-ERROR.
             
            IF NOT AVAILABLE SubSerPara OR
               SubSerPara.ParaValue NE CTServAttr.DefValue
            THEN DO:
               llAlreadyOn = FALSE.
               LEAVE CheckElements.
            END.
         END.

      END.   /* CheckElements */

   END.
      
   RETURN llAlreadyOn.
       
END FUNCTION.
    
    
/* copy one package from clitype to mobsub */
PROCEDURE pCopyPackage:

   DEF INPUT  PARAMETER icCLIType     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icServPac     AS CHAR NO-UNDO. 
   DEF INPUT  PARAMETER icDCEvent     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiMSSeq       AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idtDate       AS DATE NO-UNDO. 
   DEF INPUT  PARAMETER ilOnlyNew     AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER ilSetFees     AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER ilSolog       AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrigRequest AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilMandatory   AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER oiCopied      AS INT  NO-UNDO.

   DEF VAR lcFeeModel          AS CHAR  NO-UNDO.
   DEF VAR liSCnt              AS INT   NO-UNDO.
   DEF VAR lcContract          AS CHAR  NO-UNDO. 
   DEF VAR llCompCopy          AS LOG   NO-UNDO. 
   DEF VAR lcResult            AS CHAR  NO-UNDO.
   DEF VAR liReq               AS INT   NO-UNDO.
   DEF VAR ldeActTime          AS DEC   NO-UNDO FORMAT "99999999.99999".
   DEF VAR ldeEndTime          AS DEC   NO-UNDO FORMAT "99999999.99999".
   DEF VAR llForce             AS LOG   NO-UNDO INIT FALSE.
   DEF VAR liDefValue          AS INT   NO-UNDO.
   DEF VAR lcDefParam          AS CHAR  NO-UNDO.
   DEF VAR liDCPackageID       AS INT   NO-UNDO.
   DEF VAR liDCComponentID     AS INT   NO-UNDO.
   DEF VAR llFound             AS LOG   NO-UNDO.
   DEF VAR lcServPac           AS CHAR  NO-UNDO.
   DEF VAR liUpsellQuota       AS INT64 NO-UNDO.
   DEF VAR liGraceQuota        AS INT64 NO-UNDO.
   DEF VAR ldeUpgradeLimit     AS DEC   NO-UNDO.
   DEF VAR lcTemplate          AS CHAR  NO-UNDO.
   DEF VAR lcBONOContracts     AS CHAR  NO-UNDO.
   DEF VAR lcServSkipList      AS CHAR  NO-UNDO.
   DEF VAR liPeriod            AS INTE  NO-UNDO.
   DEF VAR liRequest           AS INTE  NO-UNDO.
   DEF VAR ldeConsumption      AS DECI  NO-UNDO.
   DEF VAR lcAdjustConsProfile AS CHAR  NO-UNDO.  

   DEF BUFFER bContSub           FOR MobSub.
   DEF BUFFER bDCPackage         FOR DCServicePackage.
   DEF BUFFER bDCComponent       FOR DCServiceComponent.
   DEF BUFFER bDCAttribute       FOR DCServiceAttribute.
   DEF BUFFER bMsRequest         FOR MsRequest.
   DEF BUFFER bf_ServiceLimit    FOR ServiceLimit.
   DEF BUFFER bf_mServiceLimit   FOR mServiceLimit.
   DEF BUFFER bf_ServiceLCounter FOR ServiceLCounter. 

   ASSIGN
      oiCopied = 0
      llFound  = FALSE.

   IF ilOnlyNew = ? THEN ASSIGN
      ilOnlyNew = FALSE
      llForce   = TRUE.
   
   IF idtDate = TODAY 
   THEN ASSIGN
      ldeActTime = Func.Common:mMakeTS()
      ldeEndTime = ldeActTime.
   ELSE ASSIGN
      ldeActTime = Func.Common:mMake2DT(idtDate,0)
      ldeEndTime = Func.Common:mMake2DT(idtDate,86399).

   EMPTY TEMP-TABLE ttServCom.
   EMPTY TEMP-TABLE ttServAttr.
   
   FIND bContSub WHERE bContSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bContSub THEN RETURN "Mobsub not found".

   lcServSkipList = fCParamC("ServSkipList").

   FOR EACH CTServPac NO-LOCK WHERE
            CTServPac.Brand     = Syst.Var:gcBrand   AND
            CTServPac.CLIType   = icCLIType AND
            CTServPac.FromDate <= idtDate   AND
            CTServPac.ToDate   >= idtDate
   BREAK BY CTServPac.ServPac
         BY CTServPac.FromDate DESC:

      /* use newest */
      IF NOT FIRST-OF(CTServPac.ServPac) THEN NEXT.
        
      IF icServPac > "" THEN DO:
         /* copy only profile */
         IF icServPac = "*" THEN DO:
            IF CTServPac.ServType NE 1 THEN NEXT.
         END.
         /* one package */
         ELSE IF CTServPac.ServPac NE icServPac THEN NEXT.
      END.
   
      llFound = TRUE.
        
      FOR EACH CTServEl NO-LOCK WHERE
               CTServEl.Brand     = Syst.Var:gcBrand   AND
               CTServEl.CLIType   = CTServPac.CLIType  AND
               CTServEl.ServPac   = CTServPac.ServPac  AND
               CTServEl.FromDate >= CTServPac.FromDate AND
               CTServEl.FromDate <= CTServPac.ToDate   AND
               CTServEl.FromDate <= idtDate,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand    = Syst.Var:gcBrand          AND
               ServCom.ServCom  = CTServEl.ServCom AND
               ServCom.Target   = 0
      BREAK BY ServCom.ScPosition
            BY CTServEl.ServCom
            BY CTServEl.FromDate DESC:

         /* use newest */
         IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.
      
         IF LOOKUP(CTServEl.ServCom,lcServSkipList) > 0 THEN NEXT.
            
         CREATE ttServCom.
         BUFFER-COPY CTServEl TO ttServCom.
         
         FOR EACH CTServAttr OF CTServEl NO-LOCK WHERE
                  CTServAttr.FromDate <= idtDate
         BREAK BY CTServAttr.ServAttr
               BY CTServAttr.FromDate DESC:
      
            /* use newest */
            IF NOT FIRST-OF(CTServAttr.ServAttr) THEN NEXT.

            CREATE ttServAttr.
            BUFFER-COPY CTServAttr TO ttServAttr.
         END.    
      END.
   END.       

   /* if not defined to clitype then check if there is a general package
      definition (e.g. bundles) 
   IF NOT llFound AND liDcPackageID > 0 THEN DO:
      RUN pGetGeneralPackage(icServPac).
   END.
   */
    
   FOR EACH ttServCom:
         
      ASSIGN 
         llCompCopy = TRUE
         liDCComponentID = 0
         lcServPac = ttServCom.ServPac.
          
      FIND FIRST SubSer WHERE
                 SubSer.MSSeq   = iiMSSeq          AND
                 SubSer.ServCom = ttServCom.ServCom NO-LOCK NO-ERROR.

      /* are only new components copied */
      IF ilOnlyNew THEN DO:
         IF AVAILABLE SubSer THEN llCompCopy = FALSE.
      END.
  
      /* otherwise check if value has changed */
      ELSE DO:
         /* no need to copy if value remains the same */           
         IF AVAILABLE SubSer AND NOT llForce THEN DO:
            IF SubSer.SSStat = ttServCom.DefValue
            THEN llCompCopy = FALSE.
            /* if there already is a definition and value is not changeable 
               on subscription level then override it */
            ELSE IF SubSer.SSDate = idtDate THEN DO:
                  llCompCopy = TRUE.
            END.
         END.
      END.   

      IF llCompCopy THEN DO: 

         ASSIGN 
            liDefValue = ttServCom.DefValue
            lcDefParam = ttServCom.DefParam
            liDCPackageID = 0.

         /* are there default values defined on per.contract */
         IF icDCEvent > "" THEN 
         FOR FIRST bDCPackage NO-LOCK WHERE
                   bDCPackage.Brand     = Syst.Var:gcBrand AND
                   bDCPackage.DCEvent   = icDCEvent AND
                   bDCPackage.ServPac   = ttServCom.ServPac AND
                   bDCPackage.ToDate   >= idtDate AND
                   bDCPackage.FromDate <= idtDate,
             FIRST bDCComponent NO-LOCK WHERE
                   bDCComponent.DCServicePackageID = 
                      bDCPackage.DCServicePackageID AND
                   bDCComponent.ServCom = ttServCom.ServCom AND
                   bDCComponent.ToDate   >= idtDate AND
                   bDCComponent.FromDate <= idtDate:
            ASSIGN
               liDefValue = bDCComponent.DefValue
               lcDefParam = bDCComponent.DefParam
               liDCComponentID = bDCComponent.DCServiceComponentID
               liDcPackageID = bDCPackage.DCServicePackageID       
               lcServPac  = "".
         END.

         IF ttServCom.ServPac BEGINS "SHAPER" THEN
            lcDefParam = fGetShaperConfId(iiMSSeq,
                             icDCEvent,
                             lcDefParam,
                             idtDate,
                             icclitype).

         IF INDEX(lcDefParam,"#UPSELL") > 0 THEN DO:
            lcDefParam = REPLACE(lcDefParam,"#UPSELL","").

            /* Check Upgrade Upsell */
            IF INDEX(lcDefParam,"UPGRADE") > 0 THEN DO:
               lcDefParam = REPLACE(lcDefParam,"UPGRADE","").
               FOR FIRST bMsRequest WHERE
                         bMsRequest.MsRequest = iiOrigRequest NO-LOCK,
                   FIRST TMSCodes WHERE
                         TMSCodes.TableName = "UpsellUpgrade" AND
                         TMSCodes.FieldName = "BundleLimit"   AND
                         TMSCodes.CodeValue = bMsRequest.ReqCParam5 NO-LOCK:
                   ldeUpgradeLimit = DEC(TMSCodes.ConfigValue) * 1024 * 1024.
                   lcDefParam = STRING(ldeUpgradeLimit) + lcDefParam.
               END. /* FOR FIRST bMsRequest WHERE */
            END. /* IF INDEX(lcDefParam,"UPGRADE") > 0 THEN DO: */

            IF INDEX(lcDefParam,"#GRACELIMIT") > 0 THEN DO:

               IF NUM-ENTRIES(lcDefParam) >= 2 THEN
                  ASSIGN lcTemplate = ENTRY(2,lcDefParam)
                         lcDefParam = ENTRY(1,lcDefParam).

               lcDefParam = REPLACE(lcDefParam,"#GRACELIMIT","").
               liUpsellQuota = INT64(lcDefParam) NO-ERROR.
               IF ERROR-STATUS:ERROR OR liUpsellQuota = ? THEN
                  liUpsellQuota = 0.

               IF liUpsellQuota > 0 THEN
                  liGraceQuota = (liUpsellQuota * 5) / 100.

               lcDefParam = STRING(liUpsellQuota) +
                            ",GRACE=" + STRING(liGraceQuota).

               IF lcTemplate > "" THEN
                  lcDefParam = lcDefParam + "," + lcTemplate.
            END. /* IF INDEX(lcDefParam,"#GRACELIMIT") > 0 THEN DO: */
         END. /* IF INDEX(lcDefParam,"#GRACELIMIT") > 0 THEN DO: */

         IF lcDefParam = "TARJ7" THEN
            FOR FIRST ServiceLimit NO-LOCK WHERE
                      ServiceLimit.GroupCode = lcDefParam,
                FIRST MserviceLimit NO-LOCK WHERE
                      MserviceLimit.MsSeq = iiMsSeq AND
                      MserviceLimit.DialType = ServiceLimit.DialType AND
                      MserviceLimit.SLSeq = ServiceLimit.SLSeq AND
                      MserviceLimit.EndTS >= ldeEndTime AND
                      MserviceLimit.InclAmt = 1228:
               lcDefParam = lcDefParam + "_PROMO".
            END.

         IF LOOKUP(lcDefParam,"TARJ7,TARJ9") > 0 AND
            CAN-FIND(FIRST Order NO-LOCK WHERE
                           Order.MsSeq = iiMsSeq AND
                           Order.CLIType = lcDefParam AND
                           Order.Crstamp >= 20170301 AND
                           Order.Crstamp < 20170405.25200 AND
                           Order.OrderType < 2) THEN
             lcDefParam = lcDefParam + "_DOUBLE".
            
         /* if solog update needed then create a msrequest from change */
         IF ilSolog THEN 
         DO:
            /* RELAX BONOs */
            IF INDEX(lcDefParam,"RELAX") > 0 THEN
            DO:
                ASSIGN liPeriod = INT(SUBSTRING(STRING(ldeActTime),1,6)).

                FOR EACH bf_ServiceLimit WHERE bf_ServiceLimit.GroupCode = "MM_DATA600" AND bf_ServiceLimit.ValidTo >= TODAY NO-LOCK,
                    FIRST bf_mServiceLimit WHERE bf_mServiceLimit.MsSeq    = iiMSSeq                  AND 
                                                 bf_mServiceLimit.DialType = bf_ServiceLimit.DialType AND
                                                 bf_mServiceLimit.SLSeq    = bf_ServiceLimit.SLSeq    AND
                                                 bf_mServiceLimit.EndTS   >= ldeActTime               NO-LOCK,
                    FIRST bf_ServiceLCounter WHERE bf_ServiceLCounter.MsSeq  = iiMSSeq                AND
                                                   bf_ServiceLCounter.Period = liPeriod               AND
                                                   bf_ServiceLCounter.SlSeq  = bf_mServiceLimit.SLSeq AND 
                                                   bf_ServiceLCounter.MSID   = bf_mServiceLimit.MSID  NO-LOCK:
                    IF bf_ServiceLCounter.Amt > 0 THEN 
                    DO:
                        ASSIGN ldeConsumption = (bf_ServiceLCounter.Amt / 1024 / 1024).

                        IF ldeConsumption > bf_ServiceLimit.InclAmt THEN
                           ldeConsumption = (bf_ServiceLimit.InclAmt * 1024 * 1024).
                        ELSE
                           ldeConsumption = bf_ServiceLCounter.Amt.

                        ASSIGN lcAdjustConsProfile = STRING(-1 * ldeConsumption) + ",GRACE=0" + ",TEMPLATE=HSPA".

                        liRequest = fServiceRequest(iiMSSeq,
                                                    ttServCom.ServCom,
                                                    1, /* on */
                                                    lcAdjustConsProfile,
                                                    ldeActTime,
                                                    "", /* salesman */
                                                    FALSE, /* fees */
                                                    FALSE, /* sms */
                                                    "", /* creator */
                                                    "",
                                                    iiOrigRequest, /* father request */
                                                    ilMandatory, /* mandatory for father request */
                                                    OUTPUT lcResult).
                        IF liRequest = 0 THEN
                        DO:
                          Func.Common:mWriteMemo("MobSub",
                                           STRING(iiMSSeq),
                                           bContSub.CustNum,
                                           "Consumption adjustment failed;",
                                           lcResult).
                          ASSIGN lcResult = "".
                        END.  
                    END. /* IF bf_ServiceLCounter.Amt > 0 */
                END.
            END.

            liReq = fServiceRequest (iiMsSeq ,     
                                     ttServCom.ServCom,
                                     liDefValue,
                                     lcDefParam,
                                     ldeActTime,
                                     "",                /* SalesMan */ 
                                     ilSetFees,
                                     TRUE,             /* SMS */ 
                                     "",
                                     "",     /* source */
                                     iiOrigRequest,
                                     ilMandatory,
                                     OUTPUT lcResult).
            IF liReq = 0 THEN NEXT.
         END.

         /* otherwise create subsers directly (new subscription) */
         ELSE DO:
            FIND FIRST SubSer WHERE 
                 SubSer.MsSeq   = iiMsSeq AND
                 SubSer.ServCom = ttServCom.ServCom AND
                 SubSer.SSDate  = idtDate EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE SubSer THEN DO:
               CREATE SubSer.
               ASSIGN SubSer.MSSeq   = iiMSSeq
                      SubSer.ServPac = lcServPac
                      SubSer.ServCom = ttServCom.ServCom
                      SubSer.SSDate  = idtDate.
            END. 
            ASSIGN SubSer.SSStat  = liDefValue
                   SubSer.SSParam = lcDefParam.
         END.
         
         /* list of fee models for fee creation */
         IF ilSetFees AND ttServCom.DefValue  > 0 THEN DO:
            FIND FIRST ServPac WHERE
                 ServPac.Brand   = Syst.Var:gcBrand AND
                 ServPac.ServPac = ttServCom.ServPac NO-LOCK NO-ERROR.

            IF AVAILABLE ServPac AND ServPac.FeeModel > "" THEN DO:
         
               IF LOOKUP(ServPac.FeeModel,lcFeeModel) = 0
               THEN lcFeeModel = lcFeeModel + ServPac.FeeModel + ",".
            END.
         END. 
         
         oiCopied = oiCopied + 1.
         
      END.
      
      FOR EACH ttServAttr WHERE
               ttServAttr.ServCom = ttServCom.ServCom:
               
         lcDefParam = ttServAttr.DefValue.
               
         /* are there default values defined on per.contract */
         IF liDcPackageID > 0 THEN 
         FOR FIRST bDCAttribute NO-LOCK WHERE
                   bDCAttribute.DCServiceComponentID = liDCComponentID AND
                   bDCAttribute.ServAttr  = ttServAttr.ServAttr AND
                   bDCAttribute.ToDate   >= idtDate AND
                   bDCAttribute.FromDate <= idtDate:
            lcDefParam = bDCAttribute.DefParam.
         END.
          
         /* are only new parameters copied */
         IF ilOnlyNew THEN DO:
            IF CAN-FIND(FIRST SubSerPara WHERE
                              SubSerPara.MSSeq     = iiMSSeq             AND
                              SubSerPara.ServCom   = ttServAttr.ServCom  AND
                              SubSerPara.ParaName  = ttServAttr.ServAttr)
            THEN NEXT.
         END.
         
         /* otherwise check if value has changed */
         ELSE DO:
             FIND FIRST SubSerPara WHERE
                        SubSerPara.MSSeq     = iiMSSeq             AND
                        SubSerPara.ServCom   = ttServAttr.ServCom  AND
                        SubSerPara.ParaName  = ttServAttr.ServAttr
             NO-LOCK NO-ERROR.
             
            /* no need to copy if value remains the same */           
            IF AVAILABLE SubSerPara THEN DO:
               IF SubSerPara.ParaValue = lcDefParam THEN NEXT.
         
               /* if there already is a definition and value is not 
                  changeable on subscription level then override it */
               ELSE IF SubSerPara.SSDate = idtDate THEN DO:
                  IF NOT ttServAttr.ChgAllowed THEN DO:
                     FIND CURRENT SubSerPara EXCLUSIVE-LOCK.
                     DELETE SubSerPara.
                  END.
                  ELSE NEXT.
               END.
            END.

         END.
                           
         CREATE SubSerPara.
         ASSIGN SubSerPara.MSSeq     = iiMSSeq
                SubSerPara.ServCom   = ttServAttr.ServCom
                SubSerPara.ParaName  = ttServAttr.ServAttr
                SubSerPara.SSDate    = idtDate
                SubSerPara.ParaValue = lcDefParam.
            
      END.   /* ttservattr */

   END.  /* ttservcom */

   /* create fees according to packages' feemodels */
   IF ilSetFees THEN DO:
   
      /* contract */
      lcContract = fFeeContract(Syst.Var:gcBrand,
                                bContSub.CustNum,
                                "",  
                                idtDate,
                                "Service param.").

      DO liSCnt = 1 TO NUM-ENTRIES(lcFeeModel):
   
         IF ENTRY(liSCnt,lcFeeModel) = "" THEN NEXT.
      
         RUN Mm/setfees.p(ENTRY(liSCnt,lcFeeModel),
                     iiMsSeq,
                     YEAR(idtDate) * 100 + MONTH(idtDate),
                     "Mobsub",
                     STRING(iiMsSeq),
                     0,
                     FALSE).
      END.
                     
   END.

   EMPTY TEMP-TABLE ttServCom.
   EMPTY TEMP-TABLE ttServAttr.
    
END PROCEDURE.

PROCEDURE pTerminatePackage:

   DEF INPUT  PARAMETER icNewCLIType  AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icOldCLIType  AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icServPac     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icOrigDCEvent AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER icBundleList  AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER iiMSSeq       AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idEndStamp    AS DEC  NO-UNDO. 
   DEF INPUT  PARAMETER ilSetFees     AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER ilSolog       AS LOG  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrigRequest AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilMandatory   AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER oiTerminated  AS INT  NO-UNDO.

   DEF VAR lcFeeModel AS CHAR NO-UNDO.
   DEF VAR lcContract AS CHAR NO-UNDO. 
   DEF VAR liReq      AS INT  NO-UNDO.
   DEF VAR lcResult   AS CHAR NO-UNDO.
   DEF VAR ldaEndDate AS DATE NO-UNDO.
   DEF VAR liEndTime  AS INT  NO-UNDO.
   DEF VAR liCount    AS INT  NO-UNDO.
   DEF VAR llFound    AS LOG  NO-UNDO.
   DEF VAR lcRetainBundle AS CHAR NO-UNDO.
   DEF VAR llCommonComponent AS LOG NO-UNDO. 
   DEF VAR lcParam    AS CHAR NO-UNDO.
   
   DEF BUFFER bContSub FOR MobSub.
   DEF BUFFER bDCPackage FOR DCServicePackage.
   DEF BUFFER bDCComponent FOR DCServiceComponent.
   
   ASSIGN
      oiTerminated = 0
      llFound      = FALSE.

   /* profile cannot be removed with this */
   IF LOOKUP(icServPac,",*") > 0 THEN RETURN.
   
   Func.Common:mSplitTS(idEndStamp,
            OUTPUT ldaEndDate,
            OUTPUT liEndTime).
            
   EMPTY TEMP-TABLE ttServCom.
   EMPTY TEMP-TABLE ttServAttr.
 
   FOR EACH CTServPac NO-LOCK WHERE
            CTServPac.Brand     = Syst.Var:gcBrand      AND
            CTServPac.CLIType   = icOldCLIType AND
            CTServPac.ServPac   = icServPac    AND 
            CTServPac.FromDate <= ldaEndDate   AND
            CTServPac.ToDate   >= ldaEndDate
   BREAK BY CTServPac.ServPac
         BY CTServPac.FromDate DESC:

      /* use newest */
      IF NOT FIRST-OF(CTServPac.ServPac) THEN NEXT.
        
      llFound = TRUE.
            
      FOR EACH CTServEl NO-LOCK WHERE
               CTServEl.Brand     = Syst.Var:gcBrand   AND
               CTServEl.CLIType   = CTServPac.CLIType  AND
               CTServEl.ServPac   = CTServPac.ServPac  AND
               CTServEl.FromDate >= CTServPac.FromDate AND
               CTServEl.FromDate <= CTServPac.ToDate   AND
               CTServEl.FromDate <= ldaEndDate,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand    = Syst.Var:gcBrand          AND
               ServCom.ServCom  = CTServEl.ServCom AND
               ServCom.Target   = 0
      BREAK BY ServCom.ScPosition
            BY CTServEl.ServCom
            BY CTServEl.FromDate DESC:

         /* use newest */
         IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.

         CREATE ttServCom.
         BUFFER-COPY CTServEl TO ttServCom.
      END.
   END.      

   /* if not defined to clitype then check if there is a general package
      definition (e.g. bundles) 
   IF NOT llFound THEN DO:
      RUN pGetGeneralPackage(icServPac).
   END.
   */    

   TERMINATECOMPONENT:
   FOR EACH ttServCom:
   
      FIND FIRST SubSer WHERE
                 SubSer.MSSeq   = iiMSSeq AND
                 SubSer.ServCom = ttServCom.ServCom NO-LOCK NO-ERROR.

      IF icBundleList > "" THEN 
      RETAINCOMPONENT:
      DO liCount = 1 TO NUM-ENTRIES(icBundleList):
        
         llCommonComponent = FALSE.

         FOR FIRST bDCPackage NO-LOCK WHERE
                   bDCPackage.Brand     = Syst.Var:gcBrand AND
                   bDCPackage.DCEvent   = ENTRY(liCount,icBundleList) AND
                   bDCPackage.ServPac   = ttServCom.ServPac AND
                   bDCPackage.ToDate   >= ldaEndDate + 1 AND
                   bDCPackage.FromDate <= ldaEndDate + 1,
             FIRST bDCComponent NO-LOCK WHERE
                   bDCComponent.DCServicePackageID = 
                      bDCPackage.DCServicePackageID AND
                   bDCComponent.ServCom  = ttServCom.ServCom AND
                   bDCComponent.ToDate   >= ldaEndDate + 1 AND
                   bDCComponent.FromDate <= ldaEndDate + 1:
                   
            /* if another bundle has the same component with same value 
               then no need to do anything */
            IF AVAIL SubSer AND
               bDCComponent.DefValue = SubSer.SSStat AND
               bDCComponent.DefParam = SubSer.SSParam THEN
               NEXT TERMINATECOMPONENT.
       
           /* shaper needs special handling; if there has been several 
              bundles (not an upsell) and one has now been terminated, then 
              shaper needs to be activated again for the remaining bundle */
            ELSE IF AVAIL SubSer AND
               bDCComponent.DefValue = SubSer.SSStat AND
               bDCComponent.DefParam NE SubSer.SSParam THEN DO:
               ASSIGN
                  lcRetainBundle = bDCPackage.DCEvent
                  llCommonComponent = TRUE.
            END.
         END.
      END.

      /* if solog update needed then create a msrequest from change.     */
      /* Note - SHAPER=0 will not be used anymore, if we are terminating */
      /* the Bundle and retain bundle list is empty then make sure       */
      /* DEFAULT profile should be created from next month.              */
      IF ilSolog THEN DO:
         
         IF ttServCom.ServCom = "SHAPER"
         THEN lcParam = ttServCom.DefParam.
         ELSE IF AVAIL SubSer THEN lcParam = SubSer.SSParam.
         ELSE lcParam = ttServCom.DefParam.
      
         /* YTS-8017 */
         /* During prepaid STC/iSTC, while onging contract termination request 
            If any contract activation is done WITH new clitype, THEN avoid 
            creating termination SHAPER profile service request WITH new clitype  */

         IF ttServCom.ServCom = "SHAPER"   AND 
            icNewCLIType NE icOldCLIType   AND
            CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.CLIType EQ icNewCLIType AND
                           CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID}) AND
            CAN-FIND(FIRST CLIType NO-LOCK WHERE
                           CLIType.CLIType EQ icOldCLIType AND
                           CLIType.PayType EQ {&CLITYPE_PAYTYPE_PREPAID}) AND
            AVAILABLE SubSer               AND 
            icNewCLIType EQ SubSer.SSParam THEN 
            NEXT TERMINATECOMPONENT.

         IF NOT llCommonComponent THEN DO:

            liReq = fServiceRequest (iiMsSeq ,     
                                     ttServCom.ServCom,
                                     (IF ttServCom.ServCom = "SHAPER" THEN 1
                                      ELSE 0),
                                     lcParam,
                                     (IF ttServCom.ServCom = "SHAPER" OR
                                         ttServCom.ServCom = "HSDPA" THEN
                                         Func.Common:mSecOffSet(idEndStamp,1)
                                      ElSE idEndStamp),
                                     "",                /* SalesMan */ 
                                     ilSetFees,
                                     TRUE,             /* SMS */ 
                                     "",
                                     "",
                                     iiOrigRequest,
                                     ilMandatory,
                                     OUTPUT lcResult).

            IF liReq > 0 THEN oiTerminated = oiTerminated + 1.
         END. /* IF lcRetainBundle = "" THEN DO: */
      END. /* IF ilSolog THEN DO: */

      /* update subsers directly */
      ELSE DO:
         FIND FIRST SubSer WHERE 
            SubSer.MsSeq   = iiMsSeq AND
            SubSer.ServCom = ttServCom.ServCom AND
            SubSer.SSDate  = ldaEndDate EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAILABLE SubSer THEN DO:
            CREATE SubSer.
            ASSIGN SubSer.MSSeq   = iiMSSeq
                   SubSer.ServPac = ttServCom.ServPac
                   SubSer.ServCom = ttServCom.ServCom
                   SubSer.SSDate  = ldaEndDate.
         END. 
         ASSIGN 
            SubSer.SSStat = 0
            oiTerminated = oiTerminated + 1.
      END.

      IF ServCom.ClFeeModel > "" THEN 
         lcFeeModel = lcFeeModel + (IF lcFeeModel > "" THEN "," ELSE "") +
                      ServCom.ClFeeModel.
   END.    

   /* create fees according to packages' feemodels */
   IF ilSetFees THEN DO:
   
      /* contract */
      FIND bContSub WHERE bContSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF AVAILABLE bContSub THEN
      lcContract = fFeeContract(Syst.Var:gcBrand,
                                bContSub.CustNum,
                                "",  
                                ldaEndDate,
                                "Service param.").

      DO liCount = 1 TO NUM-ENTRIES(lcFeeModel):
   
         IF ENTRY(liCount,lcFeeModel) = "" THEN NEXT.
      
         RUN Mm/setfees.p(ENTRY(liCount,lcFeeModel),
                     iiMsSeq,
                     YEAR(ldaEndDate) * 100 + MONTH(ldaEndDate),
                     "Mobsub",
                     STRING(iiMsSeq),
                     0,
                     FALSE).
      END.
                     
   END.

   /* YTS-8017 */
   /* During prepaid STC/iSTC ongoing termination request processing, it has 
      to avoid creating Data bundle clitypes TARJ7, TARJ9 (Newclitype) service termination request */

   IF lcRetainBundle > "" AND 
      LOOKUP(lcRetainBundle, "TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") = 0 THEN DO:
      /* SHAPER should be created from 1st day of next month */
      RUN pCopyPackage(icNewCLIType,
                       icServPac,
                       lcRetainBundle,
                       iiMSSeq,
                       /* used for staging, otherwise TODAY would be ok */
                       (IF DAY(ldaEndDate) EQ 1 AND liEndTime <> 86399
                        THEN ldaEndDate ELSE ldaEndDate + 1),
                       ?,      /* all, forced  */
                       FALSE,  /* create fees */
                       TRUE,   /* solog (provisioning) */
                       iiOrigRequest,
                       TRUE,   /* mandatory subrequest */
                       OUTPUT liCount). 
      IF liCount > 0 THEN oiTerminated = oiTerminated + 1.
   END.
    
END PROCEDURE.

PROCEDURE pGetGeneralPackage:

   DEF INPUT  PARAMETER icServPac AS CHAR NO-UNDO. 

   FOR EACH ServEl NO-LOCK WHERE
            ServEl.Brand   = Syst.Var:gcBrand AND
            ServEl.ServPac = icServPac,
      FIRST ServCom NO-LOCK WHERE
            ServCom.Brand   = Syst.Var:gcBrand AND
            ServCom.ServCom = ServEl.ServCom AND
            ServCom.Target  = 0:
             
      CREATE ttServCom.
      BUFFER-COPY ServEl TO ttServCom.                
      ttServCom.DefValue = ServEl.SEValue.
      
      IF ServCom.ServAttr THEN 
      FOR EACH ServAttr NO-LOCK WHERE
               ServAttr.Brand   = Syst.Var:gcBrand AND
               ServAttr.ServCom = ServCom.ServCom:
         CREATE ttServAttr.
         BUFFER-COPY ServAttr TO ttServAttr.
         ttServAttr.ChgAllowed = ServAttr.ScChgable.
      END.
   END.          

END PROCEDURE.

&ENDIF
