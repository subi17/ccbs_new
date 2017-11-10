/*------------------------------------------------------------------------
  MODULE .......: bundlecreation.p
  TASK .........:
  APPLICATION ..: TMS
  CHANGED ......:
  Version ......: Yoigo
  
  This program creates bundle related data (a data having no link to CLIType)
  ----------------------------------------------------------------------*/  

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

&GLOBAL-DEFINE BTYPE "BundleType"
&GLOBAL-DEFINE BBTYPE "BaseBundleType"
&GLOBAL-DEFINE BBNAME "BaseBundleName"
&GLOBAL-DEFINE BBUNDLE "BaseBundle"
&GLOBAL-DEFINE DATALIMIT "DataLimit"
&GLOBAL-DEFINE VOICELIMIT "VoiceLimit"
&GLOBAL-DEFINE BDESTLIMIT "BDestinationLimit"
&GLOBAL-DEFINE PAYTYPE "PaymentType"
&GLOBAL-DEFINE MFBILLCODE "MonthlyFeeBillCode"
&GLOBAL-DEFINE UPSELL "Upsell"
&GLOBAL-DEFINE BONOSUPPORT "BonoSupport"
&GLOBAL-DEFINE OPTIONAL "OptionalBundle"
&GLOBAL-DEFINE FMDL "FirstMonthDataLimit"
&GLOBAL-DEFINE LMDL "LastMonthDataLimit"
&GLOBAL-DEFINE FMVL "FirstMonthVoiceLimit"
&GLOBAL-DEFINE LMVL "LastMonthVoiceLimit"
&GLOBAL-DEFINE FMBDL "FirstMonthBDestLimit"
&GLOBAL-DEFINE LMBDL "LastMonthBDestLimit"

/* 
   The following three preprocess variables are used only for creating
   FMItems. 
   NOTE! This program only creates FMItems which belogs to "optional" bundles.          
         The optional bundles have "COMMON" pricelist and thus no link
         to the CLIType.
         
         the CommercialFee defined, if it is
         not defined then this program won't create FMItem records!
*/
&GLOBAL-DEFINE COMMFEE "CommercialFee"
&GLOBAL-DEFINE FMFC "FirstMonthFeeCalc"
&GLOBAL-DEFINE LMFC "LastMonthFeeCalc"

DEFINE TEMP-TABLE ttBundle NO-UNDO 
   FIELD FieldName  AS CHARACTER
   FIELD FieldValue AS CHARACTER
   FIELD TypeUse    AS CHARACTER
   FIELD Mandatory  AS LOGICAL
   FIELD ValueList  AS CHARACTER
   FIELD DataType   AS CHARACTER
   .
   
DEFINE TEMP-TABLE ttCharToIntMap NO-UNDO 
   FIELD charval    AS CHARACTER
   FIELD intval     AS INTEGER
   FIELD FieldName  AS CHARACTER
   INDEX FieldName IS PRIMARY UNIQUE FieldName CharVal
   .

FUNCTION fCreatettCharToIntMap RETURNS LOGICAL
   ( icFieldName AS CHARACTER,
     icCharVal   AS CHARACTER,
     iiIntVal    AS INTEGER):
        
   CREATE ttCharToIntMap.
   ASSIGN
      ttCharToIntMap.FieldName = icFieldName
      ttCharToIntMap.charval   = icCharVal
      ttCharToIntMap.intval    = iiIntVal.
      
   RETURN FALSE.

END FUNCTION.


FUNCTION fCharToInt RETURNS INTEGER
   ( icFieldName AS CHARACTER,
     icCharVal   AS CHARACTER):
        
   FIND ttCharToIntMap WHERE
      ttCharToIntMap.FieldName = icFieldName AND
      ttCharToIntMap.charval   = icCharVal
   NO-ERROR.
   
   IF NOT AVAILABLE ttCharToIntMap
   THEN RETURN 0.
   
   RETURN ttCharToIntMap.intval.

END FUNCTION. 


FUNCTION fCreatettBundle RETURNS LOGICAL
   ( icFieldName AS CHARACTER,
     icDataType  AS CHARACTER,
     icTypeUse   AS CHARACTER,      
     icValueList AS CHARACTER,
     ilMandatory AS LOGICAL):

   CREATE ttBundle.
   ASSIGN 
      ttBundle.FieldName  = icFieldName
      ttBundle.TypeUse    = icTypeUse
      ttBundle.Mandatory  = ilMandatory
      ttBundle.ValueList  = icValueList
      ttBundle.DataType   = icDataType.
      
   RETURN FALSE.

END FUNCTION.

fCreatettBundle({&BTYPE}, "CHARACTER", "FixedLine,Mobile", "FixedLine,Mobile", YES).
fCreatettBundle({&PAYTYPE}, "CHARACTER", "FixedLine,Mobile", "Postpaid,Prepaid", YES).
fCreatettBundle({&BBUNDLE}, "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle({&BBNAME}, "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle({&BBTYPE}, "CHARACTER", "FixedLine,Mobile", "ServicePackage,PackageWithCounter,PackagewithoutCounter,Upsell", YES).
fCreatettBundle({&UPSELL}, "CHARACTER", "Mobile", "", NO).
fCreatettBundle({&BONOSUPPORT}, "LOGICAL", "Mobile", "Yes,No,True,False", YES).
fCreatettBundle({&MFBILLCODE}, "CHARACTER", "FixedLine,Mobile", "", YES).
fCreatettBundle({&COMMFEE}, "DECIMAL", "FixedLine,Mobile", "", NO). /* If this is given with value NE 0 then FMItem will be created */
fCreatettBundle({&FMFC}, "CHARACTER", "FixedLine,Mobile", "Full,Relative,UsageBased", NO).
fCreatettBundle({&LMFC}, "CHARACTER", "FixedLine,Mobile", "Full,Relative,UsageBased", NO).
fCreatettBundle({&DATALIMIT}, "DECIMAL", "Mobile", "", NO).
fCreatettBundle({&VOICELIMIT}, "DECIMAL", "FixedLine,Mobile", "", NO).
fCreatettBundle({&BDESTLIMIT}, "DECIMAL", "FixedLine,Mobile", "", YES).
fCreatettBundle({&FMDL}, "CHARACTER", "Mobile", "Full,Relative", YES).
fCreatettBundle({&LMDL}, "CHARACTER", "Mobile", "Full,Relative", YES).
fCreatettBundle({&FMVL}, "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES). 
fCreatettBundle({&LMVL}, "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).
fCreatettBundle({&FMBDL}, "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).
fCreatettBundle({&LMBDL}, "CHARACTER", "FixedLine,Mobile", "Full,Relative", YES).

/* Following three used only in FMItems... */
fCreatettCharToIntMap("FeeCalc","Full",1).
fCreatettCharToIntMap("FeeCalc","Relative",0).
fCreatettCharToIntMap("FeeCalc","UsageBased",2).


fCreatettCharToIntMap("Limit","Full",0).
fCreatettCharToIntMap("Limit","Relative",1).

fCreatettCharToIntMap({&BBTYPE},"ServicePackage",1).
fCreatettCharToIntMap({&BBTYPE},"PackageWithCounter",4).
fCreatettCharToIntMap({&BBTYPE},"Upsell",6).
fCreatettCharToIntMap({&BBTYPE},"PackagewithoutCounter",7).


FUNCTION fGetFieldValue RETURNS CHARACTER
   ( icFieldName  AS CHARACTER ):

   DEFINE BUFFER ttBundle FOR ttBundle.
   
   FIND ttBundle WHERE
      ttBundle.FieldName = icFieldName
   NO-ERROR.
   
   IF NOT AVAILABLE ttBundle
   THEN RETURN "".
   
   RETURN ttBundle.FieldValue.

END FUNCTION.

FUNCTION fCheckStoreBundle RETURNS CHARACTER
   ( icFieldName  AS CHARACTER,
     icFieldValue AS CHARACTER):

   DEFINE VARIABLE liInteger  AS INTEGER NO-UNDO.
   DEFINE VARIABLE ldeDecimal AS DECIMAL NO-UNDO.
   DEFINE VARIABLE llLogical  AS LOGICAL NO-UNDO.

   FIND ttBundle WHERE
      ttBundle.FieldName = icFieldName
   NO-ERROR.
   
   IF NOT AVAILABLE ttBundle
   THEN RETURN SUBSTITUTE("Unknown field name '&1'", icFieldName).
   
   IF ttBundle.FieldValue > ""
   THEN RETURN SUBSTITUTE("Tried to set value to field &1 multiple times", ttBundle.FieldName).
   
   CASE ttBundle.DataType:
      WHEN "INTEGER"
      THEN DO:
         liInteger = INTEGER(icFieldValue) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN RETURN SUBSTITUTE("Cannot set value '&1' to integer field '&2'", icFieldValue, ttBundle.FieldName).
      END. 
      WHEN "DECIMAL"
      THEN DO:
         liInteger = DECIMAL(icFieldValue) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN RETURN SUBSTITUTE("Cannot set value '&1' to decimal field '&2'", icFieldValue, ttBundle.FieldName).
      END.
      WHEN "LOGICAL"
      THEN DO:
         llLogical = LOGICAL(icFieldValue) NO-ERROR.
         IF ERROR-STATUS:ERROR
         THEN RETURN SUBSTITUTE("Cannot set value '&1' to logical field '&2'", icFieldValue, ttBundle.FieldName).
      END.
   END.

   ASSIGN 
      ttBundle.FieldName  = icFieldName
      ttBundle.FieldValue = icFieldValue
      .

   RETURN "".

END FUNCTION.

FUNCTION fValidateBundle RETURNS CHARACTER ():

   DEFINE VARIABLE ldeDataLimit     AS DECIMAL NO-UNDO.
   DEFINE VARIABLE ldeVoiceLimit    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lcBundleType     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBaseBundleType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcPayType        AS CHARACTER NO-UNDO.
   
   ASSIGN
      ldeDataLimit     = DECIMAL(fGetFieldValue({&DATALIMIT}))
      ldeVoiceLimit    = DECIMAL(fGetFieldValue({&VOICELIMIT}))
      lcBundleType     = fGetFieldValue({&BTYPE})
      lcBaseBundleType = fGetFieldValue({&BBTYPE})
      .
   
   IF lcBundleType EQ "FixedLine" AND fGetFieldValue({&PAYTYPE}) EQ "Prepaid"
   THEN RETURN "FixedLine bundle cannot have prepaid payment type".
   
   CASE lcBaseBundleType:
      WHEN "ServicePackage" OR WHEN "PackageWithCounter"
      THEN IF ldeDataLimit = 0 AND ldeVoiceLimit = 0
           THEN RETURN SUBSTITUTE("Wrong &1-contract data with limits provided", lcBaseBundleType).
      WHEN "PackageWithoutCounter"
      THEN IF ldeDataLimit > 0 OR ldeVoiceLimit > 0
           THEN RETURN SUBSTITUTE("Wrong &1-contract data with limits provided", lcBaseBundleType).
   END CASE. 
   
   FOR EACH ttBundle:

      IF LOOKUP(lcBundleType, ttBundle.TypeUse) > 0 AND ttBundle.Mandatory AND ttBundle.FieldValue EQ ""
      THEN RETURN SUBSTITUTE("Field '&1' needs a value.", ttBundle.FieldName).

      IF ttBundle.FieldValue > "" AND
         LOOKUP(lcBundleType,ttBundle.TypeUse) EQ 0
      THEN RETURN SUBSTITUTE("Field '&1' cannot have a value when bundle type is &2", ttBundle.FieldName, lcBundleType).

      IF ttBundle.Mandatory AND
         ttBundle.ValueList > "" AND
         LOOKUP(lcBundleType, ttBundle.TypeUse) > 0 AND
         LOOKUP(ttBundle.FieldValue, ttBundle.ValueList) = 0
      THEN RETURN SUBSTITUTE("Field '&1' needs one of following values '&2'. A value '&3' is invalid.", ttBundle.FieldName, ttBundle.ValueList, ttBundle.FieldValue).

   END.

   RETURN "".

END FUNCTION.

DEFINE INPUT  PARAMETER icBaseFile AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icFile     AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER icSpoolDir AS CHARACTER NO-UNDO.

DEFINE STREAM strin.
DEFINE STREAM BundleLog.

/* ********************  Functions  ******************** */

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   icMessage = "ERROR:" + icMessage.
   
   PUT STREAM BundleLog UNFORMATTED
      TODAY                   " | " 
      STRING(TIME,"HH:MM:SS") " | "
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fTMSCValue RETURNS CHARACTER
   (iTableName AS CHAR,
    iFieldName AS CHAR,
    iCodeName  AS CHAR).

    FIND FIRST TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = iTableName AND
               TMSCodes.FieldName = iFieldName AND
          TRIM(TMSCodes.CodeName) = iCodeName  NO-ERROR.

    IF AVAILABLE TMSCodes THEN RETURN TMSCodes.CodeValue.
    ELSE RETURN "".

END FUNCTION.  

/* ***************************  Main Block  *************************** */
DO ON ERROR UNDO, THROW:  

   RUN pReadBundle.
   
   RUN pStoreBundle.

   RETURN "OK".

   CATCH e AS Progress.Lang.Error:
      OUTPUT STREAM BundleLog TO VALUE(icSpoolDir + icBaseFile + ".log") APPEND.
      fError(e:GetMessage(1)).
      OUTPUT STREAM BundleLog CLOSE.
      UNDO, THROW e.
   END CATCH.
   FINALLY:

   END FINALLY.
END.
/* ***************************  Main End  *************************** */ 

PROCEDURE pReadBundle:   

   DEFINE VARIABLE lcLine      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcError     AS CHARACTER NO-UNDO.

   INPUT STREAM strin FROM VALUE(icFile).

   REPEAT ON ERROR UNDO, THROW:
      
      IMPORT STREAM strin UNFORMATTED lcLine.
 
      lcError = fCheckStoreBundle(TRIM(ENTRY(1,lcLine,";")), TRIM(ENTRY(2,lcLine,";"))).
      
      IF lcError > ""
      THEN UNDO, THROW NEW Progress.Lang.AppError(lcError, 1). 
 
   END.

   lcError = fValidateBundle().   

   IF lcError > ""
   THEN UNDO, THROW NEW Progress.Lang.AppError(lcError, 1).
   
   IF CAN-FIND(FIRST DayCampaign NO-LOCK WHERE DayCampaign.Brand = Syst.Var:gcBrand AND DayCampaign.DCEvent = fGetFieldValue({&BBUNDLE}))
   THEN UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE("Bundle '&1' already exists", fGetFieldValue({&BBUNDLE})), 1). 

   IF CAN-FIND(FIRST FeeModel NO-LOCK WHERE FeeModel.Brand = Syst.Var:gcBrand AND FeeModel.FeeModel = fGetFieldValue({&MFBILLCODE}))    
   THEN UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE("Feemodel '&1' already exists", fGetFieldValue({&MFBILLCODE})), 1).
   
   IF CAN-FIND(FIRST ServiceLimitGroup NO-LOCK WHERE ServiceLimitGroup.Brand = Syst.Var:gcBrand AND ServiceLimitGroup.GroupCode = fGetFieldValue({&BBUNDLE}))
   THEN UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE("ServiceLimitGroup having GroupCode '&1' already exists", fGetFieldValue({&BBUNDLE})), 1).

   CATCH err AS Progress.Lang.Error:
      UNDO, THROW NEW Progress.Lang.AppError('Incorrect input file data' + err:GetMessage(1), 1). 
   END CATCH.

   FINALLY:
      INPUT STREAM strin CLOSE.
   END FINALLY.   


END PROCEDURE.


PROCEDURE pCreateServiceLimit:

   DEFINE OUTPUT PARAMETER icLimitType AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER oiSLSeq     AS INTEGER    NO-UNDO.

   oiSLSeq = 0.

   DEFINE BUFFER bf_ServiceLimit FOR ServiceLimit.

   DEFINE VARIABLE lcGroupCode      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSLCode         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSLName         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liDialType       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liInclUnit       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeInclAmt       AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liFirstMonthCalc AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLastMonthCalc  AS INTEGER   NO-UNDO.
   
   lcGroupCode = fGetFieldValue({&BBUNDLE}).
   
   CASE icLimitType:
      WHEN "Data"
      THEN ASSIGN
              lcSLCode         = lcGroupCode + "_DATA"
              lcSLName         = "Data"
              liDialType       = 7
              liInclUnit       = 4
              ldeInclAmt       = DECIMAL(fGetFieldValue({&DATALIMIT}))
              liFirstMonthCalc = fCharToInt("Limit",fGetFieldValue({&FMDL}))
              liLastMonthCalc  = fCharToInt("Limit",fGetFieldValue({&LMDL}))
              .
      WHEN "Voice"
      THEN ASSIGN
              lcSLCode         = lcGroupCode + "_MIN"
              lcSLName         = "National calls"
              liDialType       = (IF fGetFieldValue({&BTYPE}) EQ "Mobile"
                                  THEN 4
                                  ELSE 1 ) 
              liInclUnit       = 1
              ldeInclAmt       = DECIMAL(fGetFieldValue({&VOICELIMIT}))
              liFirstMonthCalc = fCharToInt("Limit",fGetFieldValue({&FMVL}))
              liLastMonthCalc  = fCharToInt("Limit",fGetFieldValue({&LMVL}))
              .
      WHEN "BDest"
      THEN ASSIGN
              lcSLCode         = lcGroupCode + "_QTY"
              lcSLName         = "BDest"
              liDialType       = (IF fGetFieldValue({&BTYPE}) EQ "Mobile"
                                  THEN 0
                                  ELSE 50)
              liInclUnit       = 7
              ldeInclAmt       = DECIMAL(fGetFieldValue({&BDESTLIMIT}))
              liFirstMonthCalc = fCharToInt("Limit",fGetFieldValue({&FMBDL}))
              liLastMonthCalc  = fCharToInt("Limit",fGetFieldValue({&LMBDL}))
              .
      WHEN "Progressive"
      THEN ASSIGN
              lcSLCode         = lcGroupCode
              lcSLName         = "Data"
              liDialType       = 7
              liInclUnit       = 4 
              ldeInclAmt       = DECIMAL(fGetFieldValue({&DATALIMIT}))
              liFirstMonthCalc = fCharToInt("Limit",fGetFieldValue({&FMDL}))
              liLastMonthCalc  = fCharToInt("Limit",fGetFieldValue({&LMDL}))
              .
   END CASE.

   FIND FIRST ServiceLimit NO-LOCK WHERE
              ServiceLimit.GroupCode = lcGroupCode AND 
              ServiceLimit.SLCode    = lcSLCode    AND 
              ServiceLimit.DialType  = liDialType  AND 
              ServiceLimit.ValidFrom <= TODAY      AND
              ServiceLimit.ValidTo   >= TODAY
   NO-ERROR.

   IF AVAILABLE ServiceLimit
   THEN UNDO, THROW NEW Progress.Lang.AppError
      (SUBSTITUTE("ServiceLimit having " +
                  "GroupCode=&1, SLCode=&2 and DialType=&3 " +
                  "is already defined and active",
                  lcGroupCode, lcSLCode, liDialType), 1). 

   FIND LAST bf_ServiceLimit NO-LOCK USE-INDEX SLSeq NO-ERROR.               
   IF AVAILABLE bf_ServiceLimit THEN 
      ASSIGN oiSLSeq = bf_ServiceLimit.SLSeq + 1.          
   ELSE 
      ASSIGN oiSLSeq = 1.
                  
   CREATE ServiceLimit.
   ASSIGN 
      ServiceLimit.GroupCode      = lcGroupCode
      ServiceLimit.SLCode         = lcSLCode                                                   
      ServiceLimit.SLSeq          = oiSLSeq 
      ServiceLimit.SLName         = lcSLName
      ServiceLimit.DialType       = liDialType
      ServiceLimit.InclAmt        = ldeInclAmt
      ServiceLimit.InclUnit       = liInclUnit
      ServiceLimit.BDestLimit     = IF icLimitType = "VOICE"
                                    THEN INTEGER(fGetFieldValue({&BDESTLIMIT}))
                                    ELSE 0
      ServiceLimit.ValidFrom      = TODAY 
      ServiceLimit.ValidTo        = DATE(12,31,2049)
      ServiceLimit.FirstMonthCalc = liFirstMonthCalc
      ServiceLimit.LastMonthCalc  = liLastMonthCalc
      Servicelimit.Web            = 0.
   
   RETURN "".
      
END PROCEDURE.

PROCEDURE pCreateServiceLimitTarget:
   
   DEFINE INPUT PARAMETER icLimitType AS CHARACTER NO-UNDO.     
   DEFINE INPUT PARAMETER iiSLSeq     AS INTEGER   NO-UNDO. 

   DEFINE VARIABLE lcServiceLMembers AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInSideRate      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcOutSideRate     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liCCN             AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liCount           AS INTEGER   NO-UNDO.

   FIND ServiceLimit NO-LOCK WHERE ServiceLimit.SlSeq = iiSlSeq.

   CASE icLimitType:
      WHEN "Data"
      THEN ASSIGN
              lcServiceLMembers = "14100001"
              lcInSideRate      = ServiceLimit.GroupCode + "_DATA_IN"
              lcOutSideRate     = ServiceLimit.GroupCode + "_DATA_OUT"
              liCCN             = 93
              . 
      WHEN "Voice"
      THEN DO:
         IF fGetFieldValue({&BTYPE}) = "FixedLine"
         THEN ASSIGN
                 lcServiceLMembers = "F10100003"
                 lcInSideRate      = ServiceLimit.GroupCode + "_MIN_IN"
                 lcOutSideRate     = ServiceLimit.GroupCode + "_MIN_OUT"
                 liCCN             = 81
                 .
         ELSE ASSIGN /* Mobile */
                 lcServiceLMembers = "10100001,10100003,10100005,CFOTHER,CFYOIGO"
                 lcInSideRate      = ServiceLimit.GroupCode + "_VOICE_IN"
                 lcOutSideRate     = ServiceLimit.GroupCode + "_VOICE_OUT"
                 liCCN             = 81
                 .
      END. 
      WHEN "BDest"
      THEN DO:
         IF fGetFieldValue({&BTYPE}) = "FixedLine"
         THEN ASSIGN
                 lcServiceLMembers = "F10100005"
                 lcInSideRate      = ServiceLimit.GroupCode + "_QTY_IN"
                 lcOutSideRate     = ServiceLimit.GroupCode + "_QTY_OUT"
                 liCCN             = 81
                 .
         ELSE ASSIGN /* Mobile */
                 lcServiceLMembers = "10100001,10100003,10100005,CFOTHER,CFYOIGO"
                 lcInSideRate      = ServiceLimit.GroupCode + "_VOICE_IN"
                 lcOutSideRate     = ServiceLimit.GroupCode + "_VOICE_OUT"
                 liCCN             = 81
                 .      
      END.
   END.

   DO liCount = 1 TO NUM-ENTRIES(lcServiceLMembers):

      FIND FIRST ServiceLimitTarget NO-LOCK WHERE
                 ServiceLimitTarget.SLSeq = iiSLSeq AND
                 ServiceLimitTarget.ServiceLMember = ENTRY(liCount,lcServiceLMembers)
      NO-ERROR.
       
      IF AVAILABLE ServiceLimitTarget
      THEN UNDO, THROW NEW Progress.Lang.AppError
         (SUBSTITUTE("ServiceLimitTarget having " +
                     "SLSeq=&1 and ServiceLMember=&2 " +
                     "is already defined",
                     iiSLSeq, ENTRY(liCount,lcServiceLMembers)), 1). 
      CREATE ServiceLimitTarget.
      ASSIGN
         ServiceLimitTarget.Slseq          = iiSLSeq
         ServiceLimitTarget.ServiceLMember = ENTRY(liCount,lcServiceLMembers)
         ServiceLimitTarget.InSideRate     = lcInSideRate
         ServiceLimitTarget.OutSideRate    = lcOutSideRate.

   END.
   

   DEFINE VARIABLE lcBDest AS CHARACTER NO-UNDO.
   lcBDest = ServiceLimitTarget.InSideRate + "|" + ServiceLimitTarget.OutSideRate.

   DO liCount = 1 TO 2:
      RUN pCreateBDest(ENTRY(liCount,lcBDest,"|"),
                       SUBSTRING(ENTRY(liCount,lcBDest,"|"),1,LENGTH(ServiceLimit.GroupCode)) +
                       REPLACE(SUBSTRING(ENTRY(liCount,lcBDest,"|"),LENGTH(ServiceLimit.GroupCode) + 1),"_"," "),
                       liCCN).
   END.

END PROCEDURE.

PROCEDURE pCreateProgLimit:

   DEFINE INPUT PARAMETER iiSLSeq      AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER icBDest      AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ideLimitFrom AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER ideLimitTo   AS DECIMAL NO-UNDO.

   FIND ServiceLimit NO-LOCK WHERE ServiceLimit.SlSeq = iiSlSeq.

   FIND FIRST ProgLimit NO-LOCK WHERE
              ProgLimit.GroupCode = ServiceLimit.GroupCode AND
              ProgLimit.SLSeq     = iiSLSeq                AND
              ProgLimit.BDest     = icBDest
   NO-ERROR.
   
   IF AVAILABLE ProgLimit
   THEN UNDO, THROW NEW Progress.Lang.AppError
      (SUBSTITUTE("ProgLimit having " +
                  "GroupCode=&1, SLSeq=&2 and BDest=&2 " +
                  "is already defined",
                  ServiceLimit.GroupCode,
                  iiSLSeq,
                  icBDest), 1).

   CREATE ProgLimit.
   ASSIGN 
      ProgLimit.GroupCode = ServiceLimit.GroupCode
      ProgLimit.SLSeq     = iiSLSeq
      ProgLimit.ValidFrom = TODAY
      ProgLimit.ValidTo   = DATE(12,31,2049)
      ProgLimit.LimitFrom = ideLimitFrom
      ProgLimit.LimitTo   = ideLimitTo
      ProgLimit.BDest     = icBDest.

END PROCEDURE.

PROCEDURE pCreateBDest:

   DEFINE INPUT  PARAMETER icBDest  AS CHARACTER NO-UNDO.            
   DEFINE INPUT  PARAMETER icBDName AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER iiCCN    AS INTEGER   NO-UNDO.            

   DEFINE VARIABLE liBDLValue AS INTEGER INITIAL 1 NO-UNDO.

   DEFINE BUFFER bf_BDest FOR BDest.

   FIND FIRST BDest NO-LOCK WHERE
              BDest.Brand = Syst.Var:gcBrand AND
              BDest.BDest = icBDest          AND
              BDest.FromDate <= TODAY        AND
              BDest.ToDate   >= TODAY
   NO-ERROR.
 
   IF AVAILABLE BDest
   THEN UNDO, THROW NEW Progress.Lang.AppError
      (SUBSTITUTE("BDest where BDest=&1 " +
                  "is already defined and active",
                  icBDest), 1).     
  
   FIND LAST bf_BDest USE-INDEX BDestID NO-LOCK NO-ERROR.    
   IF AVAILABLE bf_BDest
   THEN liBDLValue = bf_BDest.BDestID + 1.

   CREATE BDest. 
   ASSIGN 
      BDest.Brand    = Syst.Var:gcBrand    
      BDest.BDestID  = liBDLValue
      BDest.BDest    = icBDest
      BDest.BDName   = icBDName
      BDest.DestType = 0 
      BDest.CCN      = iiCCN
      BDest.Class    = 1
      BDest.FromDate = TODAY 
      BDest.ToDate   = DATE(12,31,2049).
   
END PROCEDURE.

PROCEDURE pCreateFMItem:

   DEFINE VARIABLE lcBillCode AS CHARACTER NO-UNDO.

   /* NOTE! It is not possible to create normal FMItems here 
            as the PriceList information is not known at this
            point. It is however possible to create fmitems
            having "common" pricelist. The common pricelist
            doesn't have a link to CLIType like normal pricelists
            have. We create the common FMItem for bundles
            which have commercial fee defined. */
   IF DECIMAL(fGetFieldValue({&COMMFEE})) > 0
   THEN DO:
      lcBillCode = fGetFieldValue({&MFBILLCODE}).
      
      IF fGetFieldValue({&FMFC}) = "" OR fGetFieldValue({&LMFC}) = ""
      THEN UNDO, THROW NEW Progress.Lang.AppError
               (SUBSTITUTE("&1 had a value. Then also '&2' and '&3' needs to be defined",
                {&COMMFEE}, {&FMFC}, {&LMFC}), 1). 
      
      FIND FIRST FMItem NO-LOCK WHERE
         FMItem.Brand     = Syst.Var:gcBrand AND
         FMItem.FeeModel  = lcBillCode       AND
         FMItem.PriceList = "COMMON"         AND
         FMItem.BillCode  = lcBillCode       AND
         FMItem.FromDate <= TODAY            AND
         FMItem.ToDate   >= TODAY
      NO-ERROR.

      IF AVAILABLE BDest
      THEN UNDO, THROW NEW Progress.Lang.AppError
         (SUBSTITUTE("FMItem having FeeModel=&1, PriceList=COMMON, BillCode=&2 " +
                     "is already defined and active",
                     lcBillCode, lcBillCode), 1).           

      CREATE FMItem. 
      ASSIGN     
         FMItem.Brand             = Syst.Var:gcBrand
         FMItem.FeeModel          = lcBillCode
         FMItem.BillCode          = lcBillCode
         FMItem.PriceList         = "COMMON"
         FMItem.FromDate          = TODAY       
         FMItem.ToDate            = DATE(12,31,2049)
         FMItem.BillType          = "MF"
         FMItem.Interval          = 1    
         FMItem.BillCycle         = 2
         FMItem.FFItemQty         = 0
         FMItem.FFEndDate         = ?
         FMItem.Amount            = DECIMAL(fGetFieldValue({&COMMFEE}))
         FMItem.FirstMonthBR      = fCharToInt("FeeCalc", fGetFieldValue({&FMFC}))
         FMItem.BrokenRental      = fCharToInt("FeeCalc", fGetFieldValue({&LMFC}))
         FMItem.ServiceLimitGroup = "".
   END.   

END PROCEDURE.

PROCEDURE pStoreBundle:

   DEFINE VARIABLE lcBaseBundle     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBaseBundleType AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeDataLimit     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeVoiceLimit    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeBDestLimit    AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcProcessType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lii              AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liSlSeq          AS INTEGER   NO-UNDO.
      
   ASSIGN
      lcBaseBundle     = fGetFieldValue({&BBUNDLE})
      lcBaseBundleType = fGetFieldValue({&BBTYPE})
      ldeDataLimit     = DECIMAL(fGetFieldValue({&DATALIMIT}))
      ldeVoiceLimit    = DECIMAL(fGetFieldValue({&VOICELIMIT}))
      ldeBDestLimit    = DECIMAL(fGetFieldValue({&BDESTLIMIT}))
      .

   CREATE DayCampaign.
   ASSIGN 
      DayCampaign.Brand           = Syst.Var:gcBrand
      DayCampaign.DCEvent         = lcBaseBundle 
      DayCampaign.DCName          = fGetFieldValue({&BBNAME})
      DayCampaign.PayType         = INTEGER(fTMSCValue("CLIType","PayType",fGetFieldValue({&PAYTYPE}))) 
      DayCampaign.ValidFrom       = TODAY
      DayCampaign.ValidTo         = DATE(12,31,2049)
      DayCampaign.StatusCode      = 1           /* Default value Active */
      DayCampaign.DCType          = STRING(fCharToInt({&BBTYPE}, lcBaseBundleType))
      DayCampaign.CCN             = (IF lcBaseBundleType = "PackageWithCounter" OR
                                        lcBaseBundleType = "Upsell" OR
                                        DayCampaign.PayType = 2
                                     THEN 93
                                     ELSE 0)         
      DayCampaign.InstanceLimit   = (IF lcBaseBundleType = "Upsell" THEN 100 ELSE 1)
      DayCampaign.BillCode        = fGetFieldValue({&MFBILLCODE})
      DayCampaign.InclUnit        = (IF lcBaseBundle BEGINS "CONTFH"
                                     THEN 0
                                     ELSE IF lcBaseBundleType = "PackageWithCounter" OR
                                             lcBaseBundleType = "Upsell" OR
                                             DayCampaign.PayType = 2
                                     THEN 4
                                     ELSE 1) 
      DayCampaign.CalcMethod      = (IF lcBaseBundleType = "PackageWithCounter" OR
                                        DayCampaign.PayType = 2
                                     THEN 4
                                     ELSE 1)  
      DayCampaign.InclStartCharge = YES                          
      DayCampaign.MaxChargeIncl   = 0                            
      DayCampaign.MaxChargeExcl   = 0
      DayCampaign.Effective       = INTEGER(fTMSCValue("Daycampaign","Effective","PerContr"))         
      DayCampaign.DurType         = (IF DayCampaign.PayType NE 2 AND
                                        ( ldeDataLimit > 0 OR
                                          ldeVoiceLimit > 0 OR
                                          ldeBDestLimit > 0 )
                                     THEN 1
                                     ELSE 4)
      DayCampaign.DurMonth        = 0
      DayCampaign.DurUnit         = (IF lcBaseBundle BEGINS "CONTFH" OR
                                        lcBaseBundleType = "PackageWithCounter" OR
                                        DayCampaign.PayType = 2
                                     THEN 0
                                     ELSE 1)
      DayCampaign.WeekDay         = ""
      DayCampaign.BundleUpsell    = fGetFieldValue({&UPSELL})
      DayCampaign.FeeModel        = DayCampaign.BillCode
      DayCampaign.ModifyFeeModel  = ""                          
      DayCampaign.TermFeeModel    = ""                          
      DayCampaign.TermFeeCalc     = 0.
      
   IF ldeDataLimit > 0 THEN   
      RUN pDCServicePackage(lcBaseBundle, "SHAPER", LOGICAL(fGetFieldValue({&BONOSUPPORT}))).

   IF DayCampaign.PayType = 2 THEN 
      RUN pDCServicePackage(lcBaseBundle, "HSDPA", NO).

   CREATE FeeModel.
   ASSIGN 
      FeeModel.Brand    = Syst.Var:gcBrand
      FeeModel.FeeModel = fGetFieldValue({&MFBILLCODE})
      FeeModel.FeeName  = fGetFieldValue({&BBNAME})               
      FeeModel.FMGroup  = 0.

   RUN pCreateFMItem.

   /* It is earlier checked that the ServiceLimitGroup is not
      already available */
   CREATE ServiceLimitGroup.
   ASSIGN 
      ServiceLimitGroup.Brand     = Syst.Var:gcBrand
      ServiceLimitGroup.GroupCode = lcBaseBundle
      ServiceLimitGroup.GroupName = fGetFieldValue({&BBNAME})
      ServiceLimitGroup.ValidFrom = TODAY 
      ServiceLimitGroup.ValidTo   = DATE(12,31,2049).

   IF lcBaseBundleType EQ "PackageWithCounter" AND
      ldeDataLimit > 0
   THEN lcProcessType = "Progressive".  
   
   ELSE IF lcBaseBundleType EQ "ServicePackage"
   THEN DO:
      IF ldeDataLimit > 0
      THEN lcProcessType = "Data".
      
      IF ldeVoiceLimit > 0
      THEN lcProcessType = lcProcessType + "," + "Voice".

      IF ldeBDestLimit > 0 AND ldeVoiceLimit EQ 0
      THEN lcProcessType = lcProcessType + "," + "BDest".     
   END.   
   lcProcessType = TRIM(lcProcessType,",").
   
   DO lii = 1 TO NUM-ENTRIES(lcProcessType) ON ERROR UNDO, THROW:
      RUN pCreateServiceLimit(ENTRY(lii,lcProcessType),
                              OUTPUT liSlSeq).

      IF fGetFieldValue({&PAYTYPE}) EQ "Postpaid"
      THEN DO:

         IF ENTRY(lii,lcProcessType) NE "Progressive"
         THEN RUN pCreateServiceLimitTarget(ENTRY(lii,lcProcessType), liSlSeq).
         ELSE DO:
            RUN pCreateProgLimit(liSlSeq,
                                 "GPRSDATA_" + lcBaseBundle,
                                 0,
                                 ldeDataLimit).
            RUN pCreateBDest("GPRSDATA_" + lcBaseBundle,
                             "GPRS DATA HIGH",
                             93).
            RUN pCreateProgLimit(liSlSeq,
                                 "GPRSDATA2_" + lcBaseBundle,
                                 ldeDataLimit + 0.000001,
                                 999999999.999999).
            RUN pCreateBDest("GPRSDATA2_" + lcBaseBundle,
                             "GPRS DATA SLOW",
                             93).
         END.
         
      END.
      
      ELSE IF ENTRY(lii,lcProcessType) = "Data" /* Prepaid */
      THEN RUN pCreateBDest("GPRSDATA_" + lcBaseBundle,
                            "GPRS Data " + lcBaseBundle,
                            93).
   END.

END PROCEDURE.

PROCEDURE pDCServicePackage:
   DEFINE INPUT PARAMETER icDCEvent     AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER icServPac     AS CHARACTER NO-UNDO.  
   DEFINE INPUT PARAMETER ilBonoSupport AS LOGICAL   NO-UNDO.

   DEFINE VARIABLE liPackageID   AS INTEGER NO-UNDO.
   DEFINE VARIABLE liComponentID AS INTEGER NO-UNDO.    
    
   FIND LAST DCServicePackage USE-INDEX DCServicePackageID NO-LOCK NO-ERROR.    
   IF AVAILABLE DCServicePackage THEN 
      ASSIGN liPackageID = DCServicePackage.DCServicePackageID + 1.
   ELSE 
      ASSIGN liPackageID = 1.
   
   FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID NO-LOCK NO-ERROR.
   IF AVAILABLE DCServiceComponent THEN 
      liComponentID = DCServiceComponent.DCServiceComponentID + 1.
   ELSE 
      liComponentID = 1.    

   CREATE DCServicePackage.
   ASSIGN                            
      DCServicePackage.Brand              = Syst.Var:gcBrand 
      DCServicePackage.DCEvent            = icDCEvent 
      DCServicePackage.DCServicePackageID = liPackageID
      DCServicePackage.ServPac            = icServPac                    
      DCServicePackage.FromDate           = TODAY 
      DCServicePackage.ToDate             = DATE(12,31,2049).   

   IF LOOKUP(icServPac, "SHAPER") > 0 THEN 
   DO:
       CREATE DCServiceComponent.
       ASSIGN 
          DCServiceComponent.DCServicePackageID   = DCServicePackage.DCServicePackageID 
          DCServiceComponent.DCServiceComponentID = liComponentID      
          DCServiceComponent.ServCom              = DCServicePackage.ServPac
          DCServiceComponent.DefValue             = 1
          DCServiceComponent.DefParam             = (IF ilBonoSupport THEN (icDCEvent + "#ADDBUNDLE") ELSE icDCEvent)
          DCServiceComponent.FromDate             = TODAY 
          DCServiceComponent.ToDate               = DATE(12,31,2049).   
    END.

    RETURN "".
       
END PROCEDURE.
