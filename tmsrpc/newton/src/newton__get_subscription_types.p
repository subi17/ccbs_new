/**
 * A mobsub object
 *
 * @input       cli_type;string;optional;Current cli type
                bundle_id;string;optional;Current bundle id
 * @output      clitypes;array of structs;
 * @clitypes    cli_type;string;mandatory;
                tariff_bundle;string;mandatory;
                status_code;int;mandatory;(0=Inactive,1=active,2=retired)
 */
USING Progress.Json.ObjectModel.*.
USING OpenEdge.Net.HTTP.MethodEnum.

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/cparam2.i}
{Func/fixedlinefunc.i}
{Func/extralinefunc.i}

DEF VAR pcCliType     AS CHAR NO-UNDO.
DEF VAR pcTenant      AS CHAR NO-UNDO.
DEF VAR pcBundleId    AS CHAR NO-UNDO.
DEF VAR pcInputStruct AS CHAR NO-UNDO.
DEF VAR lcInputFields AS CHAR NO-UNDO.
DEF VAR piMsSeq       AS INT NO-UNDO.
/* Output parameters */
DEF VAR top_struct         AS CHAR NO-UNDO.
DEF VAR result_array       AS CHAR NO-UNDO.
DEF VAR sub_struct         AS CHAR NO-UNDO.
DEF VAR lcMandatoryELines  AS CHAR NO-UNDO.
DEF VAR ldaCont15PromoEnd  AS DATE NO-UNDO. 
DEF VAR lcStatusCode       AS INT  NO-UNDO.
DEF VAR liMLMsSeq          AS INT  NO-UNDO.

DEFINE VARIABLE lcDownSpeed            AS CHAR  NO-UNDO.
DEFINE VARIABLE lcUpSpeed              AS CHAR  NO-UNDO.
DEFINE VARIABLE liDownSpeed            AS INTE  NO-UNDO.
DEFINE VARIABLE liUpSpeed              AS INTE  NO-UNDO.
DEFINE VARIABLE lcSpeedProfileList     AS CHAR  NO-UNDO.
DEFINE VARIABLE liDestDowspeconversion AS INT64 NO-UNDO.
DEFINE VARIABLE liDestupspeconversion  AS INT64 NO-UNDO.
DEFINE VARIABLE lcHostname             AS CHAR  NO-UNDO.
DEFINE VARIABLE llUseApi               AS LOGI  NO-UNDO INIT TRUE.

DEFINE TEMP-TABLE ttSpeed
          FIELD Download      AS INT64
          FIELD Upload        AS INT64
          INDEX ix IS UNIQUE PRIMARY Download Upload ASCENDING.

DEF BUFFER bCLIType        FOR CLIType.
DEF BUFFER oldCLIType      FOR CLIType.
DEF BUFFER bfMobSub        FOR MobSub.

FUNCTION fSpeedConversion RETURNS INT64
    (INPUT icSpeed  AS CHAR):

     IF INDEX(icSpeed,"K") > 0 THEN
     DO:
        ASSIGN icSpeed = REPLACE(icSpeed,"K","").

        RETURN (INTEGER(icSpeed) * 1024).
     END.       
     ELSE IF INDEX(icSpeed,"M") > 0 THEN
     DO:
        ASSIGN icSpeed = REPLACE(icSpeed,"M","").
        
        RETURN (INTEGER(icSpeed) * 1024 * 1024).

     END.       
     ELSE IF INDEX(icSpeed,"G") > 0 THEN
     DO:
        ASSIGN icSpeed = REPLACE(icSpeed,"G","").

        RETURN (INTEGER(icSpeed) * 1024 * 1024 * 1024).
     END. 
     ELSE      
        RETURN INTEGER(icSpeed).

END FUNCTION.

FUNCTION fParseSpeedProfile RETURNS LOGICAL
   (INPUT icSpeedProfileList AS CHAR):

   DEF VAR liCount     AS INTE  NO-UNDO.
   DEF VAR lcSpeed     AS CHAR  NO-UNDO.
   DEF VAR liDownSpeed AS INT64 NO-UNDO.
   DEF VAR liUpSpeed   AS INT64 NO-UNDO.

   DO liCount = 1 TO NUM-ENTRIES(icSpeedProfileList, "||"):

       ASSIGN 
           lcSpeed     = TRIM(ENTRY(liCount,icSpeedProfileList,"||"))
           liUpSpeed   = 0
           liDownSpeed = 0.

       IF lcSpeed <> "" THEN 
           ASSIGN 
               liDownSpeed = fSpeedConversion(ENTRY(1,lcSpeed,"_"))
               liUpSpeed   = fSpeedConversion(ENTRY(2,lcSpeed,"_")).

       IF liDownSpeed <> 0 OR liUpSpeed <> 0 THEN
       DO:        
           FIND FIRST ttSpeed WHERE ttSpeed.Download = liDownSpeed AND ttSpeed.Upload = liUpSpeed NO-LOCK NO-ERROR.
           IF NOT AVAIL ttSpeed THEN 
           DO:
               CREATE ttSpeed.
               ASSIGN 
                   ttSpeed.Download = liDownSpeed
                   ttSpeed.Upload   = liUpSpeed.
           END.
       END.
   END.

   RETURN TRUE.        
  
END FUNCTION.

FUNCTION fGetSpeedProfile RETURNS CHARACTER
  (piMsSeq AS INTEGER):
    
    DEF VAR lcGescal       AS CHAR       NO-UNDO.
    DEF VAR liCount        AS INTE       NO-UNDO.
    DEF VAR lcSpeedProfile AS CHAR       NO-UNDO.
    DEF VAR lcHost         AS CHAR       NO-UNDO.
    DEF VAR liPort         AS INT        NO-UNDO.
    DEF VAR lcUserId       AS CHAR       NO-UNDO.
    DEF VAR lcpassword     AS CHAR       NO-UNDO.
    DEF VAR lcUriPath      AS CHAR       NO-UNDO.
    DEF VAR lcUriQuery     AS CHAR       NO-UNDO.
    DEF VAR lcUriQueryVal  AS CHAR       NO-UNDO.
    DEF VAR loRequestJson  AS JsonObject NO-UNDO.
    DEF VAR loJson         AS JsonObject NO-UNDO.
    DEF VAR oiStatusCode   AS INT        NO-UNDO. 
    DEF VAR ocStatusReason AS CHAR       NO-UNDO. 

    DEFINE VARIABLE loJsonArray  AS JsonArray         NO-UNDO.
    DEFINE VARIABLE loJsonObject AS JsonObject        NO-UNDO.
    DEFINE VARIABLE lcJsonObject AS JsonObject        NO-UNDO.
    DEFINE VARIABLE lcJsonArray  AS JsonArray         NO-UNDO.

    ASSIGN
        lcHost        = fCParam("GESCON", "Host")   
        liPort        = fIParam("GESCON", "Port")     
        lcUriPath     = fCParam("GESCON", "UriPath")   
        lcUriQuery    = fCParam("GESCON", "UriQuery")  
        lcUriQueryVal = fCParam("GESCON", "UriQueryValue").
        
    FOR EACH  Order WHERE Order.MsSeq = piMsSeq NO-LOCK,
        FIRST OrderCustomer WHERE 
              OrderCustomer.Brand = Syst.Var:gcBrand AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL} NO-LOCK:

        IF OrderCustomer.GesCal EQ "" THEN
            NEXT.

        ASSIGN lcGescal = OrderCustomer.GesCal.
        LEAVE.
    END.

    IF lcGescal <> "" THEN 
    DO:
        ASSIGN lcUriQueryVal = REPLACE(REPLACE(lcUriQueryVal,"#GESCAL",lcGescal)," ","+").

        RUN Gwy/http_rest_client.p(STRING(MethodEnum:GET),
                                   lcHost    ,
                                   liPort    ,     
                                   ""        ,
                                   ""        ,
                                   ""        ,
                                   ""        ,
                                   lcUriPath ,
                                   lcUriQuery,
                                   lcUriQueryVal,
                                   loRequestJson,
                                   OUTPUT oiStatusCode,
                                   OUTPUT ocStatusReason,
                                   OUTPUT loJson).

        ASSIGN lcJsonArray = loJson:GetJsonArray('feasibilities').
            
        DO liCount = 1 TO lcJsonArray:LENGTH:

           ASSIGN lcJsonObject = lcJsonArray:GetJsonObject(liCount).

           IF lcJsonObject:GetCharacter('technology') EQ "FTTH" THEN 
           DO:
               ASSIGN lcSpeedProfile = lcJsonObject:GetCharacter('speedProfile').

               RETURN lcSpeedProfile.
           END.

        END.

    END.    

    RETURN "".

END FUNCTION.

FUNCTION fAddCLITypeStruct RETURNS LOGICAL (INPUT icCLIType      AS CHAR,
                                            INPUT icTariffBundle AS CHAR,
                                            INPUT iiStatusCode   AS INT):

   /* YPR-1720 */
   IF icCLIType EQ "CONT15" AND
      iiStatusCode EQ 2 AND
      ldaCont15PromoEnd NE ? AND
      TODAY <= ldaCont15PromoEnd THEN iiStatusCode = 1.

   sub_struct = add_struct(result_array,"").

   add_string(sub_struct,"cli_type",icCLIType).
   add_string(sub_struct,"tariff_bundle",icTariffBundle).
   add_int(sub_struct,"status_code",iiStatusCode).
   
END FUNCTION.

ASSIGN Syst.Var:katun = "Newton".

IF validate_request(param_toplevel_id, "string,struct") EQ ? THEN RETURN.

pcTenant      = get_string(param_toplevel_id,"0").
pcInputStruct = get_struct(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcInputFields = validate_request(pcInputStruct,"cli_type,bundle_id,MsSeq").

IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcCliType  = get_string(pcInputStruct, "cli_type")  WHEN LOOKUP("cli_type" ,lcInputFields) > 0
   pcBundleId = get_string(pcInputStruct, "bundle_id") WHEN LOOKUP("bundle_id",lcInputFields) > 0
   piMsSeq    = get_int(pcInputStruct, "MsSeq") WHEN LOOKUP("MsSeq",lcInputFields) > 0.  

INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

{newton/src/settenant.i pcTenant}

top_struct = add_struct(response_toplevel_id, "").

result_array = add_array(top_struct, "clitypes").

ASSIGN 
    ldaCont15PromoEnd  = fCParamDa("CONT15PromoEndDate")
    llUseApi           = LOGICAL(fIParam("GESCON", "UseApi")).

IF llUseApi = ? THEN 
    ASSIGN llUseApi = TRUE.
    
IF LOOKUP(lcHostName, "sadachbia") = 0 AND llUseApi THEN 
DO:
    FIND FIRST MobSub NO-LOCK WHERE
               MobSub.Brand = Syst.Var:gcBrand AND
               MobSub.MsSeq = piMsSeq          NO-ERROR. 
    IF AVAILABLE MobSub THEN
    DO:
        FIND FIRST Customer NO-LOCK WHERE 
            Customer.CustNum = MobSub.Custnum NO-ERROR.        
        
         FIND FIRST oldCLIType WHERE oldCLIType.Brand   = Syst.Var:gcBrand  AND
                                     oldCLIType.cliType = pcCliType NO-LOCK NO-ERROR.
         IF AVAIL oldCliType AND oldCliType.FixedLineType EQ {&FIXED_LINE_TYPE_FIBER} THEN
         DO:
             lcSpeedProfileList = fGetSpeedProfile(piMsSeq).

             IF lcSpeedProfileList <> "" THEN 
                 fParseSpeedProfile(lcSpeedProfileList).
         END.
    END.
END.

FOR EACH CLIType NO-LOCK WHERE
         CLIType.Brand = Syst.Var:gcBrand AND
         CLIType.WebStatusCode > 0:
    
   /* Combine bundle based clitypes with base tariff */
   IF CLIType.BundleType = TRUE THEN
   DO:
      FOR EACH bCLIType WHERE
               bCLIType.Brand      = Syst.Var:gcBrand    AND
               bCLIType.BillTarget EQ CLIType.BillTarget AND
               bCLIType.CLIType    <> CLIType.CLIType    AND
               bCLIType.BundleType = CLIType.BundleType  NO-LOCK:

         ASSIGN lcStatusCode = bCLIType.StatusCode.

         fAddCLITypeStruct(CLIType.CLIType,bCLIType.CLIType,lcStatusCode).
      END. /* FOR EACH bCLIType WHERE */
   END.   
   ELSE 
   DO:
      lcStatusCode = CLIType.StatusCode.

      IF lcStatusCode > 0 THEN
      DO:
          FIND FIRST oldCLIType WHERE oldCLIType.Brand   = Syst.Var:gcBrand  AND
                                      oldCLIType.cliType = pcCliType         NO-LOCK NO-ERROR.
          /* 3p to 2p STC is restricted */
          IF fIsConvergenceTariff(pcClitype) AND 
             AVAIL oldCLIType AND 
             oldCLIType.TariffType = {&CLITYPE_TARIFFTYPE_CONVERGENT} AND 
             CliType.TariffType    = {&CLITYPE_TARIFFTYPE_FIXEDONLY}  THEN 
              ASSIGN lcStatusCode = 0.
          /* Internet technology change is restricted */    
          ELSE IF fIsConvergenceTariff(pcClitype) AND AVAIL oldCLIType AND 
             ((oldCLIType.FixedLineType = {&FIXED_LINE_TYPE_FIBER} AND CliType.FixedLineType = {&FIXED_LINE_TYPE_ADSL}) OR 
              (oldCLIType.FixedLineType = {&FIXED_LINE_TYPE_ADSL}  AND CliType.FixedLineType = {&FIXED_LINE_TYPE_FIBER})) THEN 
              ASSIGN lcStatusCode = 0.    
          /* Mobile subscrition should be allowed to do STC between only convergent tariffs, but fixed part should remain same */
          ELSE IF fIsConvergenceTariff(CliType.Clitype) AND fIsConvergenceTariff(pcClitype) EQ FALSE THEN 
              ASSIGN lcStatusCode = 0.    
          ELSE IF CLIType.FixedLineType EQ {&FIXED_LINE_TYPE_FIBER} THEN
          DO: 
              IF LOOKUP(lcHostName, "sadachbia") > 0 THEN 
              DO:
                  IF fIsConvergenceTariff(CliType.Clitype) AND fIsConvergenceTariff(pcClitype) EQ FALSE  THEN
                      ASSIGN lcStatusCode = 0.
              END.  
              ELSE IF MobSub.TerritoryOwner EQ "FIBMM02" OR llUseApi = FALSE THEN
              DO:
                  IF fIsConvergenceTariff(CliType.Clitype) AND (fIsConvergenceTariff(pcClitype) EQ FALSE OR NOT fCheckConvergentSTCCompability(pcClitype,Clitype.clitype)) THEN
                      ASSIGN lcStatusCode = 0.
              END.
              ELSE 
              DO:
                  FIND LAST ttSpeed NO-ERROR.
                  IF AVAIL ttSpeed THEN
                  DO:
                      ASSIGN 
                          liDestDowspeconversion = fSpeedConversion(CliType.FixedLineDownload)
                          liDestUpSpeConversion  = fSpeedConversion(clitype.FixedLineUpload).
     
                      IF NOT (ttSpeed.Download >= liDestDowspeconversion AND ttSpeed.Upload >= liDestUpSpeConversion) THEN
                          ASSIGN lcStatusCode = 0.    
                  END.
              END.
          END.
      END.    
      
      IF fCLITypeIsExtraLine(CLIType.CLIType)
      AND AVAILABLE Customer
      THEN DO:
          
          IF fCheckExistingMainLineAvailForExtraLine(CLIType.CLIType, 
                                                     Customer.CustidType,
                                                     Customer.OrgId, 
                                                     OUTPUT liMLMsSeq) > 0    OR
             fCheckOngoingMainLineAvailForExtraLine(CLIType.CLIType, 
                                                    Customer.CustidType,
                                                    Customer.OrgId) > 0
          THEN lcStatusCode = CLIType.StatusCode.
          ELSE lcStatusCode = 0.
          
          /*STC not possible if the main line is ongoing.
            If mandatory ExtraLine getting STC then we should not show dependent extra lines in the STC View*/
          IF liMLMsSeq > 0 
          THEN DO:
              
              FIND FIRST bfMobSub NO-LOCK WHERE 
                  bfMobSub.MsSeq = liMLMsSeq NO-ERROR.
              
              IF AVAILABLE bfMobSub  
              THEN DO:
                  
                  lcMandatoryELines = fGetMandatoryExtraLineForMainLine(bfMobSub.CLIType).
                  
                  IF LOOKUP(MobSub.CLIType,lcMandatoryELines) > 0
                  AND CAN-FIND(FIRST DPMember NO-LOCK WHERE 
                                     DPMember.HostTable   EQ "MobSub"              AND
                                     DPMember.KeyValue    EQ STRING(MobSub.MsSeq)  AND
                                     DPMember.ValidTo     GE TODAY    )
                  AND CAN-FIND(FIRST TMSRelation NO-LOCK WHERE 
                                     TMSRelation.TableName        EQ {&ELTABLENAME}    AND 
                                     TMSRelation.KeyType          EQ {&ELKEYTYPE}      AND 
                                     TMSRelation.ParentValue      EQ bfMobSub.CLIType  AND 
                                     TMSRelation.RelationType     EQ {&ELMANDATORY}    AND
                             ENTRY(3,TMSRelation.ChildValue,"_")  EQ CLIType.CLIType )
                  THEN ASSIGN lcStatusCode = 0.
                  
              END.
          END.          
      END. 

      fAddCLITypeStruct(CLIType.CLIType,"",lcStatusCode).
   END. /*END ELSE DO*/
END. /* FOR EACH CLIType WHERE */

FINALLY:

END.
