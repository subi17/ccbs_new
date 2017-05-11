/**
 * Get subscription type.
 *
 * @input  id;array of string;mandatory; subscription type id
 * @output subscription_type;array of struct;subscription type data
 * @subscription_type id;string;subscription type id
                      name;string;subscription type name
                      status;int;status code (eg: 0=inactive,1=active,2=retired)
                      pay_type;int;payment type (eg: 1=postpaid,2=prepaid)
                      usage_type;string;tariff type(eg: Voice/Data)
                      monthly_cost;double;monthly cost
                      bundled;boolean; bundle based clitype (eg: CONTF/CONTRD/CONTS/CONTD)
                      data_amount;double;data amount for subscriptions with base bundle (CONT6,CONT7,CONT8,CONT9)
                      line_type;int;line type (main=1 or additional=2)
                      fixed_line_type;int;fixed line type (ADSL=1 or FIBER=2)
                      dss2_compatible;boolean;DSS2 compatible
                      voip_compatible;boolean;Voip Compatible
                      region;array;struct
 * @region taxzone;string; VAT Code Name
           taxinclvalue;decimal; Monthly Cost based on including Zone Tax  
 */

{newton/src/header_get.i}
{Func/transname.i}
{Syst/tmsconst.i}
{Func/cparam2.i}

DEF VAR lcMobileStruct       AS CHAR NO-UNDO.
DEF VAR lcFixed2FixedStruct  AS CHAR NO-UNDO.
DEF VAR lcFixed2MobileStruct AS CHAR NO-UNDO.
DEF VAR lcFixedLineBB        AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttTariff    NO-UNDO LIKE Tariff.
DEFINE TEMP-TABLE ttPListConf NO-UNDO LIKE PListConf.
DEFINE TEMP-TABLE ttPriceList NO-UNDO LIKE PriceList.

FUNCTION fFillTariff RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttTariff.

   FOR EACH Tariff NO-LOCK WHERE Tariff.Brand = "1":
      CREATE ttTariff.
      BUFFER-COPY Tariff TO ttTariff.
   END.

END FUNCTION.

FUNCTION fFillPListConf RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPListConf.
   
   FOR EACH PListConf NO-LOCK WHERE PListConf.Brand = "1":
      CREATE ttPListConf.
      BUFFER-COPY PListConf TO ttPListConf.
   END.
   
END FUNCTION.

FUNCTION fFillPriceList RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPriceList.
   
   FOR EACH PriceList NO-LOCK WHERE PriceList.Brand = "1":  
      CREATE ttPriceList.
      BUFFER-COPY PriceList TO ttPriceList.
   END.
   
END FUNCTION.

FUNCTION fGetFixedLineBaseBundle RETURNS CHARACTER
  (icCliType AS CHARACTER):

  FIND FIRST RequestAction WHERE RequestAction.Brand      = "1"             AND
                                 RequestAction.ReqType    = 14              AND
                                 RequestAction.CliType    = icCliType       AND
                                 RequestAction.ActionType = "DayCampaign"   AND
                                 RequestAction.Action     = 1               AND
                                 RequestAction.ValidFrom <= TODAY           AND
                                 RequestAction.ValidTo   >= TODAY           NO-LOCK NO-ERROR. 
  IF AVAIL RequestAction THEN 
    RETURN RequestAction.ActionKey.                               

  RETURN "".

END FUNCTION.  

FUNCTION fGetTMSCodeName RETURNS CHAR
  (icTableName AS CHAR,
   icFieldName AS CHAR,
   icCodeGroup AS CHAR,
   icCodeValue AS INT ):

    FIND FIRST TMSCodes WHERE TMSCodes.TableName = "Tariff"            AND
                              TMSCodes.FieldName = "DataType"          AND
                              TMSCodes.CodeGroup = "Tariff"            AND
                              TMSCodes.CodeValue = STRING(icCodeValue) NO-LOCK NO-ERROR.
    IF AVAIL TMSCodes THEN 
        RETURN TMSCodes.CodeName.

    RETURN "".    

END FUNCTION.

FUNCTION fGetTariffCCNAndBDest RETURNS LOGICAL
    (icType         AS CHAR, 
     icCallRegion   AS CHAR,
     icDialType     AS CHAR,
     icMobileBB     AS CHAR,
     icFixedLineBB  AS CHAR,
     OUTPUT oiCCN   AS INTE,
     OUTPUT ocBDest AS CHAR):

     CASE icType:
        WHEN "Mobile" THEN 
        DO:
            CASE icDialType:
                WHEN "GPRS" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 93 ocBDest = "GPRSDATA2_" + icMobileBB.        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 0 ocBDest = "".        
                END.
                WHEN "VOICE" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 81 ocBDest = icMobileBB + "_VOICE_OUT".        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 2 ocBDest = "".    
                END.
                WHEN "SMS" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 51 ocBDest = "".        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 51 ocBDest = "INTERNATIONAL". 
                END.       
                WHEN "MMS" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 94 ocBDest = "".        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 97 ocBDest = "". 
                END.
                OTHERWISE
                DO:
                    ASSIGN
                        oiCCN   = 0
                        ocBDest = "". 
                END.
            END CASE.
        END.
        WHEN "Fixed2Fixed" THEN 
        DO:
            CASE icDialType:
                WHEN "VOICE" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 1081 ocBDest = icFixedLineBB + "_QTY_OUT".        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 1002 ocBDest = "". 
                END.
                OTHERWISE
                DO:
                    ASSIGN
                        oiCCN   = 0
                        ocBDest = "". 
                END.
            END CASE.
        END.
        WHEN "Fixed2Mobile" THEN 
        DO:
            CASE icDialType:
                WHEN "VOICE" THEN 
                DO:
                    IF icCallRegion = "National" THEN
                        ASSIGN oiCCN = 1081 ocBDest = icFixedLineBB + "_MIN_OUT".        
                    ELSE IF icCallRegion = "International" THEN
                        ASSIGN oiCCN = 1002 ocBDest = "". 
                END.
                OTHERWISE
                DO:
                    ASSIGN
                        oiCCN   = 0
                        ocBDest = "". 
                END.
            END CASE.
        END.
     END CASE.

END FUNCTION.

FUNCTION fGetTariffAttributes RETURNS LOGICAL
    (INPUT  icPricePlan AS CHAR,
     INPUT  iiCCN       AS INTE,
     INPUT  icBDest     AS CHAR,
     OUTPUT odPrice     AS DECI,
     OUTPUT ocUnit      AS CHAR,
     OUTPUT odSetup     AS DECI):

    PLISTCONFLOOP:
    FOR EACH ttPListConf WHERE
             ttPListConf.Brand    = "1"         AND
             ttPListConf.RatePlan = icPricePlan AND
             ttPListConf.dFrom   <= TODAY       AND
             ttPListConf.dTo     >= TODAY       NO-LOCK,
        EACH ttPriceList OF ttPListConf WHERE ttPriceList.DedicList = FALSE NO-LOCK
        BY ttPListConf.Prior:
         
        FOR FIRST ttTariff WHERE ttTariff.Brand      = "1"                   AND
                                 ttTariff.CCN        = iiCCN                 AND 
                                 ttTariff.PriceList  = ttPListConf.PriceList AND
                                 ttTariff.BDest      = icBDest               AND
                                 ttTariff.Custnum    = 0                     AND 
                                 ttTariff.ValidFrom <= TODAY                 AND
                                 ttTariff.ValidTo   >= TODAY                 NO-LOCK:
            ASSIGN
                odPrice    = ttTariff.Price[1]
                ocUnit     = fGetTMSCodeName("Tariff","DataType","Tariff",ttTariff.DataType)
                odSetup    = ttTariff.StartCharge[1].

            LEAVE PLISTCONFLOOP.    
        END.
          
        FOR FIRST ttTariff WHERE
                  ttTariff.Brand      = "1"                   AND
                  ttTariff.CCN        = iiCCN                 AND 
                  ttTariff.PriceList  = ttPListConf.PriceList AND
                  ttTariff.BDest      = ""                    AND
                  ttTariff.CustNum    = 0                     AND 
                  ttTariff.ValidFrom <= TODAY                 AND
                  ttTariff.ValidTo   >= TODAY                 NO-LOCK:
           ASSIGN
                odPrice    = ttTariff.Price[1]
                ocUnit     = fGetTMSCodeName("Tariff","DataType","Tariff",ttTariff.DataType)
                odSetup    = ttTariff.StartCharge[1].
        END.

    END. 

    RETURN TRUE.

END FUNCTION.


FUNCTION fGetStruct RETURNS CHAR 
    (icType        AS CHAR,
     icPriceplan   AS CHAR,
     icMobileBB    AS CHAR,   
     icFixedLineBB AS CHAR,   
     INPUT-OUTPUT icStruct AS CHAR):

    DEF VAR lcCallType    AS CHAR NO-UNDO INIT 'national,international'.
    DEF VAR lcDialType    AS CHAR NO-UNDO INIT "gprs,voice,sms,mms".
    DEF VAR liKount       AS INTE NO-UNDO.
    DEF VAR liCount       AS INTE NO-UNDO.

    DEF VAR lcOutStruct   AS CHAR NO-UNDO.

    DEF VAR lcDialTypeStruct  AS CHAR NO-UNDO.
    
    DEF VAR ldPrice   AS DECI NO-UNDO FORMAT "zz9.99".
    DEF VAR lcUnit    AS CHAR NO-UNDO.
    DEF VAR ldSetup   AS DECI NO-UNDO FORMAT "zz9.99".

    DEF VAR liCCN       AS INTE NO-UNDO.
    DEF VAR lcBDest     AS CHAR NO-UNDO.

    DO liCount = 1 TO NUM-ENTRIES(lcCallType):

       lcOutStruct   = add_struct(icStruct, ENTRY(liCount,lcCallType)).

       DO liKount = 1 TO NUM-ENTRIES(lcDialType):

           fGetTariffCCNAndBDest(icType, ENTRY(liCount,lcCallType), CAPS(ENTRY(liKount,lcDialType)), icMobileBB, icFixedLineBB, OUTPUT liCCN, OUTPUT lcBDest).

           IF liCCN > 0 THEN 
           DO:
               lcDialTypeStruct    = add_struct(lcOutStruct, ENTRY(liKount,lcDialType)). 

               fGetTariffAttributes(icPriceplan,
                                    liCCN,
                                    lcBDest,
                                    OUTPUT ldPrice,
                                    OUTPUT lcUnit,
                                    OUTPUT ldSetup).

               add_double(lcDialTypeStruct,"price"    , ldPrice).
               add_string(lcDialTypeStruct,"unit"     , lcUnit).
               add_double(lcDialTypeStruct,"setup_fee", ldSetup).
           END.

       END.
   END.

   RETURN lcOutStruct.

END FUNCTION.    

/* Main Block */
DO ON ERROR UNDO, THROW:
    
    fFillPListConf().

    fFillPriceList().

    fFillTariff().

    DO liCounter = 0 TO get_paramcount(pcIDArray) - 1:
       
       pcID = get_string(pcIDArray, STRING(liCounter)).
       
       FIND FIRST CLIType WHERE CLIType.Brand   = "1" AND CLIType.CLIType = pcID AND CLIType.WebStatusCode > 0 NO-LOCK NO-ERROR.
       IF NOT AVAIL CLIType THEN 
           RETURN appl_err("Subscription Type not found: "+ pcId).
       ELSE IF CliType.PricePlan = "" THEN  
          RETURN appl_err("Rateplan not found: "+ pcId).
       
       ASSIGN lcFixedLineBB = fGetFixedLineBaseBundle(CliType.CliType).

       lcResultStruct = add_struct(resp_array, "").
       add_string(lcResultStruct, "rate_plan", CliType.PricePlan).

       lcMobileStruct   = add_struct(lcResultStruct, "mobile").
       fGetStruct("Mobile", CliType.PricePlan, CliType.BaseBundle, lcFixedLineBB, lcMobileStruct).

       IF lcFixedLineBB > "" THEN 
       DO:
           lcFixed2FixedStruct   = add_struct(lcResultStruct, "fixed2fixed").
           fGetStruct("Fixed2Fixed", CliType.PricePlan, CliType.BaseBundle, lcFixedLineBB, lcFixed2FixedStruct).

           lcFixed2MobileStruct   = add_struct(lcResultStruct, "fixed2mobile").
           fGetStruct("Fixed2Mobile", CliType.PricePlan, CliType.BaseBundle, lcFixedLineBB, INPUT-OUTPUT lcFixed2MobileStruct).
       END.
       
    END.

END.
