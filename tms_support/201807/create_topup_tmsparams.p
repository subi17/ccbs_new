
/*------------------------------------------------------------------------
    File        : create_topup_tmsparams.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jul 25 14:17:52 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


MESSAGE "Are you sure you want to create the TMS Params for the Initial and Campaign Topup Prefix?"
VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE liChoice AS LOGICAL .

IF NOT liChoice THEN RETURN.

FUNCTION fCreateTMSParam RETURN CHARACTER (INPUT icParamGroup AS CHARACTER , INPUT icParamCode AS CHARACTER ,
                                           INPUT icParamName  AS CHARACTER , INPUT icCharVal   AS CHARACTER) :
                             
                             
    DEFINE BUFFER bfTMSParam FOR TMSParam.
    
    CREATE bfTMSParam.
    ASSIGN bfTMSParam.Brand         =   Syst.Var:gcBrand
           bfTMSParam.ParamGroup    =   icParamGroup
           bfTMSParam.ParamCode     =   icParamCode
           bfTMSParam.ParamName     =   icParamName
           bfTMSParam.CharVal       =   icCharVal
           bfTMSParam.ParamType     =   "C"           
           .                      
    
    RETURN "".
    
END FUNCTION.

fCreateTMSParam(  INPUT "TOPUP" , INPUT "InitialTopupPrefix",            INPUT "Prefix   to Initial  TopUp",          INPUT "997"). 
fCreateTMSParam(  INPUT "TOPUP" , INPUT "InitialTopupBillCode",          INPUT "BillCode to Initial  TopUp",          INPUT "TS0000050").
fCreateTMSParam(  INPUT "TOPUP" , INPUT "InitialTopupDiscountBillCode",  INPUT "BillCode to Initial  TopUp Discount", INPUT "TS0DISC50").
fCreateTMSParam(  INPUT "TOPUP" , INPUT "CampaignTopupPrefix",           INPUT "Prefix   to Campaign TopUp",          INPUT "994"). 
fCreateTMSParam(  INPUT "TOPUP" , INPUT "CampaignTopupBillCode",         INPUT "BillCode to Campaign TopUp",          INPUT "CPDISC10").  
fCreateTMSParam(  INPUT "TOPUP" , INPUT "CampaignTopupDiscountBillCode", INPUT "BillCode to Campaign TopUp Discount", INPUT "CPDISC11").

MESSAGE "TMSParam created successfully."
VIEW-AS ALERT-BOX.

















