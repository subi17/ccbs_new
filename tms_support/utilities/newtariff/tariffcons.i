/*------------------------------------------------------------------------
  MODULE .......: tariffcons.i
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi
  CREATED ......: Fri Feb 06 13:33:36 EET 2015
  CHANGED ......:
  Version ......: Yoigo
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE LINETYPE   "Entry,Main,Additional"
&GLOBAL-DEFINE FLINETYPE  "ADSL,FIBER" 
&GLOBAL-DEFINE WEBSTATUS  "Inactive,Active,Retired,Hidden"
&GLOBAL-DEFINE STCSTATUS  "Inactive,Active,Retired"
&GLOBAL-DEFINE PAYTYPE    "Postpaid,Prepaid"
&GLOBAL-DEFINE USAGETYPE  "Voice,Data"
&GLOBAL-DEFINE FEECALC    "Full,Relative,UsageBased"
&GLOBAL-DEFINE LIMITVALUE "Full,Relative"
&GLOBAL-DEFINE CONTRACT   "ServicePackage,PackageWithCounter,PackagewithoutCounter"
&GLOBAL-DEFINE BBPROFILE  "1,2"
&GLOBAL-DEFINE LOGVALUE   "Yes,No"
&GLOBAL-DEFINE TAXCLASS   "0,1,2"   /* 0: Duty free / 1: Usage / 2: Terminals */

&GLOBAL-DEFINE CT      "CLIType"  
&GLOBAL-DEFINE TB      "TariffBundle"
&GLOBAL-DEFINE BB      "BaseBundle"
&GLOBAL-DEFINE LT      "LineType"
&GLOBAL-DEFINE FLT     "FixedLineType"
&GLOBAL-DEFINE CF      "CommercialFee"
&GLOBAL-DEFINE CMF     "ComparisonFee"
&GLOBAL-DEFINE SC      "ServiceClass"
&GLOBAL-DEFINE WS      "WebStatus"
&GLOBAL-DEFINE STCS    "STCStatus"
&GLOBAL-DEFINE PT      "PaymentType"
&GLOBAL-DEFINE UT      "UsageType"
&GLOBAL-DEFINE FMFC    "FirstMonthFeeCalc" 
&GLOBAL-DEFINE LMFC    "LastMonthFeeCalc"
&GLOBAL-DEFINE TOC     "TypeOfContract"
&GLOBAL-DEFINE DL      "DataLimit"
&GLOBAL-DEFINE VL      "VoiceLimit" 
&GLOBAL-DEFINE BDL     "BDestinationLimit" 
&GLOBAL-DEFINE FMDL    "FirstMonthDataLimit" 
&GLOBAL-DEFINE LMDL    "LastMonthDataLimit"
&GLOBAL-DEFINE FMVL    "FirstMonthVoiceLimit"
&GLOBAL-DEFINE LMVL    "LastMonthVoiceLimit" 
&GLOBAL-DEFINE FMBDL   "FirstMonthBDestLimit" 
&GLOBAL-DEFINE LMBDL   "LastMonthBDestLimit"
&GLOBAL-DEFINE BBP     "BBProfile"
&GLOBAL-DEFINE DSS2C   "DSS2Compatible"
&GLOBAL-DEFINE DSS2PL  "DSS2PrimaryLine"
&GLOBAL-DEFINE NVC     "NativeVoIPCompatible"
&GLOBAL-DEFINE OV      "OnlyVOICE"
&GLOBAL-DEFINE MFBC    "MonthlyFeeBillCode"
&GLOBAL-DEFINE VBDES   "VoiceBDest"
&GLOBAL-DEFINE CONTU   "CONTRATO"
&GLOBAL-DEFINE CONTL   "Contrato"
&GLOBAL-DEFINE TARJ    "Tarjeta"
&GLOBAL-DEFINE MOF     "MonthlyFee"
&GLOBAL-DEFINE TN      "TariffName"
&GLOBAL-DEFINE BS      "BonoSupport"
/*convergence project*/
&GLOBAL-DEFINE BUPS     "BundleUpsell"


