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
&GLOBAL-DEFINE TN      "TariffName"
&GLOBAL-DEFINE TB      "TariffBundle"
&GLOBAL-DEFINE WS      "WebStatus"
&GLOBAL-DEFINE STCS    "STCStatus"
&GLOBAL-DEFINE PT      "PaymentType"
&GLOBAL-DEFINE UT      "UsageType"
&GLOBAL-DEFINE LT      "LineType"
&GLOBAL-DEFINE FLT     "FixedLineType"
&GLOBAL-DEFINE FLD     "FixedLineDownload"
&GLOBAL-DEFINE FLU     "FixedLineUpload"
&GLOBAL-DEFINE SC      "ServiceClass"
&GLOBAL-DEFINE CF      "CommercialFee"
&GLOBAL-DEFINE CMF     "ComparisonFee"
&GLOBAL-DEFINE AB      "OptionalBundlesAndBONOs"
&GLOBAL-DEFINE STC_BT  "BundlesForActivateOnSTC"
&GLOBAL-DEFINE STC_SR  "ServicesForReCreateOnSTC"
&GLOBAL-DEFINE CSF     "CopyServicesFromCliType"


&GLOBAL-DEFINE M_BB     "MobileBaseBundle"
&GLOBAL-DEFINE M_BBT    "MobileBaseBundleType"
&GLOBAL-DEFINE M_UPSL   "BundleUpsell"
&GLOBAL-DEFINE M_BONO   "BonoSupport"
&GLOBAL-DEFINE M_MFBC   "MonthlyFeeBillCode"
&GLOBAL-DEFINE M_CF     "CommercialFeeForMobile"
&GLOBAL-DEFINE M_FMFC   "FirstMonthFeeCalc" 
&GLOBAL-DEFINE M_LMFC   "LastMonthFeeCalc"
&GLOBAL-DEFINE M_DL     "DataLimit"
&GLOBAL-DEFINE M_VL     "VoiceLimit" 
&GLOBAL-DEFINE M_BDL    "BDestinationLimit" 
&GLOBAL-DEFINE M_FMDL   "FirstMonthDataLimit" 
&GLOBAL-DEFINE M_LMDL   "LastMonthDataLimit"
&GLOBAL-DEFINE M_FMVL   "FirstMonthVoiceLimit"
&GLOBAL-DEFINE M_LMVL   "LastMonthVoiceLimit" 
&GLOBAL-DEFINE M_FMBDL  "FirstMonthBDestLimit" 
&GLOBAL-DEFINE M_LMBDL  "LastMonthBDestLimit"


&GLOBAL-DEFINE FL_BB    "FixedLineBaseBundle"
&GLOBAL-DEFINE FL_BBT   "FixedLineBaseBundleType"
&GLOBAL-DEFINE FL_UPSL  "UpsellForFixedLine"
&GLOBAL-DEFINE FL_BONO  "BonoSupportForFixedLine"
&GLOBAL-DEFINE FL_MFBC  "MonthlyFeeBillCodeForFixedLine"
&GLOBAL-DEFINE FL_CF    "CommercialFeeForFixedLine"
&GLOBAL-DEFINE FL_FMFC  "FirstMonthFeeCalcForFixedLine" 
&GLOBAL-DEFINE FL_LMFC  "LastMonthFeeCalcForFixedLine"
&GLOBAL-DEFINE FL_VL    "VoiceLimitForFixedLine" 
&GLOBAL-DEFINE FL_BDL   "BDestinationLimitForFixedLine" 
&GLOBAL-DEFINE FL_FMVL  "FirstMonthVoiceLimitForFixedLine"
&GLOBAL-DEFINE FL_LMVL  "LastMonthVoiceLimitForFixedLine" 
&GLOBAL-DEFINE FL_FMBDL "FirstMonthBDestLimitForFixedLine" 
&GLOBAL-DEFINE FL_LMBDL "LastMonthBDestLimitForFixedLine"

&GLOBAL-DEFINE BBP     "BBProfile"
&GLOBAL-DEFINE DSS2C   "DSS2Compatible"
&GLOBAL-DEFINE DSS2PL  "DSS2PrimaryLine"
&GLOBAL-DEFINE NVC     "NativeVoIPCompatible"
&GLOBAL-DEFINE OV      "OnlyVOICE"
&GLOBAL-DEFINE VBDES   "VoiceBDest"
&GLOBAL-DEFINE CONTU   "CONTRATO"
&GLOBAL-DEFINE CONTL   "Contrato"
&GLOBAL-DEFINE TARJ    "Tarjeta"
&GLOBAL-DEFINE MOF     "MonthlyFee"

/*convergence project*/



