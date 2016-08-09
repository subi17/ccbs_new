/* ----------------------------------------------------------------------
  MODULE .......: clitypedump.p
  TASK .........: Daily full dump of clitype with specific fields  
  APPLICATION ..: tms
  AUTHOR .......: Chanchal sharma
  CREATED ......: 09/08/2016
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

DEF VAR lcOutFile AS CHAR NO-UNDO.

ASSIGN lcOutFile = "/mnt/qss/subscription_types/ypi31_clitype_" + 
                   STRING(TODAY,"99999999").

OUTPUT TO VALUE(lcOutFile).
   FOR EACH clitype NO-LOCK:
      EXPORT DELIMITER "," 
                       clitype.clitype clitype.paytype 
                       clitype.BundleType clitype.BaseBundle 
                       clitype.MinimAmt clitype.CommercialFee
                       clitype.CompareFee clitype.UsageType.
   END.
OUTPUT CLOSE.


