/*One time script that modifies DPCategory field in DiscountPlan.*/

DEF STREAM sLog.
DEF STREAM sIn.
DEF VAR lcInputFile AS CHAR NO-UNDO.
DEF VAR lcBillCode AS CHAR NO-UNDO.
DEF VAR lcNewCategory AS CHAR NO-UNDO.
DEF VAR lcLog AS CHAR NO-UNDO.
DEF VAR llgSimulate AS LOGICAL NO-UNDO INIT TRUE.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcUpdateStatus AS CHAR NO-UNDO.

FUNCTION fUpdateCategory RETURNS CHAR
   (icBillcode AS CHAR,
    icCategory AS CHAR):
   FIND FIRST DiscountPlan EXCLUSIVE-LOCK  where
              DiscountPlan.Billcode EQ icBillcode NO-ERROR.
   IF NOT AVAIL DiscountPlan THEN RETURN "Discount Plan not found".

   IF llgSimulate EQ FALSE THEN DO:
      DiscountPlan.DPCategory = icCategory.
      RETURN "OK".
   END.
   ELSE DO:
      RETURN "SIMULATED OK".
   END.
   RETURN "Unknown status".

END.


lcInputFile = "discountplan_category_list.txt".

lcLog = "category_update_"  +
               STRING(YEAR(TODAY)) +
               STRING(MONTH(TODAY),"99") +
               STRING(DAY(TODAY),"99") + ".log".
message lcInputFile VIEW-AS ALERT-BOX.
INPUT STREAM sIn FROM "/apps/yoigo/tms_support/2017/RES-608_discounts/RES-608_discounts/discountplan_category_list.txt".
OUTPUT STREAM sLog TO VALUE("/apps/yoigo/tms_support/2017/RES-608_discounts/" + lcLog).

REPEAT:
   IMPORT STREAM sIn UNFORMATTED lcLine.
   message lcLine VIEW-AS ALERT-BOX.
   lcBillCode = ENTRY(1,lcLine,";").
   lcNewCategory = ENTRY(3,lcLine,";").
   lcUpdateStatus = "".
   lcUpdateStatus = fUpdateCategory(lcBillCode, lcNewCategory).
   PUT STREAM sLog UNFORMATTED lcLine + ";" + lcUpdateStatus SKIP.

END.



