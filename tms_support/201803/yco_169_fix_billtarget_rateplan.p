/* RUN AFTER CONFIGURATIONS ARE MIGRATED */

DEF VAR lcRatePlanFrom AS CHAR NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.

DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.
def buffer bbilltarget for billtarget.
llSimulate = true.

def stream sout.

if llSimulate then
   output stream sout to /apps/yoigo/tms_support/201803/yco_169_fix_billtarget_rateplan_simu.txt.
else
   output stream sout to /apps/yoigo/tms_support/201803/yco_169_fix_billtarget_rateplan.txt append.

FOR EACH clitype NO-LOCK where
         clitype.basebundle = "cont34":

   if clitype.priceplan = "CONTRATO34" then
     lcRatePlanFrom = "CONTRATOS".
   else if clitype.priceplan = "CONTRATOCONVC" then
      lcRatePlanFrom = "CONTRATOCONVS".
   else do:
      MESSAGE "configuration migration not yet done!" VIEW-AS ALERT-BOX.
      QUIT.
   end.

   FOR EACH billtarget no-LOCK  where
            billtarget.rateplan = lcRatePlanFrom and
            billtarget.billtarget = clitype.billtarget:

       i = i + 1.
       if i mod 1000 eq 0 THEN DO:
          disp clitype.clitype i j with frame a.
          pause 0.
       END.

      if billtarget.rateplan = clitype.priceplan then next.

      put stream sout unformatted
         "BILLTARGET_CHANGED" ";"
         billtarget.custnum ";"
         billtarget.billtarget ";"
         billtarget.rateplan ">" clitype.priceplan skip.

      j = j + 1.

      if llSimulate then next.
      else do trans:
         find bbilltarget EXCLUSIVE-LOCK where
              rowid(bbilltarget) = rowid(billtarget).
         bbilltarget.rateplan = clitype.priceplan.
         release bbilltarget.
      end.
   end.
end.


