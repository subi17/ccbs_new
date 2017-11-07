
DEF VAR lcSourceFolder AS CHAR NO-UNDO. 

lcSourceFolder = "../tms_support/2017/convergent_cross_sell/".
input from value(lcSourceFolder + "daycampaign.d").

repeat:
   create daycampaign.
   import daycampaign.
end.

input close.
input from value(lcSourceFolder + "feemodel.d").

repeat:
   create feemodel.
   import feemodel.
end.

input close.
input from value(lcSourceFolder + "fmitem.d").

repeat:
   create fmitem.
   import fmitem.
end.
