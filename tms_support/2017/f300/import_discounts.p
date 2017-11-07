DEF TEMP-TABLE ttDiscountPlan like discountplan.
DEF TEMP-TABLE ttDPTarget like dptarget.
DEF TEMP-TABLE ttDPSubject like dpsubject.

DEF VAR lcInputFolder AS CHAR NO-UNDO. 

lcInputFolder = "./".

input from value(lcInputFolder + "discountplan.d").
repeat:
   create ttDiscountplan.
   import ttDiscountplan.
end.  

input close.
input from value(lcInputFolder + "dptarget.d").
repeat:
   create ttDptarget.
   import ttDptarget.
end.  

input close.
input from value(lcInputFolder + "dpsubject.d").
repeat:
   create ttdpsubject.
   import ttdpsubject.
end.  

create_disc:
DO TRANS:

   FOR EACH ttDiscountplan:
      if ttDiscountplan.dpid eq 0 then next.

      create discountplan.
      buffer-copy ttDiscountplan to discountplan no-error.
      if error-status:error then do:
         MESSAGE "here 1" VIEW-AS ALERT-BOX.
         undo create_disc, leave create_disc.
      end.

      FOR EACH ttDPTarget:
         create dptarget.
         buffer-copy ttDPTarget to dptarget no-error.
         if error-status:error then do:
            MESSAGE "here 2" VIEW-AS ALERT-BOX.
            undo create_disc, leave create_disc.
         end.
      end.

      FOR EACH ttDPSubject:
         create dpsubject.
         buffer-copy ttDPsubject to dpsubject no-error.
         if error-status:error then do:
            MESSAGE "here 3" VIEW-AS ALERT-BOX.
            undo create_disc, leave create_disc.
         end.
      end.
   end.
end.

