DEF TEMP-TABLE ttDiscountPlan like discountplan.
DEF TEMP-TABLE ttDPTarget like dptarget.
DEF TEMP-TABLE ttDPSubject like dpsubject.
DEF TEMP-TABLE ttDPRate like dprate.

input from discountplan.d.
repeat:
   create ttDiscountplan.
   import ttDiscountplan.
end.  

input close.
input from dptarget.d.
repeat:
   create ttDptarget.
   import ttDptarget.
end.  

input close.
input from dpsubject.d.
repeat:
   create ttdpsubject.
   import ttdpsubject.
end.  

input close.
input from dprate.d.
repeat:
   create ttdprate.
   import ttdprate.
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

      FOR EACH ttDPTarget where
               ttDPTarget.dpid = ttDiscountplan.dpid:
         create dptarget.
         buffer-copy ttDPTarget to dptarget no-error.
         if error-statu:error then do:
            MESSAGE "here 2" VIEW-AS ALERT-BOX.
            undo create_disc, leave create_disc.
         end.
      end.

      FOR EACH ttDPSubject where
               ttDPSubject.dpid = ttDiscountplan.dpid:
         create dpsubject.
         buffer-copy ttDPsubject to dpsubject no-error.
         if error-statu:error then do:
            MESSAGE "here 3" VIEW-AS ALERT-BOX.
            undo create_disc, leave create_disc.
         end.
      end.
      
      FOR EACH ttDPRate where
               ttDPRate.dpid = ttDiscountplan.dpid:
         create dprate.
         buffer-copy ttDPRate to dprate no-error.
         if error-statu:error then do:
            MESSAGE "here 4" VIEW-AS ALERT-BOX.
             undo create_disc, leave create_disc.
         end.
      end.
   end.
end.
