DEF VAR i AS INT NO-UNDO. 
DISABLE TRIGGERS FOR LOAD OF DPMember.

FOR EACH dpmember EXCLUSIVE-LOCK:

   i = i + 1.
   if dpmember.dpid ne ? and
      dpmember.dpid > 0 then next.

   dpmember.dpmemberid = next-value(dpmemberid).

   if i mod 100 eq 0 then do:
      disp i with frame a.
      pause 0.
   end.

end.  
