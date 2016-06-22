def stream sout.
output stream sout to ydr_2257.txt.

DEF VAR ldeNewGrace AS DEC NO-UNDO init 5242880.

FOR EACH shaperconf EXCLUSIVE-LOCK:

   if shaperconf.limitshaped eq ? or
      shaperconf.limitshaped eq 0 then do:
   disp
      shaperconf.shaperconf
      shaperconf.limitshaped / shaperconf.limitunshaped.
      next.
   end.

   put stream sout unformatted
      shaperconf.shaperconfid  ";"
      shaperconf.limitunshaped ";"
      shaperconf.limitshaped ";"
      round(shaperconf.limitshaped / shaperconf.limitunshaped,2) ";"
      round(ldeNewGrace / shaperconf.limitunshaped,4) skip.

   shaperconf.limitshaped = ldeNewGrace. 

end.

