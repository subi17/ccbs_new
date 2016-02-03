{Syst/testpaa.i}
katun = "snet".

def var llfound as log no-undo.
def var lireq   as int no-undo.

for each mobsub no-lock where
         brand = "1"  and
         clitype = "tarj2":

   disp mobsub.cli mobsub.activationdate.
   
   assign llfound = false
          lireq   = 0.
   
   for first prepaidrequest no-lock where
             prepaidrequest.brand = "1" and
             prepaidrequest.cli   = mobsub.cli and
             prepaidrequest.topupamt = 2000 and
             lookup(prepaidrequest.source,"cc,web order") > 0:
      llfound = true.
   end.
   
   if not llfound then do:
      run topupcamp.p (mobsub.msseq,
                       output lireq).
   end.
    
   disp llfound lireq.
    
end.