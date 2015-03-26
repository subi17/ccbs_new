def var i as i no-undo.
def var lcspooldir as ch no-undo.

def var libilled as int no-undo.
def var lirated  as int no-undo.
def var lierror  as int no-undo.

def stream sbilled.
def stream srated.
def stream serror.

lcspooldir = "/store/riftp/dumpfiles/dbdumb/spool/".
   
output stream sbilled to value(lcspooldir + 
                               "postpaid_august_laskutetut_2007.dump").
output stream srated  to value(lcspooldir + 
                               "postpaid_august_hinnoitellut_2007.dump").
output stream serror  to value(lcspooldir +                                 
                               "postpaid_august_error_2007.dump").

for each mobcdr no-lock where
         mobcdr.datest >= 8/1/7 and
         mobcdr.datest <= 8/31/7:
         
   i = i + 1.
   
   if mobcdr.errorcode > 0 then do:
      put stream serror unformatted
         {1} skip.
      lierror = lierror + 1.
   end.

   else do:
      find invseq where invseq.invseq = mobcdr.invseq no-lock no-error.
      if available invseq and invseq.billed then do:
         put stream sbilled unformatted
            {1} skip.
         libilled = libilled + 1.
      end.
      
      else do:
         put stream srated unformatted
            {1} skip.
         lirated = lirated + 1.
      end.
   end.
   
   if i mod 1000 = 0 then do:
      pause 0.
      disp i mobcdr.datest libilled lierror lirated with 1 down.
   end.
end.


output stream sbilled close.
output stream srated  close.
output stream serror  close.

