def stream s_ctservel.
output stream s_ctservel to /apps/snet/200901/ctservel.d append.

def stream s_ctservattr.
output stream s_ctservattr to /apps/snet/200901/ctservattr.d append.

FOR EACH ctservpac NO-LOCK where 
  ctservpac.brand = "1" and
  ctservpac.clitype eq "contrd1" and
  ctservpac.servpac = "*1":

  FOR EACH ctservel EXCLUSIVE-LOCK where
   ctservel.brand = "1" and
   ctservel.clitype = ctservpac.clitype and
   ctservel.servpac = ctservpac.servpac and
   ctservel.servcom = "TypeCHG":
      
      FOR EACH ctservattr EXCLUSIVE-LOCK 
         WHERE ctservattr.ctservel = ctservel.ctservel:
         
         export stream s_ctservattr ctservattr.
         delete ctservattr.
      
      END.
      export stream s_ctservel ctservel.
      delete ctservel. 
   END.
END.

output stream s_ctservel close.
output stream s_ctservattr close.
