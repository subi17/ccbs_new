{testpaa.i}
katun = "ari".

&GLOBAL-DEFINE STAR_EVENT_USER katun

{lib/eventlog.i}

def buffer bctservel for ctservel.
def buffer bservel for servel.

DEFINE VARIABLE lhbctservel AS HANDLE NO-UNDO.
lhbctservel = BUFFER bctservel:HANDLE.
RUN StarEventInitialize(lhbctservel).

DEFINE VARIABLE lhbservel AS HANDLE NO-UNDO.
lhbservel = BUFFER bservel:HANDLE.
RUN StarEventInitialize(lhbservel).


def var linew as int no-undo.
def var liqty as int no-undo.
def var lichg as int no-undo.

FOR EACH CTServEl NO-LOCK WHERE
         CTServEl.Brand   = "1" AND
         CTServEl.ServCom = "RSA":
               
   disp ctservel.clitype 
        ctservel.servpac
        ctservel.fromdate
        ctservel.defvalue.
        
   linew = 0.
   liqty = liqty + 1.
   
   if ctservel.clitype begins "cont" then do:

      /*
      case ctservel.defvalue:
      when 4 then linew = 22.
      when 2 then linew = 20.
      end case.
      
      update linew.
      */
      if ctservel.servpac begins "un" then linew = 20.
      else linew = 22.
        
   end.
   
   else do:

      /*
      case ctservel.defvalue:
      when 3 or when 103 then linew = 23.
      when 1 or when 100 then linew = 21.
      end case.
      */
      if ctservel.servpac begins "un" then linew = 21.
      else linew = 23.
    end.
   
   disp linew format ">9".

   if linew ne ctservel.defvalue and linew ne 0 then do:

      find bctservel where recid(bctservel) = recid(ctservel) exclusive-lock.
      RUN StarEventSetOldBuffer(lhbctservel).
      bctservel.defvalue = linew.
      RUN StarEventMakeModifyEvent(lhbctservel).

      
      lichg = lichg + 1.
   end.       

   disp liqty format ">>9" lichg format ">>9". 
end.   

for each servel no-lock where
         servel.brand = "1" and
         servel.servcom = "rsa":

   disp servel.servpac servel.sevalue.
         
   linew = 0.
   case servel.sevalue:
   when 4 or when 100 then linew = 22.
   when 2 or when 0   then linew = 20.
   end case.
   
   disp linew format ">9".

   if linew ne servel.sevalue and linew ne 0 then do:

      find bservel where recid(bservel) = recid(servel) exclusive-lock.
      RUN StarEventSetOldBuffer(lhbservel).
      bservel.sevalue = linew.
      RUN StarEventMakeModifyEvent(lhbservel).

   end.       


end.

fCleanEventObjects().

