{Syst/testpaa.i}
katun = "ari".

{Syst/eventval.i}
{Mc/lib/tokenlib.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRoamTariff AS HANDLE NO-UNDO.
   lhRoamTariff = BUFFER RoamTariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff ).
end.
 
def var lcpricelist as char no-undo.
def var i as int no-undo.

lcpricelist = "dnkia,dnkmx,fintf,nornc". 

do i = 1 to num-entries(lcpricelist):

   for each roamtariff exclusive-lock where
            roamtariff.pricelist = entry(i,lcpricelist) and
            roamtariff.validfrom = 6/1/9 and
            roamtariff.validto   = 12/31/9 and
            roamtariff.ratetype = 1:
            
       disp pricelist validfrom validto service.

       IF llDoEvent THEN RUN StarEventSetOldBuffer (lhRoamTariff).
       roamtariff.validto = 10/31/9.
       IF llDoEvent THEN RUN StarEventMakeModifyEvent (lhRoamTariff).
  end.

end.

fcleaneventobjects().


         
