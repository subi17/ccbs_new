def stream sout.
output stream sout to ydr_2257_2.txt.

FOR EACH dcservicecomponent EXCLUSIVE-LOCK where
     dcservicecomponent.servcom = "shaper" and
     dcservicecomponent.todate >= today and
     index(dcservicecomponent.DefParam,"#gracelimit") > 0:

   find dcservicepackage NO-LOCK where
        dcservicepackage.dcservicepackageid = dcservicecomponent.dcservicepackageid no-error.

   IF AVAIL dcservicepackage then
   put stream sout unformatted
      dcservicepackage.dcevent skip.

   put stream sout unformatted
      dcservicecomponent.DefParam skip
      replace(dcservicecomponent.DefParam,"#GRACELIMIT",",GRACE=0") skip.
   export stream sout
      dcservicecomponent.
   put stream sout unformatted skip(1).

   dcservicecomponent.defparam = replace(dcservicecomponent.DefParam,"#GRACELIMIT",",GRACE=0").
end.

