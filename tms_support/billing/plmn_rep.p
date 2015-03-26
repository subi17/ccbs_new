DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO.
lcRootDir = "/scratch/reports/plmn".
pause 0.

update
   lcRootDir format "x(52)" label "Output Directory:"
   help "plmn.txt, country.txt, rzitem.txt"
with overlay side-labels 1 column row 4 centered title " PLMN Data Reports "
   frame fdir.
hide frame fdir no-pause.

if lcRootDir = "" THEN return. 

output to value(lcRootDir + "/plmn.txt").
FOR EACH PLMN NO-LOCK:
   put unformatted 
      PLMN.plmn "|"
      PLMN.commname "|"
      PLMN.CoName "|"
      PLMN.Country "|"
      PLMN.CountryPrefix skip. 
END.

output close.
output to value(lcRootDir + "/country.txt").
FOR EACH country NO-LOCK:
   
   put unformatted 
      country.country "|"
      country.coname. 

   FOR EACH RepText WHERE
            RepText.Brand    = "1"    AND
            RepText.TextType = 5 AND
            RepText.LinkCode = Country.Country NO-LOCK:
   
      put unformatted 
       "|" RepText.language "|" RepText.reptext. 

   end.
   
   put unformatted skip.
end.

DEFINE VARIABLE lcBDest AS CHARACTER NO-UNDO.

output close.
output to value(lcRootDir + "/rzitem.txt").
FOR EACH rzitem NO-LOCK:

   lcBDest = "".
 
  FIND FIRST BDest WHERE
           BDest.BDest = RZitem.CountryPrefix NO-LOCK NO-ERROR.
 
   if avail bdest then lcBDest = bdest.bdname.

   FIND FIRST RoamZone WHERE
              RoamZone.RoamZone = RZItem.RoamZone NO-LOCK NO-ERROR.

   put unformatted 
      rzitem.plmncode "|"
      rzitem.countryprefix "|"
      rzitem.roamzone "|"
      roamzone.rzname skip. 
END.

message "done to" lcRootDir.
