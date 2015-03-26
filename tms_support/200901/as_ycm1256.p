output to /apps/snet/200901/plmn_2.txt.
FOR EACH PLMN NO-LOCK:
   put unformatted 
      PLMN.plmn "|"
      PLMN.commname "|"
      PLMN.CoName "|"
      PLMN.Country skip. 
END.

output close.

DEFINE VARIABLE lcBDest AS CHARACTER NO-UNDO.

output to /apps/snet/200901/rzitems_2.txt.
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
      lcbdest "|"
      rzitem.roamzone "|"
      roamzone.rzname "|"
      rzitem.dialtype skip. 
END.
