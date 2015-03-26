{commpaa.i}
katun = "anttis".
gcBrand = "1".
DEFINE VARIABLE ldaBegin AS DATE NO-UNDO.
DEFINE VARIABLE ldaEnd AS DATE NO-UNDO.

ldaBegin  = 5/6/2010.
ldaEnd  = 5/7/2010.

{roamtariff.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 


DEFINE VARIABLE lcOpers AS CHARACTER NO-UNDO. 
lcOpers = "DEUD1".

PROCEDURE pFoo:

   
   release roamgprs.
   ROAMCDR:
   FOR EACH RoamCDR WHERE
      roamcdr.DateStart >= ldaBegin AND
      roamcdr.DateStart <= ldaEnd and
      roamcdr.plmn = lcOpers EXCLUSIVE-LOCK:

      DO TRANSACTION:
         fRoamTariff(FALSE, BUFFER roamcdr, BUFFER roamgprs).  
         i = i + 1.
      END.

   END.
   release roamcdr.

   ROAMGPRS:
   FOR EACH RoamGPRS WHERE
      RoamGPRS.DateStart >= ldaBegin AND
      RoamGPRS.DateStart <= ldaEnd and
      RoamGPRS.PLMN = lcOpers EXCLUSIVE-LOCK:

      DO TRANSACTION:
         fRoamTariff(FALSE, BUFFER roamcdr, BUFFER roamgprs).  
         j = j + 1.
      END.

   END.
END PROCEDURE.

etime(true).
run pfoo.
MESSAGE etime i j VIEW-AS ALERT-BOX.

