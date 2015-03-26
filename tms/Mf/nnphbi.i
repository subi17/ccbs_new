{country.i}

DEF VAR totrab   AS c NO-UNDO.
DEF VAR volrab   AS c NO-UNDO.
DEF VAR dformat  AS c NO-UNDO.
DEF VAR htext    AS c NO-UNDO.
DEF VAR collbl   AS c NO-UNDO.
DEF VAR dsctext  AS c NO-UNDO.
DEF VAR totext1  AS c NO-UNDO.
DEF VAR totext2  AS c NO-UNDO.
DEF VAR asublbl  AS c NO-UNDO.
DEF VAR intrinvg AS c NO-UNDO.

ASSIGN 
   dformat  = "yyyy-mm-dd"
   totrab   = "Er totala rabatt är:"
   volrab   = "Volymrabatt enligt avtal"
   intrinvg = "T1E".


FUNCTION fHeader RETURNS logical
   (INPUT rep AS INT).

   case rep:

      when 1 THEN DO:
         htext    = "SPECIFICERING AV SAMTALSTYPER / TJÄNST,FÖR PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSLÄNGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan för detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 2 THEN DO:
         htext    = "SPECIFICERING AV ALLA SAMTAL,FÖR PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSLÄNGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FRÅN NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan för detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 3 THEN DO:
         htext    = "SPECIFICERING AV TJÄNS / ANKNYTNING,FÖR PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSLÄNGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FRÅN NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan för detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 4 THEN DO:
         htext    = "SPECIFICERING AV 020 SAMTAL,FÖR PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSLÄNGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FRÅN NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan för detaljer)". 
         totext2  = "ATT BETALA".
      END.

   END.

END.

