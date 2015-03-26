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
   totrab   = "Er totala rabatt �r:"
   volrab   = "Volymrabatt enligt avtal"
   intrinvg = "T1E".


FUNCTION fHeader RETURNS logical
   (INPUT rep AS INT).

   case rep:

      when 1 THEN DO:
         htext    = "SPECIFICERING AV SAMTALSTYPER / TJ�NST,F�R PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSL�NGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan f�r detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 2 THEN DO:
         htext    = "SPECIFICERING AV ALLA SAMTAL,F�R PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSL�NGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FR�N NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan f�r detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 3 THEN DO:
         htext    = "SPECIFICERING AV TJ�NS / ANKNYTNING,F�R PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSL�NGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FR�N NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan f�r detaljer)". 
         totext2  = "ATT BETALA".
      END.

      when 4 THEN DO:
         htext    = "SPECIFICERING AV 020 SAMTAL,F�R PERIODEN," + 
                    "TOTALT/HUVUDNUMMER".
         collbl   = "SAMTALSTYP,TOTAL SAMTALSL�NGD,ANTAL SAMTAL,NETTOBELOPP," +
                    "SUMMA".
         asublbl  = "SAMTAL FR�N NR  ".
         totext1  = "TOTALT".
         dsctext  = "RABATT (Se baksidan av fakturan f�r detaljer)". 
         totext2  = "ATT BETALA".
      END.

   END.

END.

