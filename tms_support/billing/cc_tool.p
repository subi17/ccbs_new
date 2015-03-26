
DEF VAR a       AS CHAR NO-UNDO.
DEF VAR odtDate AS DATE NO-UNDO.

FORM
SKIP
"  1) Etsi puhelua b-numerolla?   "                  SKIP
"  2) Etsi Yoigo kampanja, alle kampanjarajan"       SKIP
"  3) Etsi Yoigo kampanja, kampanja täynnä   "       SKIP
"  4) Etsi GPRS kampanja,  alle kampanjarajan"       SKIP
"  5) Etsi GPRS kampanja, kampanja täynnä"           SKIP(2)
" Valintasi?" a no-label   

WITH FRAME aa.

view frame aa.
 odtdate = today.

update a with frame aa.

IF a > "1" AND a < "6" THEN 
update odtdate column-label "Alkaen päivästä..." 
       FORMAT "99-99-9999"  WITH FRAME ab.


hide frame aa.
hide frame ab.

CASE a.

   WHEN "1" THEN DO:
      run find_bnumber.p.
   END.
   WHEN "2" THEN DO:
      run compare_dccounter.p("","YOIGOYOIGO",odtDate,FALSE).
   END.
   WHEN "3" THEN DO:
      run compare_dccounter.p("","YOIGOYOIGO",odtDate,TRUE).
   END.
   WHEN "4" THEN DO:
      run compare_dccounter.p("","GPRS",odtDate,FALSE).
   END.
   
   WHEN "5" THEN DO:
      run compare_dccounter.p("","GPRS",odtDate,TRUE).
   END.
END.

