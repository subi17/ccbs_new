
DEF VAR a       AS CHAR NO-UNDO.
DEF VAR odtDate AS DATE NO-UNDO.

FORM
SKIP
"  1) Search call with B-number                   "       SKIP
"  2) Search Yoigo campaign, below campaign limit "       SKIP
"  3) Search Yoigo campaign, campaign full        "       SKIP
"  4) Search GPRS campaign, below campaign limit  "       SKIP
"  5) Search GPRS campaign, campaign full         "       SKIP(2)
"  Selection: " a no-label   

WITH FRAME aa.

view frame aa.
 odtdate = today.

update a with frame aa.

IF a > "1" AND a < "6" THEN 
update odtdate column-label "Start day..." 
       FORMAT "99-99-9999"  WITH FRAME ab.


hide frame aa.
hide frame ab.

CASE a.

   WHEN "1" THEN DO:
      RUN tms_support/billing/find_bnumber.p.
   END.
   WHEN "2" THEN DO:
      RUN tms_support/billing/compare_dccounter.p("","YOIGOYOIGO",odtDate,FALSE).
   END.
   WHEN "3" THEN DO:
      RUN tms_support/billing/compare_dccounter.p("","YOIGOYOIGO",odtDate,TRUE).
   END.
   WHEN "4" THEN DO:
      RUN tms_support/billing/compare_dccounter.p("","GPRS",odtDate,FALSE).
   END.
   
   WHEN "5" THEN DO:
      RUN tms_support/billing/compare_dccounter.p("","GPRS",odtDate,TRUE).
   END.
END.

