
&GLOBAL-DEFINE NOW_TS YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY) + (TIME / 100000)
&GLOBAL-DEFINE TODAY_CHAR STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99")  + STRING(DAY(TODAY),"99")
&GLOBAL-DEFINE TAB CHR(9)
&GLOBAL-DEFINE UNL CHR(10)