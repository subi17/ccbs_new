/*
  MODULE .......: tsformat.i 
  TASK .........: General timestamp formatter
  APPLICATION ..: TMS
  AUTHOR .......: anttis, mvi
  CREATED ......: 11.09.07
  CHANGED ......:
  Version ......: all
----------------------------------------------------------------------- */
/*

Funtion:    fTSFormat(CHARACTER, DECIMAL)
Param 1:    Output format (see possible masks below)
Param 2:    Timestamp
Returns:    Timestamp converted to requested format.

e.g. fTSFormat("dd.mm.yyyy HH:MM", fMakeTS()):

Mask  Description
====  ===========
d     Day of the month as digits. no leading zero for single-digit days.
dd    Day of the month as digits. leading zero for single-digit days.
ddd   Day of the week as a three-letter abbreviation.
dddd  Day of the week as its full name.
m     Month as digits. no leading zero for single-digit months.
mm    Month as digits. leading zero for single-digit months.
mmm   Month as a three-letter abbreviation.
mmmm  Month as its full name.
yy    Year as last two digits. leading zero for years less than 10.
yyyy  Year represented by four digits.
h     Hours. no leading zero for single-digit hours (12-hour clock).
hh    Hours. leading zero for single-digit hours (12-hour clock).
H     Hours. no leading zero for single-digit hours (24-hour clock).
HH    Hours. leading zero for single-digit hours (24-hour clock).
M     Minutes. no leading zero for single-digit minutes.
MM    Minutes. leading zero for single-digit minutes.
s     Seconds. no leading zero for single-digit seconds.
ss    Seconds. leading zero for single-digit seconds.
tt    Lowercase time marker string THEN  am or pm.
TT    Uppercase time marker string THEN  AM or PM.

*/

&IF "{&tsformat}" NE "YES"
&THEN
&GLOBAL-DEFINE tsformat YES

{date.i}

FUNCTION fTSFormatItem RETURNS CHAR
(ldaDate AS DATE,
 liTime AS INT,
 icPattern AS CHAR):

   CASE icPattern:  
      WHEN 'dd' THEN RETURN STRING(DAY(ldaDate),"99").
      WHEN 'yy' THEN RETURN SUBSTRING(STRING(YEAR(ldaDate)),3,2).
      WHEN 'MM' THEN DO:
         IF ASC("M") EQ ASC(SUBSTRING(icPattern,1,1)) THEN
            RETURN STRING(TRUNC(liTime / 60, 0) MOD 60,"99").        
         ELSE
            RETURN STRING(MONTH(ldaDate),"99").
      END.
      WHEN 'ss' THEN RETURN STRING(liTime MOD 60,"99").
      WHEN 'hh' THEN DO:
         IF ASC("h") EQ ASC(SUBSTRING(icPattern,1,1)) THEN 
            IF liTime < 46800 THEN RETURN STRING(TRUNC(liTime / 3600,0),"99").
            ELSE RETURN STRING(TRUNC(liTime / 3600,0) - 12,"99").
         ELSE 
            RETURN STRING(TRUNC(liTime / 3600,0),"99").
      END.
      WHEN 'x' THEN DO:
         DEFINE VARIABLE lcLastDigit AS CHARACTER NO-UNDO. 
         lcLastDigit = SUBSTRING(STRING(DAY(ldaDate),"99"),2,1).
         CASE lcLastDigit:
            WHEN "1" THEN RETURN "st".
            WHEN "2" THEN RETURN "nd".
            WHEN "3" THEN RETURN "rd".
            OTHERWISE RETURN "th".
         END CASE.
      END.
      WHEN 'd' THEN RETURN STRING(DAY(ldaDate)).
      WHEN 'ddd' THEN RETURN ENTRY(WEEKDAY(ldaDate),
         "Sun,Mon,Tue,Wed,Thr,Fri,Sat").
      WHEN 'dddd' THEN RETURN ENTRY(WEEKDAY(ldaDate),
         "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday").
      WHEN 'mmm' THEN RETURN ENTRY(MONTH(ldaDate),
         "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec").
      WHEN 'mmmm' THEN RETURN ENTRY(MONTH(ldaDate),
         "January,February,March,April,May,June,July,
         August,September,October,November,December").
      WHEN 'yyyy' THEN RETURN STRING(YEAR(ldaDate),"9999").
      WHEN 'h' THEN DO:
         IF ASC("h") EQ ASC(SUBSTRING(icPattern,1,1)) THEN 
            IF liTime < 46800 THEN RETURN STRING(TRUNC(liTime / 3600,0)).
            ELSE RETURN STRING(TRUNC(liTime / 3600,0) - 12).
         ELSE 
            RETURN STRING(TRUNC(liTime / 3600,0)).
      END.
      WHEN 'M' THEN DO: 
      IF ASC("M") EQ ASC(SUBSTRING(icPattern,1,1)) THEN
            RETURN STRING(TRUNC(liTime / 60, 0) MOD 60).        
         ELSE
            RETURN STRING(MONTH(ldaDate)).
      END.
      WHEN  's' THEN RETURN STRING(liTime MOD 60).
      
      WHEN 'tt' THEN DO:
         IF ASC("t") EQ ASC(SUBSTRING(icPattern,1,1)) THEN 
            IF liTime < 43200 THEN RETURN 'am'.
            ELSE RETURN 'pm'.
         ELSE
            IF liTime < 43200 THEN RETURN 'AM'.
            ELSE RETURN 'PM'.
      END.
      
      OTHERWISE RETURN icPattern.
   END CASE.

END FUNCTION.


FUNCTION fTSFormat RETURNS CHAR
(icFormat AS CHAR,
 ideTS AS DECIMAL):

   DEFINE VARIABLE lcPatterns AS CHARACTER NO-UNDO CASE-SENSITIVE 
      INIT  "dddd,ddd,dd,d,x,mmmm,mmm,mm,m,yyyy,yy,hh,h,HH,H,MM,M,ss,s,tt,TT".
   
   DEFINE VARIABLE i          AS INTEGER NO-UNDO. 
   DEFINE VARIABLE liStart    AS INTEGER NO-UNDO init 1.
   DEFINE VARIABLE lcPattern  AS CHARACTER NO-UNDO CASE-SENSITIVE. 
   DEFINE VARIABLE lcResult   AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcChar     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSearch   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaDate    AS DATE NO-UNDO.
   DEFINE VARIABLE liTime     AS INTEGER NO-UNDO. 
   
   fSplitTS(ideTS,ldaDate,liTime).
   
   loop_pattern:
   DO WHILE liStart <= LENGTH(icFormat):
      /* skip non-pattern characters */
      lcChar = SUBSTRING(icFormat,liStart,1).
      IF LOOKUP(lcChar,"d,m,y,h,s,t,x") = 0 THEN DO:
         lcResult = lcResult + lcChar.
         liStart = liStart + 1.
         NEXT.
      END.
     
      /* find closest pattern */
      lcSearch = SUBSTRING(lcPatterns,INDEX(lcPatterns,lcChar),-1).
      DO i = 1 TO NUM-ENTRIES(lcSearch):
         lcPattern = ENTRY(i,lcSearch).
         IF INDEX(icFormat,lcPattern,liStart) EQ liStart THEN DO:
            lcResult = lcResult + fTSFormatItem(ldaDate, liTime, lcPattern).
            liStart = liStart + LENGTH(lcPattern).
            NEXT loop_pattern.
         END.   
      END.

      /* pattern not found, append char as is */
      lcResult = lcResult + SUBSTRING(icFormat,liStart,1).
      liStart = liStart + 1.
   
   END.     
   RETURN lcResult.
   
END FUNCTION.

&ENDIF
