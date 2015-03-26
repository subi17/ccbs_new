/* ----------------------------------------------------------------------
  MODULE .......: date.i
  TASK .........: Functions for formatting and converting dates
  APPLICATION ..: tms
  AUTHOR .......: 
  VERSION.......: telef
  CREATED ......: 
  CHANGED ......: 07.03.03/aam fSec2C removed (is in func.i)
                  30.11.05/mvi cleaned code added functions
                           fDayname(date) => "Mon" 
                           fDate2C(date)  => "Mon 14.11.2005"
                           fTs2C(ts)      => "Mon 14.11.2005 12:30:22"
                  30.11.05/mvi added time string checking for form use         
                           fisTimeOk(char) <= "12:00" / "12:00:20" => yes/no  
                  01.12.05/mvi added fDate2TS(date):dec         
  ------------------------------------------------------------------------*/

&IF "{&fdate}" NE "YES"
&THEN

{timestamp.i}
&GLOBAL-DEFINE fdate YES

/** This function converts date to Timestamp 
    eg. 01/01/2005 -> 20050101.00000
*/
FUNCTION fDate2TS RETURNS DEC
  (pdtDate AS DATE):
   RETURN fMake2Dt(pdtDate,0).
END FUNCTION.  


/** 
   this function accepts time in two formats
   HH:MM or HH:MM:SS
   and returns logical depending if the input makes sense. 
*/
FUNCTION fIsTimeOK RETURNS LOGICAL
  (INPUT pcTime AS CHAR):
   DEF VAR liTemp AS INT NO-UNDO.
   /* check possible time string lenghts */
   IF (LENGTH(pcTime) NE 5 AND 
      LENGTH(pcTime) NE 8) THEN RETURN FALSE.
   /* check first : */
   IF SUBSTR(pcTime,3,1) NE ":" THEN RETURN FALSE.

   /* check hours */
   liTemp = INT(SUBSTRING(pcTime,1,2)).
   IF liTemp = ? THEN RETURN FALSE.
   IF liTemp < 0 OR liTemp > 23 THEN RETURN FALSE.  
   MESSAGE "hours:" liTemp.
   /* check minutes */
   liTemp = INT(SUBSTR(pcTime,4,2)).
   IF liTemp = ? THEN RETURN FALSE.
   IF liTemp < 0 OR liTemp > 59 THEN RETURN FALSE.
   /* check possible seconds */
   IF LENGTH(pcTime) = 8 AND SUBSTR(pcTime,6,1) = ":" THEN DO:
       liTemp = INT(SUBSTR(pcTime,7,2)).
     IF liTemp = ? THEN RETURN FALSE.
     IF liTemp < 0 OR liTemp > 59 THEN RETURN FALSE.
   END.

   RETURN TRUE.
END FUNCTION.


FUNCTION fDayname RETURNS CHAR
 (pdt AS DATE):
   DEF VAR lcDaylist AS CHAR FORMAT "X(9)" 
      INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
   RETURN ENTRY(WEEKDAY(pdt),lcDaylist).
END FUNCTION.  

FUNCTION fDate2C RETURNS CHAR
 (pdt AS DATE):
   DEFINE VARIABLE lcReturnValue AS CHAR FORMAT "x(15)".
   lcReturnValue = fDayname(pdt) + " " + STRING(pdt,"99.99.9999").
   RETURN lcReturnValue.
END FUNCTION. 

FUNCTION fTs2C RETURNS CHAR
 (pdeTs AS DEC):
   DEFINE VARIABLE ldtDate AS DATE    NO-UNDO.
   DEFINE VARIABLE liSecs  AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcReturnValue AS CHAR NO-UNDO FORMAT "x(23)".
   fSplitTs(pdeTs,ldtDate,liSecs).
   lcReturnValue = fDayname(ldtDate) + " " + fTs2HMS(pdeTs).
   RETURN lcReturnValue.
END FUNCTION. 

/* Get the last day of the month */
FUNCTION fLastDayOfMonth RETURNS DATE
   (INPUT iDate AS DATE).
   
   IF MONTH(iDate) = 12 THEN RETURN DATE(12, 31, YEAR(iDate)).
   ELSE RETURN DATE(MONTH(iDate) + 1, 1, YEAR(iDate)) - 1.
END.

/* Get period after offsetting specified months from current period */
FUNCTION fOffsetMonthsToPeriod RETURNS INT
   (INPUT iOffsetMonths AS INT):

   DEF VAR liFromPeriod   AS INT NO-UNDO.
   DEF VAR liOffsetYear   AS INT NO-UNDO.
   DEF VAR liOffsetMonths AS INT NO-UNDO.

   IF iOffsetMonths < MONTH(TODAY) THEN
      liFromPeriod = YEAR(TODAY) * 100 + (MONTH(TODAY) - iOffsetMonths).
   ELSE DO:
      IF iOffsetMonths > 12 THEN
         ASSIGN liOffsetYear   = (iOffsetMonths - (iOffsetMonths MOD 12)) / 12
                iOffsetMonths  = (iOffsetMonths MOD 12).
      ELSE
         liOffsetMonths = iOffsetMonths.

      IF ((MONTH(TODAY) + 12) - liOffsetMonths) > 12 THEN
         liFromPeriod = (YEAR(TODAY) - liOffsetYear) * 100 +
                        ((MONTH(TODAY)) - liOffsetMonths).
      ELSE
         liFromPeriod = (YEAR(TODAY) - liOffsetYear - 1) * 100 +
                        ((MONTH(TODAY) + 12) - liOffsetMonths).
   END. /* ELSE DO: */

   RETURN liFromPeriod.
END. /* FUNCTION fOffsetMonthsToPeriod */

FUNCTION fISO8601Date RETURNS CHARACTER
  (INPUT idaDate AS DATE):

   DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeStamp  AS DEC NO-UNDO. 

   lcDateFmt = SESSION:DATE-FORMAT.
   SESSION:DATE-FORMAT = "ymd".

   IF idaDate = ? THEN idaDate = TODAY.

   ldeStamp = fHMS2TS(idaDate,"12:00:00").

   lcTmp = STRING((ldeStamp - TRUNCATE(ldeStamp,0)) * 100000).
   liTime = INT(lcTmp).
   
   lcTmp = STRING(idaDate,"99999999") + "T" +
           STRING(liTime,"HH:MM:SS")     + "+0000".

   SESSION:DATE-FORMAT = lcDateFmt.

   RETURN lcTmp.

END FUNCTION.

&IF "{&fdatefmt}" NE "YES"
&THEN

FUNCTION fDateFmt RETURNS CHAR
   (INPUT d AS Date, INPUT f AS CHAR):

   DEF VAR yy   AS i.
   DEF VAR mm   AS i.
   DEF VAR dd   AS i.
   DEF VAR cy   AS c.
   DEF VAR cm   AS c.
   DEF VAR cd   AS c.
   DEF VAR i    AS i.
   DEF VAR sep  AS c.
   DEF VAR amt  AS i EXTENT 5.
   DEF VAR pos  AS i EXTENT 3.
   DEF VAR spos AS i EXTENT 2.
   DEF VAR ret  AS c.
   DEF VAR b-ok AS lo.

   /* separate INTEGER values */
   ASSIGN
      yy = year(d)
      mm = month(d)
      dd = day(d).

   /* scan YEAR - MONTH - DAY FORMAT */
   DO i = 1 TO length(f):

      amt[5] = amt[5] + 1.

      case substr(f,i,1):

         when "y" THEN DO:
            amt[1] = amt[1] + 1.
            IF pos[1] = 0 THEN pos[1] = i.
         END.

         when "m" THEN DO:
            amt[2] = amt[2] + 1.
            IF pos[2] = 0 THEN pos[2] = i.
         END.

         when "d" THEN DO:
            amt[3] = amt[3] + 1.
            IF pos[3] = 0 THEN pos[3] = i.
         END.

         otherwise DO:
            amt[4] = amt[4] + 1.
            sep = substr(f,i,1).
            IF spos[1] = 0 THEN spos[1] = i.
            ELSE spos[2] = i.
         END.

      END.

   END.

   IF yy > 9 AND amt[1] = 1 THEN 
      ASSIGN amt[1] = 2 amt[5] = amt[5] + 1.
   IF mm > 9 AND amt[2] = 1 THEN 
      ASSIGN amt[2] = 2 amt[5] = amt[5] + 1.
   IF dd > 9 AND amt[3] = 1 THEN 
      ASSIGN amt[3] = 2 amt[5] = amt[5] + 1.

   /* check that incoming FORMAT is OK */
   b-ok = (amt[1] > 4 OR
           amt[2] > 2 OR
           amt[3] > 2 OR
           amt[4] > 2) = FALSE.

   IF b-ok THEN DO:

      /* INTEGER values into characters */
      ASSIGN
         cy = string(yy,"9999")
         cm = string(mm,"99")
         cd = string(dd,"99").

      /* FORMAT lengths */
      cy = substr(cy,length(cy) + 1 - amt[1]).
      IF mm < 10 THEN
         cm = substr(cm,length(cm) + 1 - amt[2]).
      ELSE amt[2] = 2.
      IF dd < 10 THEN
         cd = substr(cd,length(cd) + 1 - amt[3]).
      ELSE amt[3] = 2.

      /* build RETURN STRING in right order */
      DO i = 1 TO amt[5]:
         IF pos[1]  = i THEN ret = ret + cy.
         IF pos[2]  = i THEN ret = ret + cm.
         IF pos[3]  = i THEN ret = ret + cd.
         IF spos[1] = i THEN ret = ret + sep.
         IF spos[2] = i THEN ret = ret + sep.
      END.

   END.
   ELSE ret = ?.

   RETURN ret.             

END.

/* Add century TO a Date using -yy PARAMETER */
FUNCTION fAddCent RETURNS INTEGER
  (INPUT yyy AS INT).

   DEF VAR ret AS i NO-UNDO.

   ret = session:year-offset.

   case ret - yyy <= truncate(ret / 100,0) * 100.

      when FALSE THEN ret = (truncate(ret / 100,0) + 1) * 100.

      when TRUE  THEN ret = truncate(ret / 100,0) * 100.

   END.   

   RETURN ret + yyy.

END.   

&ENDIF

&ENDIF

