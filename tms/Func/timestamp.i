/* KooAa 28.06.1999 
   Functions for creating & splitting timestamps 
   
   changes:         02.09.04/aam use no-error in fHetu2Date
                    18.02.05/aam TimeStampDefined
                    08.04.05 kl fUTCTime
                    06.06.05 tk fUTC2TS
                    15.06.05 kl fDayLight
                    16.06.05 tk previous to fUTC2TS
                    30.09.05 tk fDayLight fixed
                    09.11.06 kl fISO860
                    07.12.06 kl fOffSetTS
                    24.01.07 kl fOffSet - timestamp as parameter
                    14.08.07/aam fSecOffSet, other offset functions to use it
*/

&IF "{&TimeStampDefined}" NE "YES" 
&THEN

&GLOBAL-DEFINE TimeStampDefined YES


&IF "{&fmakets}" NE "YES"
&THEN

&GLOBAL-DEFINE fmakets YES


function fMakeTS returns dec.

   def var yy  as i no-undo.
   def var mm  as i no-undo.
   def var dd  as i no-undo.
   def var ret as de no-undo format "99999999.99999".

   assign
      yy = year(today)
      mm = month(today)
      dd = day(today).

   ret = yy * 10000 + mm * 100 + dd.
   ret = ret + (time / 100000).

   return ret.

end.



function fSplitTS returns log
  (input ts as dec, output dte as date, output tme as int).

   def var yy  as i  no-undo.
   def var mm  as i  no-undo.
   def var dd  as i  no-undo.
   def var c   as c  no-undo.
   def var ret as lo no-undo.

   assign
      c   = substr(string(ts,"99999999.99999"),1,8)
      yy  = integer(substr(c,1,4))
      mm  = integer(substr(c,5,2))
      dd  = integer(substr(c,7,2))
      dte = date(mm,dd,yy)
      c   = substr(string(ts,"99999999.99999"),10,5)
      tme = integer(c)
   no-error.

   if error-status:error then ret = false.
   else ret = true.

   return ret.

end.

function fTS2Date returns log
  (input ts as dec, output dte as date).

   def var yy  as i  no-undo.
   def var mm  as i  no-undo.
   def var dd  as i  no-undo.
   def var c   as c  no-undo.

   assign
      c   = substr(string(ts,"99999999.99999"),1,8)
      yy  = integer(substr(c,1,4))
      mm  = integer(substr(c,5,2))
      dd  = integer(substr(c,7,2))
      dte = date(mm,dd,yy)
   no-error.

   if error-status:error then return false.
   else return true.

end.

FUNCTION fHMS2TS RETURNS DECIMAL
   (INPUT pDate AS DATE, INPUT pTime AS CHARACTER).

   DEF VAR lRet AS DECIMAL.
   DEF VAR lSec AS INTEGER.

   ASSIGN
      lSec = INT(ENTRY(1,pTime,":")) * 3600 + 
             INT(ENTRY(2,pTime,":")) * 60   +
             INT(ENTRY(3,pTime,":")) WHEN SUBSTR(pTime,3,1) = ":"
      lSec = INT(SUBSTR(pTime,1,2))  * 3600 +
             INT(SUBSTR(pTime,3,2))  * 60   +
             INT(SUBSTR(pTime,5,2))  WHEN SUBSTR(pTime,3,1) NE ":"
      lRet = YEAR(pDate) * 10000 + MONTH(pDate) * 100 + DAY(pDate) +
             lSec / 100000.

   RETURN lRet.
END FUNCTION.

FUNCTION fTS2HMS RETURNS CHARACTER
   (INPUT tstamp AS DECIMAL).

   DEF VAR outstring  AS C  NO-UNDO.
   DEF VAR dte        AS DA NO-UNDO.
   DEF VAR tme        AS I  NO-UNDO.

   IF fSplitTS(tstamp, OUTPUT dte, OUTPUT tme) THEN
      outstring = STRING(dte,"99.99.9999") + " " + STRING(tme,"hh:mm:ss").
   ELSE
      outstring = "00.00.0000 00:00:00".

   RETURN outstring.

END FUNCTION.

&ENDIF

FUNCTION fCheckTime RETURNS LOGICAL
  (INPUT pTime AS CHARACTER):

   DEF VAR pRet AS LOGICAL NO-UNDO.

   IF (INT(SUBSTR(pTime,1,2)) > 23 OR  
       INT(SUBSTR(pTime,3,2)) > 59 OR  
       INT(SUBSTR(pTime,5,2)) > 59) THEN pRet = FALSE.  
   ELSE pRet = TRUE.  

   RETURN pRet.  

END FUNCTION.

&IF "{&fmake2dt}" NE "YES"
&THEN

&GLOBAL-DEFINE fmake2dt YES


FUNCTION fMake2Dt RETURNS DECIMAL
  (INPUT tsdate AS DATE, INPUT tstime AS INT):

   DEFINE VARIABLE yy  AS INTEGER NO-UNDO.
   DEFINE VARIABLE mm  AS INTEGER NO-UNDO.
   DEFINE VARIABLE dd  AS INTEGER NO-UNDO.
   DEFINE VARIABLE ret AS DECIMAL NO-UNDO format "99999999.99999".

   ASSIGN
      yy = YEAR(tsdate)
      mm = MONTH(tsdate)
      dd = DAY(tsdate).

   ret = yy * 10000 + mm * 100 + dd.
   ret = ret + (tstime / 100000).

   RETURN ret.

END.
&ENDIF
FUNCTION fHetu2Date RETURN DATE
  (INPUT lcOrgid AS CHAR ).

   def var lddate as date NO-UNDO.
   DEF VAR liyear AS i    NO-UNDO.

   if length(lcOrgId) = 11 THEN DO:
      liYear = ?.
      IF      Substring(lcOrgId,7,1) = "-" THEN liyear = 1900.
      ELSE IF Substring(lcOrgId,7,1) = "A" THEN liyear = 2000.
      ELSE if substring(lcOrgId,7,1) = "+" THEN liyear = 1800.
      
      lddate = DATE(INT(substring(lcOrgId,3,2)),
                    INT(substring(lcOrgId,1,2)),
                    INT(substring(lcOrgId,5,2)) + liyear) no-error.
      if error-status:error then lddate = ?.              
   END.
   ELSE DO:
      lddate = ?.
   END.

   RETURN lddate .

END. 

FUNCTION fDayLight RETURNS LOGICAL
  (INPUT pdeStamp AS DECIMAL):

/*
   NOTICE that when winter time starts same hour is lived twice.
   This function does not check that, so one hour will be missed at that time
*/

   DEFINE VARIABLE llSummer  AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.

   lcTmp = STRING(TRUNCATE(pdeStamp,0)).
   
   ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                  INT(SUBSTR(lcTmp,7,2)),
                  INT(SUBSTR(lcTmp,1,4))).

   lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).

   liTime = INT(lcTmp).

   /* March */
   IF MONTH(ldaDate) = 3 THEN DO:
      IF DAY(ldaDate) >= 25 THEN DO:
         IF WEEKDAY(ldaDate) = 1 AND liTime >= 3 * 3600 THEN llSummer = TRUE.
         ELSE DO:
            llSummer = TRUE.
            DO liTmp = 31 TO DAY(ldaDate) BY -1:
               IF WEEKDAY(DATE(MONTH(ldaDate),liTmp,YEAR(ldaDate))) = 1 THEN
                  llSummer = FALSE.
            END.
         END.
      END.
   END.
   /* April - September */
   ELSE IF MONTH(ldaDate) >= 4 AND MONTH(ldaDate) <= 9 THEN llSummer = TRUE.
   /* October */
   ELSE IF MONTH(ldaDate) = 10 THEN DO:
      IF DAY(ldaDate) >= 25 THEN DO:
         IF WEEKDAY(ldaDate) = 1 AND liTime >= 4 * 3600 THEN llSummer = FALSE.
         ELSE DO:
            DO liTmp = 31 TO DAY(ldaDate) BY -1:
               IF WEEKDAY(DATE(MONTH(ldaDate),liTmp,YEAR(ldaDate))) = 1 THEN
                  llSummer = TRUE.
            END.
         END.
      END.
      ELSE llSummer = TRUE.
   END.
   /* November - February */
   ELSE llSummer = FALSE.
 
   RETURN llSummer.

END.

FUNCTION fUTCTime RETURNS CHARACTER
  (INPUT pdeStamp AS DECIMAL):

   DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.

   lcDateFmt = SESSION:DATE-FORMAT.
   SESSION:DATE-FORMAT = "ymd".

   IF pdeStamp = 0 THEN pdeStamp = fMakeTS().

   lcTmp = STRING(TRUNCATE(pdeStamp,0)).
   
   ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                  INT(SUBSTR(lcTmp,7,2)),
                  INT(SUBSTR(lcTmp,1,4))).

   lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).

   liTime = INT(lcTmp).

   IF fDayLight(pdeStamp) THEN liXHour = 2.
   ELSE                        liXHour = 1.

   liTmp = INT(lcTmp) - (liXHour * 3600).

   IF liTmp < 0 THEN ASSIGN
      ldaDate = ldaDate - 1
      liTmp   = 86400 + liTmp.
   
   lcTmp = STRING(ldaDate,"9999-99-99") + "T" + STRING(liTmp,"HH:MM:SS") + "Z".

   SESSION:DATE-FORMAT = lcDateFmt.

   RETURN lcTmp.

END FUNCTION.

FUNCTION fISO860 RETURNS CHARACTER
  (INPUT pdeStamp AS DECIMAL):

   DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.

   lcDateFmt = SESSION:DATE-FORMAT.
   SESSION:DATE-FORMAT = "ymd".

   IF pdeStamp = 0 THEN pdeStamp = fMakeTS().

   lcTmp = STRING(TRUNCATE(pdeStamp,0)).
   
   ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                  INT(SUBSTR(lcTmp,7,2)),
                  INT(SUBSTR(lcTmp,1,4))).

   lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).

   liTime = INT(lcTmp).

   IF fDayLight(pdeStamp) THEN liXHour = 2.
   ELSE                        liXHour = 1.

   liTmp = INT(lcTmp) - (liXHour * 3600).

   IF liTmp < 0 THEN ASSIGN
      ldaDate = ldaDate - 1
      liTmp   = 86400 + liTmp.
   
   lcTmp = STRING(ldaDate,"9999-99-99") + "T" +
           STRING(liTmp,"HH:MM:SS")     + "+" +
           STRING(liXHour,"99")         + "00".

   SESSION:DATE-FORMAT = lcDateFmt.

   /* NOTICE: if liXHour is negative sign will be removed */
   lcTmp = REPLACE(lcTmp,"-","").

   RETURN lcTmp.

END FUNCTION.

FUNCTION fUTC2TS RETURNS DECIMAL
   (INPUT pcUTCTime AS CHARACTER):

   DEFINE VARIABLE ldaDate AS DATE      NO-UNDO.
   DEFINE VARIABLE lcTime  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liTime  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeTS   AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE llDST   AS LOG       NO-UNDO.

   ASSIGN
      ldaDate = DATE(INT(SUBSTR(pcUTCTime,6,2)),
                     INT(SUBSTR(pcUTCTime,9,2)),
                     INT(SUBSTR(pcUTCTime,1,4)))
      lcTime  = SUBSTR(pcUTCTime,12,8)
      liTime  = INT(ENTRY(1,lcTime,":")) * 3600 + 
                INT(ENTRY(2,lcTime,":")) * 60   +
                INT(ENTRY(3,lcTime,":"))
   NO-ERROR.

   IF ERROR-STATUS:ERROR THEN RETURN ?.

   ldeTS = fHMS2TS(ldaDate, lcTime).
   
   llDST = fDayLight(ldeTS).
   
   IF llDST THEN liTime = liTime + 3 * 3600.
   ELSE          liTime = liTime + 2 * 3600.
   
   IF liTime > 86399 THEN ASSIGN
      ldaDate = ldaDate + 1
      liTime  = liTime - 86399.
   
   RETURN fHMS2TS(ldaDate, STRING(liTime,"hh:mm:ss")).

END FUNCTION.

FUNCTION fTS2Unix RETURNS INTEGER
   (INPUT iStamp AS DECIMAL):

   DEFINE VARIABLE lDate AS DATE    NO-UNDO.
   DEFINE VARIABLE lTime AS INTEGER NO-UNDO.
   
   IF iStamp = 0 THEN iStamp = fMakeTS().

   fSplitTS(iStamp, OUTPUT lDate, OUTPUT lTime).

   RETURN 86400 * (lDate - 01/01/1970) + ltime. 
   
END FUNCTION.

FUNCTION fSecOffSet RETURNS DECIMAL
  (INPUT ideTS    AS DECIMAL,
   INPUT iiOffSet AS INTEGER): 

   IF ideTS > 999999999
   THEN RETURN ideTS + (0.00001 * iiOffSet).

   DEFINE VARIABLE ldtCalcTime AS DATETIME NO-UNDO.
   DEFINE VARIABLE ldTemp      AS DECIMAL  NO-UNDO.
   DEFINE VARIABLE ldtDate     AS DATE     NO-UNDO.
   DEFINE VARIABLE liTime      AS INT      NO-UNDO.

   fSplitTS(ideTS,
            OUTPUT ldtDate,
            OUTPUT liTime).
            
   ASSIGN
      ldtCalcTime = DATETIME(ldtDate,liTime * 1000)
      ldtCalcTime = ADD-INTERVAL(ldtCalcTime,       
                                 iiOffSet,
                                 "seconds").
   
   ASSIGN 
      ldtDate = DATE(ldtCalcTime)
      liTime  = MTIME(ldtCalcTime) / 1000
      ldTemp  = fMake2Dt(ldtDate,liTime).
    
   RETURN ldTemp.
   
END.

FUNCTION fOffSetTS RETURNS DECIMAL
  (INPUT piOffSet  AS INTEGER):

   DEFINE VARIABLE ldeTS    AS DECIMAL NO-UNDO.

   ldeTS = fMakeTS().

   RETURN fSecOffSet(ldeTS,
                     INTEGER(piOffSet * 3600)).
   
END.

FUNCTION fOffSet RETURNS DECIMAL
  (INPUT pdeTS    AS DECIMAL,
   INPUT piOffSet AS INTEGER):

   RETURN fSecOffSet(pdeTS,
                     INTEGER(piOffSet * 3600)).
   
END.

FUNCTION fISOTimeZone RETURNS CHAR
   (idtDate AS DATE,
    iiTime  AS INT):

   DEF VAR ldtDate2ISO AS DATETIME-TZ NO-UNDO.
   
   ldtDate2ISO = DATETIME(idtDate,iiTime * 1000).
   
   RETURN ISO-DATE(ldtDate2ISO).
   
END FUNCTION.

FUNCTION fTimeStamp2DateTime RETURNS DATETIME
   (idTimeStamp AS DEC):
   
   DEFINE VARIABLE ldtDate     AS DATE     NO-UNDO.
   DEFINE VARIABLE liTime      AS INT      NO-UNDO.

   IF idTimeStamp = 0 THEN RETURN ?.
   
   fSplitTS(idTimeStamp,
            OUTPUT ldtDate,
            OUTPUT liTime).
          
   IF ldtDate = ? THEN RETURN ?.
            
   RETURN DATETIME(ldtDate,liTime * 1000).
   
END FUNCTION.

FUNCTION fMonthlyStamps RETURNS LOGICAL
  (INPUT  pdaDate AS DATE,
   OUTPUT pdeTS1  AS DECIMAL,
   OUTPUT pdeTS2  AS DECIMAL):

   DEFINE VARIABLE ldaDate1 AS DATE NO-UNDO.
   DEFINE VARIABLE ldaDate2 AS DATE NO-UNDO.

   ASSIGN
      ldaDate1 = DATE(MONTH(pdaDate),1,YEAR(pdaDate))
      ldaDate2 = ldaDate1 + 32
      ldaDate2 = DATE(MONTH(ldaDate2),1,YEAR(ldaDate2))
      pdeTS1   = fMake2Dt(ldaDate1,0)
      pdeTS2   = fMake2Dt(ldaDate2,86400)
   NO-ERROR.

   RETURN NOT ERROR-STATUS:ERROR.

END FUNCTION.

&ENDIF


