   
CLASS Class.timedate FINAL:
   
   METHOD PUBLIC STATIC DECIMAL MakeTS():
   
      DEFINE VARIABLE yy  AS INTEGER NO-UNDO.
      DEFINE VARIABLE mm  AS INTEGER NO-UNDO.
      DEFINE VARIABLE dd  AS INTEGER NO-UNDO.
      DEFINE VARIABLE ret AS DECIMAL NO-UNDO format "99999999.99999".
   
      assign
         yy = year(today)
         mm = month(today)
         dd = day(today).
   
      ret = yy * 10000 + mm * 100 + dd.
      ret = ret + (time / 100000).
   
      return ret.
   
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL SplitTS
     (input ts as dec, output dte as date, output tme as int).
   
      DEFINE VARIABLE yy  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE mm  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE dd  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE i   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE c   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ret AS LOGICAL   NO-UNDO.
   
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL HMS2TS
      (INPUT pDate AS DATE, INPUT pTime AS CHARACTER):
   
      DEFINE VARIABLE lRet AS DECIMAL NO-UNDO.
      DEFINE VARIABLE lSec AS INTEGER NO-UNDO.
   
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER TS2HMS
      (INPUT tstamp AS DECIMAL):
   
      DEFINE VARIABLE outstring  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE dte        AS DATE      NO-UNDO.
      DEFINE VARIABLE tme        AS INTEGER   NO-UNDO.
   
      IF SplitTS(tstamp, OUTPUT dte, OUTPUT tme) THEN
         outstring = STRING(dte,"99.99.9999") + " " + STRING(tme,"hh:mm:ss").
      ELSE
         outstring = "00.00.0000 00:00:00".
   
      RETURN outstring.
   
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL CheckTime
     (INPUT pTime AS CHARACTER):
   
      DEFINE VARIABLE pRet AS LOGICAL NO-UNDO.
   
      IF (INT(SUBSTR(pTime,1,2)) > 23 OR  
          INT(SUBSTR(pTime,3,2)) > 59 OR  
          INT(SUBSTR(pTime,5,2)) > 59) THEN pRet = FALSE.  
      ELSE pRet = TRUE.  
   
      RETURN pRet.  
   
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL Make2Dt
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC DATE Hetu2Date
     (INPUT lcOrgid AS CHAR ).
   
      DEFINE VARIABLE lddate as DATE    NO-UNDO.
      DEFINE VARIABLE liyear AS INTEGER NO-UNDO.
   
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL DayLight
     (INPUT pdeStamp AS DECIMAL):
   
   /*
      NOTICE that when winter time starts same hour is lived twice.
      This METHOD does not check that, so one hour will be missed at that time
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER UTCTime
     (INPUT pdeStamp AS DECIMAL):
   
      DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.
   
      lcDateFmt = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
   
      IF pdeStamp = 0 THEN pdeStamp = MakeTS().
   
      lcTmp = STRING(TRUNCATE(pdeStamp,0)).
      
      ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                     INT(SUBSTR(lcTmp,7,2)),
                     INT(SUBSTR(lcTmp,1,4))).
   
      lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).
   
      liTime = INT(lcTmp).
   
      IF DayLight(pdeStamp) THEN liXHour = 2.
      ELSE                       liXHour = 1.
   
      liTmp = INT(lcTmp) - (liXHour * 3600).
   
      IF liTmp < 0 THEN ASSIGN
         ldaDate = ldaDate - 1
         liTmp   = 86400 + liTmp.
      
      lcTmp = STRING(ldaDate,"9999-99-99") + "T" + STRING(liTmp,"HH:MM:SS") + "Z".
   
      SESSION:DATE-FORMAT = lcDateFmt.
   
      RETURN lcTmp.
   
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER ISO860
     (INPUT pdeStamp AS DECIMAL):
   
      DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.
   
      lcDateFmt = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
   
      IF pdeStamp = 0 THEN pdeStamp = MakeTS().
   
      lcTmp = STRING(TRUNCATE(pdeStamp,0)).
      
      ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                     INT(SUBSTR(lcTmp,7,2)),
                     INT(SUBSTR(lcTmp,1,4))).
   
      lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).
   
      liTime = INT(lcTmp).
   
      IF DayLight(pdeStamp) THEN liXHour = 2.
      ELSE                       liXHour = 1.
   
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL UTC2TS
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
   
      ldeTS = HMS2TS(ldaDate, lcTime).
      
      llDST = DayLight(ldeTS).
      
      IF llDST THEN liTime = liTime + 3 * 3600.
      ELSE          liTime = liTime + 2 * 3600.
      
      IF liTime > 86399 THEN ASSIGN
         ldaDate = ldaDate + 1
         liTime  = liTime - 86399.
      
      RETURN HMS2TS(ldaDate, STRING(liTime,"hh:mm:ss")).
   
   END METHOD.
   
   METHOD PUBLIC STATIC INTEGER TS2Unix
      (INPUT iStamp AS DECIMAL):
   
      DEFINE VARIABLE lDate AS DATE    NO-UNDO.
      DEFINE VARIABLE lTime AS INTEGER NO-UNDO.
      
      IF iStamp = 0 THEN iStamp = MakeTS().
   
      SplitTS(iStamp, OUTPUT lDate, OUTPUT lTime).
   
      RETURN 86400 * (lDate - 01/01/1970) + ltime. 
      
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL OffSetTS
     (INPUT piOffSet  AS INTEGER):
   
      DEFINE VARIABLE liOffSet AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ldeTS    AS DECIMAL NO-UNDO.
      DEFINE VARIABLE ldeTemp  AS DECIMAL NO-UNDO.
      DEFINE VARIABLE liXDays  AS INTEGER NO-UNDO.
   
      liOffSet = (piOffSet * 3600) / 100000.
   
      ldeTS = MakeTS().
   
      ldeTemp = ldeTS - TRUNC(ldeTS,0).
   
      ldeTemp = liOffSet + ldeTemp.
   
      DO WHILE ldeTemp < 0:
         ASSIGN
            liXDays = liXDays - 1
            ldeTemp = ldeTemp + 0.86400.
      END.
      
      DO WHILE ldeTemp > 0.86400:
         ASSIGN
            liXDays = liXDays + 1
            ldeTemp = ldeTemp - 0.86400.
      END.
      
      ldeTemp = TRUNC(ldeTS,0) + liXDays + ldeTemp.
      
      RETURN ldeTemp.
      
   END.

   METHOD PUBLIC STATIC CHARACTER ConvertToISO8601
     (INPUT idaDate AS DATE):

      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ldeStamp  AS DEC NO-UNDO. 

      lcDateFmt = SESSION:DATE-FORMAT.

      IF idaDate = ? THEN idaDate = TODAY.

      lcTmp = SUBST("&1-&2-&3",
                    STRING(YEAR(idaDate),"9999"),
                    STRING(MONTH(idaDate),"99"),
                    STRING(DAY(idaDate),"99")) + "T" +
              STRING(liTime,"HH:MM:SS")     + "+0000".

      RETURN lcTmp.

   END METHOD.
   
END CLASS.
