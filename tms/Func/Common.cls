 
 /*------------------------------------------------------------------------
    File        : Common
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Oct 30 16:58:45 EET 2017
    Notes       : 
  CONTENTS......: Time and date methods
                     - mDateFmt
                     - mAddCent
                     - mSec2C
                  Timestamp methods
                     - mMakeTS
                     - mSplitTS
                     - mTS2HMS
                     - mHMS2TS
                  Misc methods
                     - mTMSCodeName
                     - mTMSCodeList
                     - mTMSCodeConfigValue
                     - mHideBSub
                     - mReplaceSMS
                     - mGetHdrText
                     - mChkSaldoAccount
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Func.Common FINAL: 

   /* METHOD PUBLIC STATIC FOR converting Date FORMAT */
   METHOD PUBLIC STATIC CHARACTER mDateFmt
      (INPUT d AS DATE, INPUT f AS CHARACTER):
   
      DEFINE VARIABLE yy   AS i.
      DEFINE VARIABLE mm   AS i.
      DEFINE VARIABLE dd   AS i.
      DEFINE VARIABLE cy   AS c.
      DEFINE VARIABLE cm   AS c.
      DEFINE VARIABLE cd   AS c.
      DEFINE VARIABLE i    AS i.
      DEFINE VARIABLE sep  AS c.
      DEFINE VARIABLE amt  AS i EXTENT 5.
      DEFINE VARIABLE pos  AS i EXTENT 3.
      DEFINE VARIABLE spos AS i EXTENT 2.
      DEFINE VARIABLE ret  AS c.
      DEFINE VARIABLE b-ok AS lo.
   
      /* separate INTEGER values */
      ASSIGN
         yy = YEAR(d)
         mm = MONTH(d)
         dd = DAY(d).
   
      /* scan YEAR - MONTH - DAY FORMAT */
      DO i = 1 TO LENGTH(f):
   
         amt[5] = amt[5] + 1.
   
         CASE substr(f,i,1):
   
            WHEN "y" THEN DO:
               amt[1] = amt[1] + 1.
               IF pos[1] = 0 THEN pos[1] = i.
            END.
   
            WHEN "m" THEN DO:
               amt[2] = amt[2] + 1.
               IF pos[2] = 0 THEN pos[2] = i.
            END.
   
            WHEN "d" THEN DO:
               amt[3] = amt[3] + 1.
               IF pos[3] = 0 THEN pos[3] = i.
            END.
   
            OTHERWISE DO:
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
            cy = STRING(yy,"9999")
            cm = STRING(mm,"99")
            cd = STRING(dd,"99").
   
         /* FORMAT lengths */
         cy = substr(cy,LENGTH(cy) + 1 - amt[1]).
         IF mm < 10 THEN
            cm = substr(cm,LENGTH(cm) + 1 - amt[2]).
         ELSE amt[2] = 2.
         IF dd < 10 THEN
            cd = substr(cd,LENGTH(cd) + 1 - amt[3]).
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
   METHOD PUBLIC STATIC INTEGER mAddCent
     (INPUT yyy AS INTEGER).
   
      DEFINE VARIABLE ret AS i NO-UNDO.
   
      ret = SESSION:YEAR-OFFSET.
   
      CASE ret - yyy <= truncate(ret / 100,0) * 100.
   
         WHEN FALSE THEN ret = (TRUNCATE(ret / 100,0) + 1) * 100.
   
         WHEN TRUE  THEN ret = TRUNCATE(ret / 100,0) * 100.
   
      END.   
   
      RETURN ret + yyy.
   
   END.   

   METHOD PUBLIC STATIC CHARACTER mSec2C
     (INPUT tme      AS DECIMAL,
      INPUT iiLength AS INTEGER).
       
      DEFINE VARIABLE hh    AS de   NO-UNDO.
      DEFINE VARIABLE mm    AS i    NO-UNDO.
      DEFINE VARIABLE ss    AS i    NO-UNDO.
      DEFINE VARIABLE lcDur AS CHARACTER NO-UNDO.
      
      IF iiLength = 0 THEN RETURN "". 
   
      ASSIGN 
         hh       = TRUNCATE(tme / 3600,0)
         tme      = tme - (hh * 3600)
         mm       = TRUNCATE(tme / 60,0)
         ss       = tme MODULO 60
         lcDur    = STRING(hh,"->>>>>>99") + ":" +
                    string(mm,"99")        + ":" +
                    string(ss,"99")
         iiLength = min(iiLength,15)
         lcDur    = SUBSTRING(lcDur,16 - iiLength).
                       
      RETURN lcDur.                 
   
   END.

   METHOD PUBLIC STATIC DECIMAL mMakeTS():
   
      DEFINE VARIABLE yy  AS i NO-UNDO.
      DEFINE VARIABLE mm  AS i NO-UNDO.
      DEFINE VARIABLE dd  AS i NO-UNDO.
      DEFINE VARIABLE ret AS de NO-UNDO FORMAT "99999999.99999".
   
      ASSIGN
         yy = YEAR(TODAY)
         mm = MONTH(TODAY)
         dd = DAY(TODAY).
   
      ret = yy * 10000 + mm * 100 + dd.
      ret = ret + (TIME / 100000).
   
      RETURN ret.
   
   END.

   /** This method converts date to Timestamp 
       eg. 01/01/2005 -> 20050101.00000
   */
   METHOD PUBLIC STATIC DECIMAL mDate2TS
     (pdtDate AS DATE):
      RETURN mMake2DT(pdtDate,0).
   END METHOD. 

   METHOD PUBLIC STATIC LOGICAL mSplitTS
     (INPUT ts AS DECIMAL, OUTPUT dte AS DATE, OUTPUT tme AS INTEGER).
   
      DEFINE VARIABLE yy  AS i  NO-UNDO.
      DEFINE VARIABLE mm  AS i  NO-UNDO.
      DEFINE VARIABLE dd  AS i  NO-UNDO.
      DEFINE VARIABLE i   AS i  NO-UNDO.
      DEFINE VARIABLE c   AS c  NO-UNDO.
      DEFINE VARIABLE ret AS lo NO-UNDO.
   
      ASSIGN
         c   = substr(STRING(ts,"99999999.99999"),1,8)
         yy  = INTEGER(substr(c,1,4))
         mm  = INTEGER(substr(c,5,2))
         dd  = INTEGER(substr(c,7,2))
         dte = DATE(mm,dd,yy)
         c   = substr(STRING(ts,"99999999.99999"),10,5)
         tme = INTEGER(c)
      no-error.
   
      IF ERROR-STATUS:ERROR THEN ret = FALSE.
      ELSE ret = TRUE.
   
      RETURN ret.
   
   END.

   METHOD PUBLIC STATIC CHARACTER mTS2HMS
      (INPUT tstamp AS DECIMAL).
   
      DEFINE VARIABLE outstring  AS C  NO-UNDO.
      DEFINE VARIABLE dte        AS DA NO-UNDO.
      DEFINE VARIABLE tme        AS I  NO-UNDO.
   
      IF Func.Common:mSplitTS(tstamp, OUTPUT dte, OUTPUT tme) THEN
         outstring = STRING(dte,"99.99.9999") + " " + STRING(tme,"hh:mm:ss").
      ELSE
         outstring = "00.00.0000 00:00:00".
   
      RETURN outstring.
   
   END METHOD.

   METHOD PUBLIC STATIC DECIMAL mHMS2TS
     (INPUT pDate AS DATE, INPUT pTime AS CHARACTER).
   
      DEFINE VARIABLE lRet AS DECIMAL.
      DEFINE VARIABLE lSec AS INTEGER.
   
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

   METHOD PUBLIC STATIC DATE mTSToDate
     (INPUT ts AS DECIMAL):
   
      DEFINE VARIABLE yy  AS i  NO-UNDO.
      DEFINE VARIABLE mm  AS i  NO-UNDO.
      DEFINE VARIABLE dd  AS i  NO-UNDO.
      DEFINE VARIABLE c   AS c  NO-UNDO.
      DEFINE VARIABLE dte AS DATE NO-UNDO.
   
      ASSIGN
         c   = substr(STRING(ts,"99999999.99999"),1,8)
         yy  = INTEGER(substr(c,1,4))
         mm  = INTEGER(substr(c,5,2))
         dd  = INTEGER(substr(c,7,2))
         dte = DATE(mm,dd,yy)
      no-error.
   
      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
      
      RETURN dte.
   
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mTS2Date
     (INPUT ts AS DECIMAL, OUTPUT dte AS DATE).
   
      dte = mTSToDate(ts).
   
      IF dte EQ ?
      THEN RETURN FALSE.
      
      RETURN TRUE.
   
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mCheckTime
     (INPUT pTime AS CHARACTER):
   
      DEFINE VARIABLE pRet AS LOGICAL NO-UNDO.
   
      IF (INT(SUBSTR(pTime,1,2)) > 23 OR  
          INT(SUBSTR(pTime,3,2)) > 59 OR  
          INT(SUBSTR(pTime,5,2)) > 59) THEN pRet = FALSE.  
      ELSE pRet = TRUE.  
   
      RETURN pRet.  
   
   END METHOD.

   METHOD PUBLIC STATIC LOGICAL mDayLight
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mUTCTime
     (INPUT pdeStamp AS DECIMAL):
   
      DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.
   
      lcDateFmt = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
   
      IF pdeStamp = 0 THEN pdeStamp = Func.Common:mMakeTS().
   
      lcTmp = STRING(TRUNCATE(pdeStamp,0)).
      
      ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                     INT(SUBSTR(lcTmp,7,2)),
                     INT(SUBSTR(lcTmp,1,4))).
   
      lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).
   
      liTime = INT(lcTmp).
   
      IF mDayLight(pdeStamp) THEN liXHour = 2.
      ELSE                        liXHour = 1.
   
      liTmp = INT(lcTmp) - (liXHour * 3600).
   
      IF liTmp < 0 THEN ASSIGN
         ldaDate = ldaDate - 1
         liTmp   = 86400 + liTmp.
      
      lcTmp = STRING(ldaDate,"9999-99-99") + "T" + STRING(liTmp,"HH:MM:SS") + "Z".
   
      SESSION:DATE-FORMAT = lcDateFmt.
   
      RETURN lcTmp.
   
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mISO860
     (INPUT pdeStamp AS DECIMAL):
   
      DEFINE VARIABLE ldaDate   AS DATE      NO-UNDO.
      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liTmp     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE liXHour   AS INTEGER   NO-UNDO.
   
      lcDateFmt = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
   
      IF pdeStamp = 0 THEN pdeStamp = Func.Common:mMakeTS().
   
      lcTmp = STRING(TRUNCATE(pdeStamp,0)).
      
      ldaDate = DATE(INT(SUBSTR(lcTmp,5,2)),
                     INT(SUBSTR(lcTmp,7,2)),
                     INT(SUBSTR(lcTmp,1,4))).
   
      lcTmp = STRING((pdeStamp - TRUNCATE(pdeStamp,0)) * 100000).
   
      liTime = INT(lcTmp).
   
      IF mDayLight(pdeStamp) THEN liXHour = 2.
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
   
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL mSecOffSet
     (INPUT ideTS    AS DECIMAL,
      INPUT iiOffSet AS INTEGER): 
   
      IF ideTS > 99999999
      THEN RETURN ideTS + (0.00001 * iiOffSet).
   
      DEFINE VARIABLE ldtCalcTime AS DATETIME NO-UNDO.
      DEFINE VARIABLE ldTemp      AS DECIMAL  NO-UNDO.
      DEFINE VARIABLE ldtDate     AS DATE     NO-UNDO.
      DEFINE VARIABLE liTime      AS INTEGER      NO-UNDO.
   
      Func.Common:mSplitTS(ideTS,
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
         ldTemp  = Func.Common:mMake2DT(ldtDate,liTime).
       
      RETURN ldTemp.
      
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL mOffSetTS
     (INPUT piOffSet  AS INTEGER):
   
      DEFINE VARIABLE ldeTS    AS DECIMAL NO-UNDO.
   
      ldeTS = Func.Common:mMakeTS().
   
      RETURN Func.Common:mSecOffSet(ldeTS,
                        INTEGER(piOffSet * 3600)).
      
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL mOffSet
     (INPUT pdeTS    AS DECIMAL,
      INPUT piOffSet AS INTEGER):
   
      RETURN Func.Common:mSecOffSet(pdeTS,
                        INTEGER(piOffSet * 3600)).
      
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mISOTimeZone
      (idtDate AS DATE,
       iiTime  AS INTEGER):
   
      DEFINE VARIABLE ldtDate2ISO AS DATETIME-TZ NO-UNDO.
      
      ldtDate2ISO = DATETIME(idtDate,iiTime * 1000).
      
      RETURN ISO-DATE(ldtDate2ISO).
      
   END METHOD.
   
   METHOD PUBLIC STATIC DATETIME mTimeStamp2DateTime
      (idTimeStamp AS DECIMAL):
      
      DEFINE VARIABLE ldtDate     AS DATE     NO-UNDO.
      DEFINE VARIABLE liTime      AS INTEGER      NO-UNDO.
   
      IF idTimeStamp = 0 THEN RETURN ?.
      
      Func.Common:mSplitTS(idTimeStamp,
               OUTPUT ldtDate,
               OUTPUT liTime).
             
      IF ldtDate = ? THEN RETURN ?.
               
      RETURN DATETIME(ldtDate,liTime * 1000).
      
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL mDateTimeTZ2TS (INPUT idtDateTimeTz AS DATETIME-TZ) :
       
       DEFINE VARIABLE ideTimeStamp AS DECIMAL NO-UNDO.
       DEFINE VARIABLE idtTzDate    AS DATE    NO-UNDO.
       DEFINE VARIABLE iTzMtime      AS INTEGER NO-UNDO.
       
       ASSIGN 
           idtTzDate = DATE(idtDateTimeTz)
           iTzMtime  = INTEGER(TRUNCATE((MTIME(idtDateTimeTz) / 1000 ) ,0)).       
       
       ASSIGN ideTimeStamp = Func.Common:mMake2DT(idtTzDate,iTzMtime).
       
       RETURN ideTimeStamp.
              
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL mMonthlyStamps
     (INPUT  pdaDate AS DATE,
      OUTPUT pdeTS1  AS DECIMAL,
      OUTPUT pdeTS2  AS DECIMAL):
   
      DEFINE VARIABLE ldaDate1 AS DATE NO-UNDO.
      DEFINE VARIABLE ldaDate2 AS DATE NO-UNDO.
   
      ASSIGN
         ldaDate1 = DATE(MONTH(pdaDate),1,YEAR(pdaDate))
         ldaDate2 = ldaDate1 + 32
         ldaDate2 = DATE(MONTH(ldaDate2),1,YEAR(ldaDate2))
         pdeTS1   = Func.Common:mMake2DT(ldaDate1,0)
         pdeTS2   = Func.Common:mMake2DT(ldaDate2,86400)
      NO-ERROR.
   
      RETURN NOT ERROR-STATUS:ERROR.
   
   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mDayname
    (pdt AS DATE):
      DEFINE VARIABLE lcDaylist AS CHARACTER FORMAT "X(9)" 
         INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
      RETURN ENTRY(WEEKDAY(pdt),lcDaylist).
   END METHOD.  
   
   METHOD PUBLIC STATIC CHARACTER mTs2C
    (pdeTs AS DECIMAL):
      DEFINE VARIABLE ldtDate AS DATE    NO-UNDO.
      DEFINE VARIABLE liSecs  AS INTEGER NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO FORMAT "x(23)".
      mSplitTS(pdeTs,ldtDate,liSecs).
      lcReturnValue = mDayname(ldtDate) + " " + mTS2HMS(pdeTs).
      RETURN lcReturnValue.
   END METHOD. 
   
   /* Get the First day of the next month  */
   METHOD PUBLIC STATIC DATE mFirstDayOfNextMonth
      (INPUT iDate AS DATE).
      
      IF MONTH(iDate) = 12 THEN RETURN DATE(1, 1, YEAR(iDate) + 1).
      ELSE RETURN DATE(MONTH(iDate) + 1, 1, YEAR(iDate)).
   END METHOD.

   /* Get the last day of the month */
   METHOD PUBLIC STATIC DATE mLastDayOfMonth
      (INPUT iDate AS DATE).
      
      IF MONTH(iDate) = 12 THEN RETURN DATE(12, 31, YEAR(iDate)).
      ELSE RETURN DATE(MONTH(iDate) + 1, 1, YEAR(iDate)) - 1.
   END METHOD.
   
   /* Get period after offsetting specified months from current period */
   METHOD PUBLIC STATIC INTEGER mOffsetMonthsToPeriod
      (INPUT iOffsetMonths AS INTEGER):
   
      DEFINE VARIABLE liFromPeriod   AS INTEGER NO-UNDO.
      DEFINE VARIABLE liOffsetYear   AS INTEGER NO-UNDO.
      DEFINE VARIABLE liOffsetMonths AS INTEGER NO-UNDO.
   
      IF iOffsetMonths < MONTH(TODAY) THEN
         liFromPeriod = YEAR(TODAY) * 100 + (MONTH(TODAY) - iOffsetMonths).
      ELSE DO:
         IF iOffsetMonths > 12 THEN
            ASSIGN liOffsetYear   = (iOffsetMonths - (iOffsetMonths MOD 12)) / 12
                   liOffsetMonths  = (iOffsetMonths MOD 12).
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
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mISO8601Date
     (INPUT idaDate AS DATE):
   
      DEFINE VARIABLE liTime    AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcDateFmt AS CHARACTER NO-UNDO.
      DEFINE VARIABLE ldeStamp  AS DECIMAL NO-UNDO. 
   
      lcDateFmt = SESSION:DATE-FORMAT.
      SESSION:DATE-FORMAT = "ymd".
   
      IF idaDate = ? THEN idaDate = TODAY.
   
      ldeStamp = Func.Common:mHMS2TS(idaDate,"12:00:00").
   
      lcTmp = STRING((ldeStamp - TRUNCATE(ldeStamp,0)) * 100000).
      liTime = INT(lcTmp).
      
      lcTmp = STRING(idaDate,"99999999") + "T" +
              STRING(liTime,"HH:MM:SS")     + "+0000".
   
      SESSION:DATE-FORMAT = lcDateFmt.
   
      RETURN lcTmp.
   
   END METHOD.

   METHOD PUBLIC STATIC CHARACTER mSec2MinC
      (idTime   AS INTEGER,
       iiLength AS INTEGER):
   
      DEFINE VARIABLE liMin   AS INTEGER  NO-UNDO.
      DEFINE VARIABLE liSec   AS INTEGER  NO-UNDO.
      DEFINE VARIABLE lcDur   AS CHARACTER NO-UNDO. 
      
      IF iiLength = 0 THEN RETURN "".
   
      ASSIGN liMin    = TRUNCATE(idTime / 60,0)
             liSec    = idTime MOD 60
             lcDur    = STRING(liMin,"->>>>>>99") + ":" +
                        STRING(liSec,"99")
             iiLength = MIN(iiLength,12)
             lcDur    = SUBSTRING(lcDur,13 - iiLength).
             
      RETURN lcDur.
      
   END METHOD.

   /* -----------------
      Misc METHOD PUBLIC STATICs
   ------------------ */   
   
   /* get the description for status codes etc. */
   METHOD PUBLIC STATIC CHARACTER mTMSCodeName
      (iTableName AS CHARACTER,
       iFieldName AS CHARACTER,
       iCodeValue AS CHARACTER).

      DEFINE BUFFER TMSCodes FOR TMSCodes.
   
      FIND FIRST TMSCodes NO-LOCK WHERE
                 TMSCodes.TableName = iTableName AND
                 TMSCodes.FieldName = iFieldName AND
                 TMSCodes.CodeValue = iCodeValue NO-ERROR.
   
      IF AVAILABLE TMSCodes AND TMSCodes.InUse > 0 THEN RETURN TMSCodes.CodeName.
      ELSE RETURN "".
   
   END METHOD.
   
   /* check that used code is valid */
   METHOD PUBLIC STATIC LOGICAL mTMSCodeChk 
      (iTableName AS CHARACTER,
       iFieldName AS CHARACTER,
       iCodeValue AS CHARACTER).

      DEFINE BUFFER TMSCodes FOR TMSCodes.
   
      RETURN CAN-FIND(FIRST TMSCodes WHERE
                      TMSCodes.TableName = iTableName AND
                      TMSCodes.FieldName = iFieldName AND
                      TMSCodes.CodeValue = iCodeValue AND
                      TMSCodes.InUse > 0).
   
   END METHOD.
   
   /* get a list of all codes */
   METHOD PUBLIC STATIC CHARACTER mTMSCodeList
      (iTableName AS CHARACTER,
       iFieldName AS CHARACTER).
   
      DEFINE VARIABLE lcCodeLst AS CHARACTER NO-UNDO.
   
      DEFINE BUFFER TMSCodes FOR TMSCodes.
      
      FOR EACH TMSCodes NO-LOCK WHERE
               TMSCodes.TableName = iTableName AND
               TMSCodes.FieldName = iFieldName AND
               TMSCodes.InUse > 0:
   
         lcCodeLst = lcCodeLst          + 
                     (IF lcCodeLst > ""
                      THEN CHR(1)
                      ELSE "")          + 
                     TMSCodes.CodeValue + 
                     CHR(9)             +
                     TMSCodes.CodeName. 
      END.                  
   
      RETURN lcCodeLst.
   
   END METHOD.

   /* get the configuration value for status codes etc. */
   METHOD PUBLIC STATIC CHARACTER mTMSCodeConfigValue
      (iTableName AS CHARACTER,
       iFieldName AS CHARACTER,
       iCodeValue AS CHARACTER).
   
       DEFINE BUFFER TMSCodes FOR TMSCodes.

       FIND FIRST TMSCodes NO-LOCK WHERE
                  TMSCodes.TableName = iTableName AND
                  TMSCodes.FieldName = iFieldName AND
                  TMSCodes.CodeValue = iCodeValue NO-ERROR.
   
       IF AVAILABLE TMSCodes THEN RETURN TMSCodes.ConfigValue.
       ELSE RETURN "".
   
   END METHOD.   
   
   METHOD PUBLIC STATIC CHARACTER mHideBSub
     (INPUT str-in AS CHARACTER, 
      INPUT CustNo AS INTEGER,
      INPUT bdest  AS CHARACTER,
      INPUT iiDestType AS INTEGER,
      INPUT prefix AS CHARACTER,
      INPUT bHide  AS LOG).
   
      DEFINE VARIABLE clean       AS lo NO-UNDO.
      DEFINE VARIABLE loop        AS i  NO-UNDO.
      DEFINE VARIABLE Qty         AS i  NO-UNDO.
      DEFINE VARIABLE repl        AS c  NO-UNDO.
      DEFINE VARIABLE showbnumber AS c NO-UNDO.
   
      /* move default values to cparam */
      qty = 3.
      repl = "X".
      showbnumber = "3,15".  
   
      clean = TRUE.
   
      IF iiDestType = 9 THEN DO:
         IF str-in BEGINS "0100" OR 
            str-in BEGINS "0101" OR 
            str-in BEGINS "0200" OR 
            str-in BEGINS "0202" OR 
            str-in BEGINS "0209" OR 
            str-in BEGINS "0300" OR 
            str-in BEGINS "060" OR 
            str-in BEGINS "0700" OR 
            str-in BEGINS "106" OR 
            str-in BEGINS "100"
         THEN bhide = FALSE.
         ELSE bhide = TRUE.
      END.
      ELSE IF LOOKUP(STRING(iiDestType),showbnumber) > 0 THEN bhide = FALSE.   
   
      IF LENGTH(str-in) > Qty + 1 AND bHide THEN DO 
      loop = LENGTH(str-in) TO (LENGTH(str-in) - Qty + 1) BY -1.
         IF INDEX("0123456789",SUBSTRING(str-in,loop,1)) > 0 THEN
         SUBSTR(str-in,loop,1) = repl.
      END.
   
      RETURN str-in.
   END.
   
   METHOD PUBLIC STATIC CHARACTER mGetHdrText
      (INPUT iiTxtNbr   AS INTEGER, 
       INPUT iiLanguage AS INTEGER).
   
      DEFINE BUFFER HdrText FOR HdrText.

      FIND FIRST HdrText NO-LOCK WHERE 
                 HdrText.Brand  = "1"   AND
                 HdrText.te-nro = iiTxtNbr  AND
                 HdrText.te-kie = iiLanguage NO-ERROR.
      IF AVAILABLE HdrText THEN RETURN HdrText.te-text.
   
      /* if text on chosen language is not available use default */
      FIND FIRST HdrText NO-LOCK WHERE 
                 HdrText.Brand  = "1"  AND
                 HdrText.te-nro = iiTxtNbr AND  
                 HdrText.te-kie = 1 NO-ERROR.
      IF AVAILABLE HdrText THEN RETURN HdrText.te-text.
   
      /* nothing was found */
      RETURN "".
   
   END METHOD.
   
   
   METHOD PUBLIC STATIC DECIMAL mChkSaldoAccount
            (INPUT   CustNo       AS INTEGER,
             INPUT   Cli          AS CHARACTER,
             INPUT   period       AS INTEGER,
             INPUT   lcSaldofatime AS CHARACTER).
   
   
      DEFINE VARIABLE ldsaldo AS DECIMAL  NO-UNDO.
      ldSaldo =  0.
      
      IF period = 0 THEN Period = YEAR(TODAY) * 100 +     MONTH(TODAY).

      DEFINE BUFFER FaTime FOR FaTime.
      
      FOR EACH FaTime WHERE
               FaTime.CustNum  = CustNo        AND
               FaTime.Cli      = Cli           AND
               FaTime.ftgrp    = lcSaldoFatime AND 
               Fatime.Invnum   = 0             AND
               Fatime.Period  <= Period NO-LOCK.
         
         IF      Fatime.Period = period THEN ldsaldo = ldsaldo + Fatime.Amt.
         ELSE IF Fatime.Period < period AND 
                 Fatime.Transperiod > 0 THEN  ldsaldo = ldsaldo + Fatime.Amt.
      END.
      
      RETURN ldSaldo.
   
   END.

   METHOD PUBLIC STATIC DECIMAL mMake2DT
      (INPUT ldaTSDate AS DATE,
       INPUT liTSTime  AS INTEGER):
   
      DEFINE VARIABLE liYY   AS INTEGER NO-UNDO.
      DEFINE VARIABLE liMM   AS INTEGER NO-UNDO.
      DEFINE VARIABLE liDD   AS INTEGER NO-UNDO.
      DEFINE VARIABLE ldeRet AS DECIMAL NO-UNDO FORMAT "99999999.99999".

      ASSIGN
         liYY   = YEAR(ldaTSDate)
         liMM   = MONTH(ldaTSDate)
         liDD   = DAY(ldaTSDate)
         ldeRet = liYY * 10000 + liMM * 100 + liDD.
         ldeRet = ldeRet + (liTSTime / 100000)         
         .

      RETURN ldeRet.
      
   END METHOD.
   
   METHOD PUBLIC STATIC DECIMAL mMakeOfficeTS():
   
      DEFINE VARIABLE ldeCurrentTS AS DE NO-UNDO FORMAT "99999999.99999".
      DEFINE VARIABLE ldeSeconds   AS DE NO-UNDO FORMAT "9.999999".
      DEFINE VARIABLE liCompare    AS I  NO-UNDO.                  
      DEFINE VARIABLE ldeNewStamp  AS DE NO-UNDO FORMAT "99999999.99999".  
   
      ASSIGN
         ldeCurrentTS = Func.Common:mMakeTS()
         ldeSeconds   = ldeCurrentTS - (INT(SUBSTRING(STRING(ldeCurrentTS),1,8)))
         liCompare    = ldeSeconds * 100000.
   
      IF    liCompare < 8  * 3600 THEN DO:
         ldeNewStamp = Func.Common:mMake2DT(TODAY, 8 * 3600).
      END.
      ELSE  IF liCompare > 21 * 3600 THEN DO:
         ldeNewStamp = Func.Common:mMake2DT(TODAY + 1, 8 * 3600).
      END.   
      ELSE DO:
         ldeNewstamp = ldeCurrentTS.
      END.
   
      RETURN ldeNewStamp.
   
   END METHOD. 
   
   /* duration between two timestamps */
   METHOD PUBLIC STATIC INTEGER mTSDuration
      (INPUT  idFromStamp AS DECIMAL,
       INPUT  idToStamp   AS DECIMAL,
       OUTPUT oiSeconds   AS INTEGER).
   
      DEFINE VARIABLE liDays  AS INTEGER  NO-UNDO.
      DEFINE VARIABLE ldtFrom AS DATE NO-UNDO.
      DEFINE VARIABLE liFrom  AS INTEGER  NO-UNDO.
      DEFINE VARIABLE ldtTo   AS DATE NO-UNDO.
      DEFINE VARIABLE liTo    AS INTEGER  NO-UNDO.
      
      IF idToStamp <= idFromStamp THEN RETURN 0.
      
      Func.Common:mSplitTS(idFromStamp,
               OUTPUT ldtFrom,
               OUTPUT liFrom).
      Func.Common:mSplitTS(idToStamp,
               OUTPUT ldtTo,
               OUTPUT liTo).
               
      liDays = ldtTo - ldtFrom.
          
      IF liTo < liFrom THEN ASSIGN
         liDays    = liDays - 1
         oiSeconds = 86400 - liFrom + liTo.
      
      ELSE oiSeconds = liTo - liFrom.
      
      RETURN liDays.
       
   END METHOD.
   
   /* customer's name to be displayed */
   METHOD PUBLIC STATIC CHARACTER mDispCustName
      (BUFFER ibNameCust FOR Customer). 
      
      IF NOT AVAILABLE ibNameCust THEN RETURN "". 
   
     /* company name may be divided into two rows */
      IF ibNameCust.CustIDType = "CIF" AND ibNameCust.CompanyName > "" THEN
         RETURN ibNameCust.CompanyName + 
                (IF ibNameCust.CoName > "" 
                 THEN " " + ibNameCust.CoName
                 ELSE "").
       
      /* private customers have both lastname and firstname */
      ELSE RETURN ibNameCust.FirstName + " " + ibNameCust.CustName + 
                  (IF ibNameCust.SurName2 > "" 
                   THEN " " + ibNameCust.SurName2
                   ELSE "").
      
   END METHOD.
   
   /* customer's name to be printed */
   METHOD PUBLIC STATIC CHARACTER mPrintCustName
      (BUFFER ibNameCust FOR Customer). 
      
      IF NOT AVAILABLE ibNameCust THEN RETURN "".
   
      /* company name may be divided into two rows, but it is printing routine's
         job to use or not use COName */
      IF ibNameCust.CustIDType = "CIF" AND ibNameCust.CompanyName > "" THEN
         RETURN ibNameCust.CompanyName.
       
      /* private customers have both lastname and firstname */
      ELSE RETURN ibNameCust.FirstName + " " + ibNameCust.CustName + 
                  (IF ibNameCust.SurName2 > "" 
                   THEN " " + ibNameCust.SurName2
                   ELSE "").
       
   END METHOD.
       
   METHOD PUBLIC STATIC CHARACTER mDispOrderName
      (BUFFER ibNameOrder FOR OrderCustomer). 
      
      IF NOT AVAILABLE ibNameOrder THEN RETURN "".
      
      IF ibNameOrder.CustIDType = "CIF" AND ibNameOrder.Company > "" THEN
         RETURN ibNameOrder.Company.
   
      /* private customers have both lastname and firstname */
      ELSE RETURN ibNameOrder.FirstName + " " + ibNameOrder.SurName1 + 
                  (IF ibNameOrder.SurName2 > "" 
                   THEN " " + ibNameOrder.SurName2
                   ELSE ""). 
      
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mPrintOrderName
      (BUFFER ibNameOrder FOR OrderCustomer). 
      
      IF NOT AVAILABLE ibNameOrder THEN RETURN "".
      
      IF ibNameOrder.CustIDType = "CIF" AND ibNameOrder.Company > "" THEN
         RETURN ibNameOrder.Company.
   
      /* private customers have both lastname and firstname */
      ELSE RETURN ibNameOrder.FirstName + " " + ibNameOrder.SurName1 + 
                  (IF ibNameOrder.SurName2 > "" 
                   THEN " " + ibNameOrder.SurName2
                   ELSE ""). 
       
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL mWriteMemoWithType
      (icHostTable AS CHARACTER,
       icKeyValue  AS CHARACTER,
       iiCustNum   AS INTEGER,
       icTitle     AS CHARACTER,
       icText      AS CHARACTER,
       icType      AS CHARACTER,
       icCreUser   AS CHARACTER).
       
      DEFINE BUFFER Memo FOR Memo.

      CREATE Memo.
      ASSIGN Memo.Brand     = Syst.Var:gcBrand
             Memo.HostTable = icHostTable
             Memo.KeyValue  = icKeyValue
             Memo.CustNum   = iiCustNum
             Memo.Memotype  = icType
             Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
             Memo.CreUser   = icCreUser 
             Memo.MemoTitle = icTitle
             Memo.MemoText  = icText
             Memo.CreStamp  = Func.Common:mMakeTS().
   
      RELEASE Memo.
      
      RETURN TRUE. 
      
   END METHOD.
   
   METHOD PUBLIC STATIC LOGICAL mWriteMemo
      (icHostTable AS CHARACTER,
       icKeyValue  AS CHARACTER,
       iiCustNum   AS INTEGER,
       icTitle     AS CHARACTER,
       icText      AS CHARACTER).
   
      RETURN mWriteMemoWithType(icHostTable,icKeyValue,iiCustNum,icTitle,icText,"", Syst.Var:katun).
   END METHOD.

   
   METHOD PUBLIC STATIC LOGICAL mReplaceSMS
     (INPUT icCustName AS CHARACTER, /* Customer.CustName */
      INPUT icCLI      AS CHARACTER, /* Mobsub.CLI */
      INPUT icText     AS CHARACTER,       /* text        */
      INPUT iimsseq    AS INTEGER,
      INPUT idate      AS DATE,
      OUTPUT ocText    AS CHARACTER).      /* output Text */

      DEFINE BUFFER bRepSubSer     FOR SubSer.
      DEFINE BUFFER bRepSubserPara FOR SubserPara.
      DEFINE BUFFER KillMS FOR KillMS.
      DEFINE BUFFER MobSub FOR MobSub.
      DEFINE BUFFER msisdn FOR msisdn.
                        
      DEFINE VARIABLE loop AS i NO-UNDO.
   
      ASSIGN octext = icText.
   
      ASSIGN ocText = REPLACE(ocText,"#OWNER",icCustName)
             ocText = REPLACE(ocText,"#CLI",icCLI)
             ocText = REPLACE(ocText,"#MSISDN",icCLI)
             ocText = REPLACE(ocText,"#MASINS",icCustName)
             ocText = REPLACE(ocText,"#NUMBER","number")
             ocText = REPLACE(ocText,"#PVM",STRING(idate,"99-99-9999")).
   
      IF INDEX(ocText,"#TERMDATE") > 0  THEN DO:
         FIND KillMS  WHERE 
              Killms.msseq = iimsseq NO-LOCK NO-ERROR.
                
         IF AVAILABLE Killms THEN ASSIGN
            ocText = REPLACE(ocText,"#TERMDATE",
                                    STRING(killms.killdate,"99-99-9999")).
      END.
   
      IF INDEX(ocText,"#PORTDAYANDTIME") > 0 THEN DO:
         FIND FIRST mobsub WHERE 
                    Mobsub.msseq = iimsseq NO-LOCK NO-ERROR.
         IF AVAILABLE mobsub THEN 
         FIND FIRST msisdn WHERE 
                    msisdn.cli = mobsub.cli NO-LOCK NO-ERROR.
         IF AVAILABLE msisdn AND msisdn.portingdate NE ? THEN ASSIGN
            ocText = REPLACE(ocText,"#PORTDAYANDTIME",
                             STRING(msisdn.portingdate,"99.99.9999") + " " + 
                             REPLACE(STRING(msisdn.portingtime,"99.99"),",",":")).
         ELSE  ocText = REPLACE(ocText,"#PORTDAYANDTIME","").
      END.
         
      IF INDEX(ocText,"#FAX") > 0 THEN DO:
         FIND FIRST bRepSubSer WHERE 
                    bRepSubSer.msseq = iimsseq  AND 
                    bRepSubSer.servcom = "T62" NO-LOCK NO-ERROR.
         IF AVAILABLE bRepSubSer THEN 
         ocText = REPLACE(ocText,"#FAX",bRepSubSer.ssparam).
      END.   
   
      IF INDEX(ocText,"#DATA") > 0 THEN DO:
         FIND FIRST bRepSubSer WHERE 
                    bRepSubSer.msseq = iimsseq  AND 
                    bRepSubSer.servcom = "B16" NO-LOCK NO-ERROR.
         IF AVAILABLE bRepSubSer THEN 
            ocText = REPLACE(ocText,"#DATA",bRepSubSer.ssparam) .
         ELSE 
            octext = REPLACE(ocText,"#DATA","tuntematon").
      END.
   
      IF INDEX(ocText,"#DCF") > 0 THEN DO:
         FIND FIRST bRepSubSerPara WHERE
                    bRepSubSerPara.msseq    = iimsseq   AND
                    bRepSubSerPara.servcom  = "PP2"     AND 
                    bRepSubserPara.Paraname = "MSISDN2" 
         NO-LOCK NO-ERROR.
   
         IF AVAILABLE bRepSubSerPara THEN 
            ocText = REPLACE(ocText,"#DCF",bRepSubSerPara.ParaValue).
         ELSE ocText = REPLACE(ocText,"#DCF","").                         
      END.
   
      IF INDEX(ocText,"#NUMSMSBUNDLE") > 0 THEN DO:
         FIND FIRST bRepSubSer WHERE 
                    bRepSubSer.msseq = iimsseq  AND 
                    bRepSubSer.servcom = "SMSBundle" NO-LOCK NO-ERROR.
         IF AVAILABLE bRepSubSer THEN 
            ocText = REPLACE(ocText,"#NUMSMSBUNDLE",bRepSubSer.ssparam) .
      END.
   
      IF INDEX(ocText,"#EXPDATESMSBUNDLE") > 0 THEN DO:
         ocText = REPLACE(ocText,"#EXPDATESMSBUNDLE",STRING((idate + 30 ),"99-99-9999")) .
      END.
   END.
   
   METHOD PUBLIC STATIC DATE mGetDate (icDate AS CHARACTER) :
       
       DEFINE VARIABLE liMonth AS INTEGER NO-UNDO.
       DEFINE VARIABLE liDay   AS INTEGER NO-UNDO.
       DEFINE VARIABLE liYear  AS INTEGER NO-UNDO.
        
       ASSIGN
           liDay   = INTEGER(ENTRY(3,icDate,"-")) 
           liMonth = INTEGER(ENTRY(2,icDate,"-"))
           liYear  = INTEGER(ENTRY(1,icDate,"-"))
           NO-ERROR.
           
       IF ERROR-STATUS:ERROR 
       THEN RETURN ? . 
        
       RETURN DATE(liMonth,liDay,liYear).
       
   END METHOD.
   
   METHOD PUBLIC STATIC CHARACTER mGetTMSParamValue ( INPUT icGroupCode AS CHARACTER,
                                                      INPUT icParamCode AS CHARACTER,
                                                      INPUT icParamType AS CHARACTER) :
       
       DEFINE BUFFER bfTMSParam FOR TMSParam.
       
       IF icGroupCode > "" THEN 
           FIND FIRST bfTMSParam WHERE bfTMSParam.Brand      = Syst.Var:gcBrand
                                   AND bfTMSParam.ParamGroup = icGroupCode
                                   AND bfTMSParam.ParamCode  = icParamCode
                                 NO-LOCK NO-ERROR.
       
       ELSE 
           FIND FIRST bfTMSParam WHERE bfTMSParam.Brand     = Syst.Var:gcBrand
                                   AND bfTMSParam.ParamCode = icParamCode
                                 NO-LOCK NO-ERROR.
                       
       IF NOT AVAILABLE bfTMSParam THEN RETURN "".
       
       CASE icParamType :
            
           WHEN {&CHARACTER_PARAM} THEN             
               RETURN bfTMSParam.CharVal.
           WHEN {&DATE_PARAM} THEN             
               RETURN STRING(bfTMSParam.DateVal,"99/99/99").
           WHEN {&DECIMAL_PARAM} THEN             
               RETURN STRING(bfTMSParam.DecVal).
           WHEN {&INTEGER_PARAM} THEN 
               RETURN STRING(bfTMSParam.IntVal).
           OTHERWISE .
           
       END CASE.                  
       
       RETURN "".
       
   END METHOD.
   
   METHOD PUBLIC STATIC INT64 mConvertSpeedtoBits(INPUT icSpeed AS CHARACTER):
       
       IF INDEX(icSpeed,"K") > 0 THEN
       DO:
           ASSIGN 
               icSpeed = REPLACE(icSpeed,"K","").
           RETURN (INTEGER(icSpeed) * 1024).
       END.       
       ELSE IF INDEX(icSpeed,"M") > 0 THEN
       DO:
           ASSIGN 
               icSpeed = REPLACE(icSpeed,"M","").
           RETURN (INTEGER(icSpeed) * 1024 * 1024).
       END.       
       ELSE IF INDEX(icSpeed,"G") > 0 THEN
           DO:
               ASSIGN 
                   icSpeed = REPLACE(icSpeed,"G","").
               RETURN (INTEGER(icSpeed) * 1024 * 1024 * 1024).
           END. 
           ELSE      
               RETURN INTEGER(icSpeed).
   END METHOD.

END CLASS.