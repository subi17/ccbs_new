/*------------------------------------------------------------------------
    File        : HPDTime
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Tue Oct 28 14:42:43 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDTime FINAL:

   DEFINE PUBLIC PROPERTY CurrUTCTime     AS CHARACTER   NO-UNDO
      GET.
      PRIVATE SET.

   DEFINE PUBLIC PROPERTY FileTime AS CHARACTER NO-UNDO
      GET.
      PRIVATE SET.

   DEFINE PUBLIC PROPERTY CurrLocalTZTime AS DATETIME-TZ NO-UNDO
      GET.
      SET (INPUT ltzdtTime AS DATETIME-TZ):
         ASSIGN
            CurrLocalTZTime = ltzdtTime
            CurrUTCTime     = ISO-DATE(DATETIME-TZ(ltzdtTime,0))
            FileTime        = REPLACE(
                              REPLACE(
                              SUBSTRING(REPLACE(ISO-DATE(ltzdtTime),"T",{&HPDTimeSeparator}),1,19)
                              ,"-","")
                              ,":","")
            .
      END SET.


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


   METHOD PUBLIC STATIC DECIMAL mMake2DT
      (INPUT idtDateTime AS DATETIME):

      IF idtDateTime = ?
      THEN RETURN ?.

      RETURN mMake2DT(DATE(idtDateTime), MTIME(idtDateTime)).

   END METHOD.


   METHOD PUBLIC STATIC DATETIME mTS2DateTime
      ( ideTS AS DECIMAL ):

      DEFINE VARIABLE liYY    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE liMM    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE liDD    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE ldaDate AS DATE     NO-UNDO.
      DEFINE VARIABLE liTime  AS INTEGER  NO-UNDO.

      ASSIGN
         liYY    = TRUNCATE(ideTS,0)
         liTime  = (ideTS - liYY) * 100000000
         liMM    = liYY MOD 10000
         liDD    = liMM MOD 100
         liYY    = (liYY - liMM) / 10000
         liMM    = (liMM - liDD) / 100 
         ldaDate = DATE(liMM,liDD,liYY)
      NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN ?.

      RETURN DATETIME(ldaDate, liTime).

   END METHOD.


   METHOD PUBLIC STATIC CHARACTER mTS2DateTimeTZ
      ( ideTS AS DECIMAL ):

      DEFINE VARIABLE ldtDateTime AS DATETIME NO-UNDO.

      ldtDateTime = mTS2DateTime(ideTS).

      IF ldtDateTime = ?
      THEN RETURN ?.

      RETURN ISO-DATE(DATETIME-TZ(ldtDateTime)).

   END METHOD.


   METHOD PUBLIC STATIC INTEGER mNextPeriod
      (iiPeriod AS INTEGER):
      
      DEFINE VARIABLE liYY AS INTEGER NO-UNDO.
      DEFINE VARIABLE liMM AS INTEGER NO-UNDO.
   
      ASSIGN
         liYY = TRUNCATE(iiPeriod / 100,0)
         liMM = (iiPeriod MOD 100) + 1
         .
      
      IF liMM  = 13
      THEN ASSIGN
             liYY = liYY + 1
             liMM = 1
             .
      RETURN (liYY * 100 + liMM).      
      
   END METHOD.


   METHOD PUBLIC STATIC INTEGER mPreviousPeriod
      (iiPeriod AS INTEGER):

      DEFINE VARIABLE liYY AS INTEGER NO-UNDO.
      DEFINE VARIABLE liMM AS INTEGER NO-UNDO.

      ASSIGN
         liYY = TRUNCATE(iiPeriod / 100,0)
         liMM = (iiPeriod MOD 100) - 1
         .

      IF liMM  = 0
      THEN ASSIGN
             liYY = liYY - 1
             liMM = 12
             .
      RETURN (liYY * 100 + liMM).

   END METHOD.


   METHOD PUBLIC STATIC DATE mLastDayOfMonth
      (idaDate AS DATE):

      IF MONTH(idaDate) = 12
      THEN RETURN DATE(12, 31, YEAR(idaDate)).

      RETURN DATE(MONTH(idaDate) + 1, 1, YEAR(idaDate)) - 1.

   END METHOD.


   METHOD PUBLIC STATIC INTEGER mMake2Time
      (INPUT ideTS     AS DECIMAL):
      
      DEFINE VARIABLE liReturnValue AS INTEGER NO-UNDO.
      
      liReturnValue = INTEGER((ideTS - TRUNCATE(ideTS,0)) * 100000) NO-ERROR.
      
      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
      
      RETURN liReturnValue. 
      
   END METHOD.
   
   
   METHOD PUBLIC STATIC DATE mMake2Date
      (INPUT ideTS     AS DECIMAL):

      DEFINE VARIABLE liYY    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE liMM    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE liDD    AS INTEGER  NO-UNDO.
      DEFINE VARIABLE ldaDate AS DATE     NO-UNDO.

      ASSIGN
         liYY  = TRUNCATE(ideTS,0)
         liMM  = liYY MOD 10000
         liDD  = liMM MOD 100
         liYY  = (liYY - liMM) / 10000
         liMM  = (liMM - liDD) / 100 
         ldaDate = DATE(liMM,liDD,liYY)
         NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
      
      RETURN ldaDate.         
      
   END METHOD.


   METHOD PUBLIC STATIC DECIMAL mSecOffSet
      (INPUT ideTS    AS DECIMAL,
       INPUT iiOffSet AS INTEGER):

      DEFINE VARIABLE ldaDate AS DATE    NO-UNDO.
      DEFINE VARIABLE liTime  AS INTEGER NO-UNDO.

      ASSIGN
         ldaDate = mMake2Date(ideTS)
         liTime  = mMake2Time(ideTS)
         .

      IF ldaDate = ? OR liTime = ?
      THEN RETURN ?.

      IF liTime + iiOffSet > 86399
      THEN RETURN mMake2DT(ldaDate + 1, (liTime + iiOffSet) MOD 86400).

      IF liTime + iiOffSet < 0
      THEN RETURN mMake2DT(ldaDate - 1, 86400 + (liTime + iiOffSet)).

      RETURN mMake2DT(ldaDate, liTime + iiOffSet).

   END METHOD.


   METHOD PUBLIC STATIC CHARACTER mTS2DateHMS
      (INPUT ldeTS AS DECIMAL):
         
      DEFINE VARIABLE ldaDate AS DATE    NO-UNDO.
      DEFINE VARIABLE liTime  AS INTEGER NO-UNDO.
      
      ASSIGN
         ldaDate = mMake2Date(ldeTS)
         liTime  = mMake2Time(ldeTS)
         .
      
      IF ldaDate = ? OR liTime = ?
      THEN RETURN ?.
      
      ELSE RETURN STRING(DAY(ldaDate),"99")    + "." + 
                  STRING(MONTH(ldaDate),"99")  + "." +
                  STRING(YEAR(ldaDate),"9999") + " " +
                  STRING(liTime, "hh:mm:ss").
   
   END METHOD.

   METHOD PUBLIC STATIC DECIMAL mDateHMS2TS
      (INPUT icDateHMS AS CHARACTER):
         
      DEFINE VARIABLE ldeTS AS DECIMAL NO-UNDO.
      DEFINE VARIABLE lcTime AS CHARACTER NO-UNDO.
      
      ASSIGN
         lcTime = SUBSTRING(icDateHMS,12)
         ldeTS = INTEGER(SUBSTRING(ENTRY(3,icDateHMS,"."),1,4)) * 10000 +
                 INTEGER(ENTRY(2,icDateHMS,".")) * 100 +
                 INTEGER(ENTRY(1,icDateHMS,".")) +
                 ( INTEGER(ENTRY(1,lcTime,":")) * 3600 + 
                   INTEGER(ENTRY(2,lcTime,":")) * 60   +
                   INTEGER(ENTRY(3,lcTime,":")) ) 
                   / 100000
         NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN RETURN ?.
      
      RETURN ldeTS.
   
   END METHOD.
   
END CLASS.