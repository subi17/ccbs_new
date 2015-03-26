&IF DEFINED(NOTIMEINCLUDES) = 0 &THEN

/* Which timezone to assume when client sends none in dateTime: local or UTC */
&GLOBAL-DEFINE IF_NO_TIMEZONE_ASSUME local
/* How to send dateTimes: timezone, UTC or local */
&GLOBAL-DEFINE DATETIME_SENDS local

FUNCTION split_ts RETURNS LOGICAL
     (INPUT ts AS DECIMAL, OUTPUT dte AS DATE, OUTPUT tme AS INT):

     DEF VAR yy  AS INTEGER  NO-UNDO.
     DEF VAR mm  AS INTEGER  NO-UNDO.
     DEF VAR dd  AS INTEGER  NO-UNDO.
     DEF VAR i   AS INTEGER  NO-UNDO.
     DEF VAR s   AS CHARACTER NO-UNDO.
     DEF VAR ret AS LOGICAL NO-UNDO.

     ASSIGN
         s   = SUBSTR(STRING(ts,"99999999.99999"),1,8)
         yy  = INTEGER(SUBSTR(s,1,4))
         mm  = INTEGER(SUBSTR(s,5,2))
         dd  = INTEGER(SUBSTR(s,7,2))
         dte = DATE(mm,dd,yy)
         s   = SUBSTR(STRING(ts,"99999999.99999"),10,5)
         tme = INTEGER(s)
     NO-ERROR.

     IF ERROR-STATUS:ERROR THEN RETURN FALSE.
     ELSE RETURN TRUE.

END FUNCTION.


FUNCTION _iso8601_to_timestamp RETURN DECIMAL
      ( pcin AS CHAR ):

    DEF VAR lipos AS INT NO-UNDO.
    DEF VAR lcDate AS CHAR NO-UNDO.
    DEF VAR lcTime AS CHAR NO-UNDO.

    lipos = INDEX(pcin, "T").
    IF lipos = 0 THEN RETURN ERROR.
    lcDate = SUBSTRING(pcin, 1, lipos - 1).
    lcTime = SUBSTRING(pcin, lipos + 1).

    lcDate = REPLACE(REPLACE(REPLACE(lcDate, "-", ""), ".", ""), "/", "").

    DEF VAR liTime  AS INTEGER  NO-UNDO.

    liTime = ((INT(ENTRY(1, lcTime, ":"))
              * 60 + INT(ENTRY(2, lcTime, ":")))
              * 60 + INT(ENTRY(1, ENTRY(3, lcTime, ":"), "."))).
    RETURN DECIMAL(lcDate) + (liTime / 100000).

END FUNCTION.

FUNCTION _iso8601_to_date RETURN DATE
      ( pcin AS CHAR ):
    DEF VAR lipos AS INT NO-UNDO.
    DEF VAR lcDate AS CHAR NO-UNDO.

    lcDate = ENTRY(1, pcin, "T").
    lcDate = REPLACE(REPLACE(REPLACE(lcDate, "-", ""), ".", ""), "/", "").

    RETURN DATE(INT(SUBSTRING(lcDate, 5, 2)), INT(SUBSTRING(lcDate, 7, 2)),
                INT(SUBSTRING(lcDate, 1, 4))).
END FUNCTION.

FUNCTION _iso8601_to_datetime RETURN DATETIME-TZ
      ( pcin AS CHAR ):
    DEF VAR lcDate AS CHAR NO-UNDO.

    lcDate = REPLACE(ENTRY(1, pcin, "T"), "-", "").
    lcDate = SUBSTRING(lcDate, 7, 2) + SUBSTRING(lcDate, 5, 2) +
             SUBSTRING(lcDate, 1, 4).
    ENTRY(1, pcin, "T") = lcDate.
    pcin = REPLACE(pcin, "Z", "+00:00").
    IF "{&IF_NO_TIMEZONE_ASSUME}" EQ "UTC" THEN DO:
        IF NOT SUBSTRING(pcin, LENGTH(pcin) - 5, 1) EQ "+" AND
           NOT SUBSTRING(pcin, LENGTH(pcin) - 5, 1) EQ "-" THEN
            pcin = pcin + "+00:00".
    END. /* ELSE local is assumed by DATETIME-TZ */

    RETURN DATETIME-TZ(pcin).
END FUNCTION.
&ENDIF
