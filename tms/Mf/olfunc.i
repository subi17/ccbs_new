/* NOTICE:
     even though this module has only Called functions
     IT SHOULD BE ABLE TO COMPILE IT ALONE !
*/

/* 
CREATED ...: kl 05.08.98
MODIFIED ..: kl 31.08.98 - DO FOR buff:
*/

FUNCTION fOLRefresh RETURNS logical
   /* cd = create/delete OLRefresh -file */ 
   (INPUT cd AS logical).

   DEF VAR ret AS lo NO-UNDO.

   /* in case of a LOCK, FALSE is returned TO avoid locking 
      situation when reading OLRefreshd values from database 
      BY nncdr -module */

   FIND FIRST OLRefresh no-lock no-error no-wait.

   ret = AVAIL OLRefresh.

   /* CREATE (write) OLRefresh */
   IF cd = TRUE AND ret = FALSE THEN CREATE OLRefresh.

   /* DELETE (read)  OLRefresh */
   ELSE IF cd = FALSE AND ret = TRUE THEN DO:
      FIND FIRST OLRefresh exclusive-lock no-error no-wait.
      IF NOT error-status:error THEN DO:
         DELETE  OLRefresh.
         release OLRefresh.
      END.
   END. 
   RETURN ret.

END.

/*
CREATED ...: kl january 99 in norway
MODIFIED ..: kl 06.05.99 final tuning ...
*/
FUNCTION fPType RETURNS INTEGER

/* FUNCTION FOR determining
      - the weekday
      - peak seconds
      - offpeak seconds 
   RETURNS 
      - '1' IF call started AT peak time
      - '2' IF call started AT offpeak time */    

  (INPUT  dte     AS DATE,
   INPUT  tme     AS INT,
   INPUT  dur     AS INT,
   INPUT  pstarts AS INT,
   INPUT  pends   AS INT,
   OUTPUT pksec   AS INT,
   OUTPUT opsec   AS INT).

   DEF VAR ret   AS i NO-UNDO.
   DEF VAR shour AS i NO-UNDO.
   DEF VAR ehour AS i NO-UNDO.
   DEF VAR esec  AS i NO-UNDO.
   DEF VAR ctime AS c NO-UNDO.
   DEF VAR wd    AS i NO-UNDO.

   ASSIGN
      wd    = weekday(dte)
      /* ending time in seconds */
      esec  = tme + dur
      pksec = 0
      opsec = 0
      ret   = 0.

   case wd:
      /* sunday OR saturday */
      when 1 OR when 7 THEN DO:
         ret = 2.
         /* IF ends before midnight */
         IF esec < 86400 THEN opsec = Dur.
         /* IF started on saturday */
         ELSE IF wd = 7 THEN opsec = Dur.
         ELSE DO:
            /* check the hour of ending time */
            ASSIGN
               ctime = string(esec,"hh:mm:ss")
               ehour = integer(substr(ctime,1,2)).
            /* IF ends before peak time stars */
            IF ehour < pstarts THEN opsec = Dur.
            /* IF ends during peak time */
            ELSE IF ehour >= pstarts AND ehour < pends THEN ASSIGN
               /* ending time - pstarts hours = peak seconds */
               pksec = esec - pstarts * 3600
               opsec = Dur - pksec.
            /* IF ends after peak time */
            ELSE IF ehour > pends THEN ASSIGN
               /* reduce 9 hours: peak time */
               opsec = Dur - (9 * 3600)
               pksec = Dur - opsec.
         END.
      END.
      /* week days */
      otherwise DO:
         /* check the starting hour */
         ASSIGN
            ctime = string(tme,"hh:mm:ss")
            shour = integer(substr(ctime,1,2)).
         /* started before peak time */
         IF shour < pstarts THEN DO:
            ret = 2.
            /* check the ending hour */
            ASSIGN
               ctime = string(esec,"hh:mm:ss")
               ehour = integer(substr(ctime,1,2)).
            /* ended before peak time */
            IF ehour < pstarts THEN opsec = Dur.
            /* ended within peaktime */
            ELSE IF ehour >= pstarts AND ehour < pends THEN
               ASSIGN
                  pksec = esec - (pstarts * 3600)
                  opsec = Dur - pksec.
            /* ended after peak time */
            ELSE IF ehour > pends THEN ASSIGN
               opsec = Dur - (9 * 3600) /* reduce 9 hours: peak time */
               pksec = Dur - opsec.
         END.
         /* started in peak time */
         ELSE IF shour >= pstarts AND shour < pends THEN DO:
            ret = 1.
            ASSIGN
               ctime = string(esec,"hh:mm:ss")
               ehour = integer(substr(ctime,1,2)).
            /* ended within peak time */
            IF ehour < pends THEN pksec = Dur.
            /* ended after peak time */
            ELSE ASSIGN
               opsec = esec - pends * 3600
               pksec = Dur - opsec.
         END.
         /* started after peak time */
         ELSE IF shour >= pends THEN DO:
            ret = 2.
            /* check the ending hour */
            ASSIGN
               ctime = string(esec,"hh:mm:ss")
               ehour = integer(substr(ctime,1,2)).
            /* ended before peak time */
            IF ehour < pstarts THEN opsec = Dur.
            /* ended within peak time */
            ELSE IF ehour >= pstarts AND ehour < pends THEN
               ASSIGN
                  pksec = esec - (pstarts * 3600)
                  opsec = Dur - pksec.
            /* ended after peak time */
            ELSE IF ehour >= pends THEN ASSIGN
               opsec = Dur.
         END.
      END.
   END.

   RETURN ret.

END. /* FUNCTION fPType */

/*
CREATED ...: kl 03.06.99
MODIFIED ..: 
*/

FUNCTION fCDRAmt RETURNS INTEGER
  (INPUT swid AS CHAR,
   INPUT DAY  AS DATE,
   INPUT tme  AS INT,
   INPUT junk AS LOG).

   DEF VAR mytime AS i   NO-UNDO.
   DEF VAR ret    AS i   NO-UNDO.
   DEF VAR loop   AS i   NO-UNDO.
   DEF VAR rid    AS rec NO-UNDO.

   DEF BUFFER CDRAmt FOR CDRAmt.

   /* plus 1 because extents start from 1, NOT 0 */
   mytime = integer(substr(string(tme,"hh:mm:ss"),1,2)) + 1.

   CAMT:
   repeat:
      FIND FIRST CDRAmt where
                 CDRAmt.ExCode = swid   AND
                 CDRAmt.Date  = DAY
      exclusive-lock no-error no-wait.

      IF NOT AVAIL CDRAmt THEN DO:
         CREATE CDRAmt.
         ASSIGN
            CDRAmt.ExCode = swid
            CDRAmt.Date  = DAY
         no-error.
         IF error-status:error THEN UNDO CAMT, NEXT CAMT.
      END.

      /* complete amount of cdrs */

      /* NOT invoicable cdrs */
      IF junk THEN DO:
         ASSIGN
            CDRAmt.QtyTot[mytime]  = CDRAmt.QtyTot[mytime] + 1
            CDRAmt.QtyJunk[mytime] = CDRAmt.QtyJunk[mytime] + 1
         no-error.
         IF error-status:error THEN UNDO CAMT, NEXT CAMT.
         ELSE DO:
            rid = recid(CDRAmt).
            LEAVE CAMT.
         END.
      END.
      /* invoicable cdrs */
      ELSE DO:
         ASSIGN
            CDRAmt.QtyTot[mytime] = CDRAmt.QtyTot[mytime] + 1
            CDRAmt.QtyDB[mytime]  = CDRAmt.QtyDB[mytime] + 1
         no-error.
         IF error-status:error THEN UNDO CAMT, NEXT CAMT.
         ELSE DO:
            rid = recid(CDRAmt).
            LEAVE CAMT.
         END.
      END.

   END.  

   FIND FIRST CDRAmt where
        recid(CDRAmt) = rid
   no-lock no-error.

   DO loop = 1 TO 24:
      ret = ret + CDRAmt.QtyTot[loop].
   END.

   RETURN ret.

END.

/* add "/" TO END of a directory path IF needed */
FUNCTION fChkPath RETURN CHAR
  (INPUT p AS CHAR).

  if substr(p,length(p),1) ne "/" then p = p + "/".

  RETURN p.

END.

FUNCTION fDa2De RETURNS DECIMAL
  (INPUT pDate AS DATE, INPUT pSec AS INT).

   DEF VAR yy  AS i NO-UNDO.
   DEF VAR mm  AS i NO-UNDO.
   DEF VAR dd  AS i NO-UNDO.
   def var ret as de no-undo format "99999999.99999".

   ASSIGN
      yy  = year(pDate)
      mm  = month(pDate)
      dd  = day(pDate)
      ret = yy * 10000 + mm * 100 + dd
      ret = ret + (pSec / 100000).

   RETURN ret.

END.



