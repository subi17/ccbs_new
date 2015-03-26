/* -----------------------------------------------------------------
  MODULE .......: NNCAPO.P
  TASK .........: Calculates peak/off-peak seconds from a single call
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 18.07.1997
  CHANGED ......:
  Version ......: M15
  ------------------------------------------------------------------ */

DEF INPUT  PARAMETER sdate  AS DA.   /* IN:  call started AT (DAY)    */
DEF INPUT  PARAMETER stime  AS INT.  /* IN:  call started AT (sec)    */
DEF INPUT  PARAMETER ttime  AS INT.  /* IN:  call ended   AT (sec)    */

DEF OUTPUT PARAMETER pksec  AS INT.  /* OUT: seconds in peak time     */
DEF OUTPUT PARAMETER opsec  AS INT.  /* OUT: seconds in off-peak time */

DEF VAR call-day     AS DA  NO-UNDO.
DEF VAR call-starts  AS INT NO-UNDO.
DEF VAR call-ends    AS INT NO-UNDO.
DEF VAR peak-starts  AS INT NO-UNDO init 28800. /* 08:00:00 */
DEF VAR peak-ends    AS INT NO-UNDO init 64800. /* 18:00:00 */
DEF VAR midnight     AS INT NO-UNDO init 86400. /* 24:00:00 */
DEF VAR debug        AS lo  NO-UNDO.
DEF VAR weekend      AS lo  NO-UNDO.

/* ------ PEAK / OFF-PEAK:----------------------

  00.00.00 - 07.59.59   OFF-PEAK
  08.00.00 - 17.59.59   PEAK
  18.00.00 - 23.59.59   OFF-PEAK

----------------------------------------------- */
/* DISP sdate stime ttime. PAUSE. */

ASSIGN
   pksec = 0
   opsec = 0
   call-day    = sdate       /* call's start Date       */
   call-starts = stime       /* call's start time       */
   call-ends   = ttime.      /* call's termination time */

/* does the call continue over midnight ? We assume that call's MAX
   Duration is 24 hours */

IF call-ends < call-starts THEN call-ends = call-ends + midnight.

DAY:
repeat:

   /* weekwnd is allways off-peek from Friday evening TO Monday morning */
   IF weekday(call-day) = 1
   OR weekday(call-day) = 7
   THEN ASSIGN weekend = TRUE.
   ELSE ASSIGN weekend = FALSE.

   /* call's FIRST part before peak starts */
   IF call-starts < peak-starts THEN DO:
      ASSIGN
      opsec = opsec + (minimum(peak-starts,call-ends) - call-starts).

      IF debug THEN DO:
    message "Alkoi ennen 8.00, off-peek sec nyt" opsec. PAUSE.
      END.

      /* did call terminate before peak-ends ? */
      IF call-ends <= peak-starts THEN LEAVE DAY.
      /* no, still there are seconds left ... */
      call-starts = peak-starts.
   END.

   /* call's seconds during the peak time */
   IF call-starts >= peak-starts AND call-starts < peak-ends THEN DO:

      /* exception: Sat AND Sun are off-pak days */
      IF weekend THEN
    opsec = opsec + ( minimum(call-ends,peak-ends) - call-starts).
      ELSE
    pksec = pksec + ( minimum(call-ends,peak-ends) - call-starts).

      IF debug THEN DO:
    message "Puheluaika 8 -18 vAlillA" pksec "sekuntia". PAUSE.
      END.

      /* did this call terminate before peak-ends ? */
      IF call-ends >= peak-ends THEN
    /* call seems TO continue after peak-ends ... */
    ASSIGN call-starts = peak-ends.
      ELSE
    /* call is already terminated, no more calculations are needed */
    LEAVE DAY.
   END.

   /* call's seconds during evening's off-peak time */
   IF call-starts >= peak-ends THEN DO:
      opsec = opsec + (minimum(call-ends,midnight) - call-starts).

      IF debug THEN DO:
    message "Puhelu jatkui klo 18.00 yli, opsec on nyt" opsec. PAUSE.
      END.

      /* did call terminate before midnight ? */
     IF call-ends > midnight THEN
    /* call seems TO continue over midnight */
    ASSIGN call-starts = 0 call-ends = call-ends - midnight.
      ELSE
    /* call is already terminated, no more calculations are needed */
    LEAVE DAY.
   END.

   IF debug THEN DO:
      message "continues on the next day". PAUSE.
   END.
   call-day = call-day + 1.
END.
IF debug THEN DO:
   PAUSE 0.
   DISP
      stime           label "Call started"
      ttime           label "Call ended"
      pksec           label "Peak sec"
      opsec           label "Off-peak sec"
      pksec + opsec   label "Tot Duration sec"
   WITH
      overlay centered row 6 title " DEBUG values for a single call " FRAME d.

   message "Press Enter !".
   PAUSE no-message.
   HIDE FRAME d.
END.

