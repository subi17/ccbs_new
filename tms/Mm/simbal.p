/* ----------------------------------------------------------------------
  MODULE .......: SIMbal.p
  TASK .........: Quick SIM inventory
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 13.08.99
  CHANGED ......: 09-09-03 jp brand
                  09-08-05 mvi fixed extent bug. simstat can be 0
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}


DEF VAR xbal AS INT EXTENT 999.
DEF VAR i AS INT.
DEF VAR statname AS C NO-UNDO.

/* replace xbal-array with this temp table */
DEF TEMP-TABLE tt
FIELD n AS I 
FIELD s AS I
INDEX s s.

PAUSE 0.
VIEW FRAME LOG.
MESSAGE "Calculating SIM Cards, wait a moment ...".
FOR EACH SIM no-lock WHERE sim.Brand = gcBrand .
    FIND tt WHERE tt.s = sim.simstat NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
       ASSIGN tt.s = sim.simstat.
    END.
    tt.n = tt.n + 1.
END.

FOR EACH tt NO-LOCK.

   IF tt.n NE 0 THEN DO:
      FIND SIMStat where SIMStat.SIMStat = tt.s no-lock NO-ERROR.

      IF NOT AVAIL simstat THEN statname = "st:" + STRING(tt.s) + " n/a in DB".
      ELSE statname = simstat.ssname.
      DISP 
        tt.s     label "Status" 
        statname FORMAT "x(25)" label "St.Name"
        tt.n     label "Balance" 
      WITH 10 DOWN
      centered title " QUICK  SIM INVENTORY "
      OVERLAY ROW 5 FRAME LOG.
      DOWN WITH FRAME LOG.
   END.    
END.   
HIDE MESSAGE NO-PAUSE.

ASSIGN ufk = 0 ufk[8] = 8 ehto = 0. RUN ufkey.
HIDE FRAME LOG NO-PAUSE.

