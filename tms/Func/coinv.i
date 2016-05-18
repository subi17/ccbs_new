&IF "{&COINV_I}" NE "YES"
&THEN
&GLOBAL-DEFINE COINV_I YES

/* CONTRACT INVOIDE INCLUDE File WITH Period & Date FUNCTIONS 
   NOTICE !!!: works only within 12 MONTH periods 
   
   changes:    08.11.05/aam error-handling to fInt2Date
*/   


/* muutettu apia varten */   

/* FOR contract fee dates from the Period */
FUNCTION fPer2Date RETURNS Date
   (INPUT per AS INT, INPUT cint AS INT).

   DEF VAR yy AS i NO-UNDO.
   DEF VAR mm AS i NO-UNDO.

   yy = truncate(per / 100,0).
   mm = per MOD yy.

   IF cint > 0 THEN DO:
      mm = mm + cint.
      DO WHILE mm > 12:
         ASSIGN mm = mm - 12
                yy = yy + 1.
      END.
   END.

   RETURN date(mm,1,yy).

END.

/* FOR NEW Period WITH Interval checking */ 
FUNCTION fPer2Per RETURNS INTEGER
   (INPUT per AS INT, INPUT cint AS INT).

   DEF VAR yy AS i NO-UNDO.
   DEF VAR mm AS i NO-UNDO.

   yy = truncate(per / 100,0).
   mm = per MOD yy.

   IF cint > 0 THEN DO:
      IF mm - cint < 1 THEN 
         ASSIGN
            mm = 12 - (cint - mm)
            yy = yy - 1.
      ELSE mm = mm - cint.
   END.

   RETURN 100 * yy + mm.

END.

FUNCTION fPer2PerAdd RETURNS INTEGER
   (INPUT per AS INT, INPUT cint AS INT).

   DEF VAR yy AS i NO-UNDO.
   DEF VAR mm AS i NO-UNDO.

   yy = truncate(per / 100,0).
   mm = per MOD yy.

   IF cint > 0 THEN DO:
      IF mm + cint > 12 THEN
         ASSIGN
            mm = 12 + (cint - mm)
            yy = yy + 1.
      ELSE mm = mm + cint.
   END.

   RETURN 100 * yy + mm.

END.


/* counts conserning periods due TO period/type/interval */
FUNCTION fCPer RETURNS INTEGER
   (INPUT  per  AS INT,
    INPUT  type AS INT,
    INPUT  cint  AS INT,
    OUTPUT per1 AS INT,
    OUTPUT per2 AS INT).

   DEF VAR yy AS i.
   DEF VAR mm AS i.
   DEF VAR rc AS i.

   yy = truncate(per / 100,0).
   mm = per MOD yy.

   /* 1st conserning Period */
   case type:
      when 1 THEN DO:
         IF mm + 1 > 12 THEN
            ASSIGN mm = 1 yy = yy + 1.
         ELSE mm = mm + 1.
         per1 = 100 * yy + mm.
      END.
      when 2 THEN DO:
         per1 = per.
      END.
      when 3 THEN DO:
         IF mm - 1 < 1 THEN
            per2 = (yy - 1) * 100 + 12.
         ELSE per2 = yy * 100 + (mm - 1).
         IF mm - cint < 1 THEN
            ASSIGN mm = 12 - (cint - mm) yy = yy - 1.
         ELSE mm = mm - cint.
         per1 = 100 * yy + mm.
      END.
   END.

   /* 2nd conserning Period when RepType = 1 OR 2 */
   IF type NE 3 THEN DO:
      IF mm + cint - 1 > 12 THEN 
         ASSIGN mm = mm + cint - 12 - 1 yy = yy + 1.
      ELSE mm = mm + cint - 1.
      per2 = yy * 100 + mm.
   END.

   IF per1 = 0 THEN rc = rc - 1.
   IF per2 = 0 THEN rc = rc - 2.

   RETURN rc.

END. /* FUNCTION */



 /* convert INTEGER VALUE (yyyymmdd) TO Date  */
FUNCTION fInt2Date RETURNS Date
   (INPUT  per  AS INT,
    INPUT  calldate AS INT).

   DEF VAR perdate AS Date NO-UNDO.
   DEF VAR i       AS i    NO-UNDO.
   DEF VAR dd1     AS Date   NO-UNDO.
   DEF VAR per1    AS Date NO-UNDO.

   IF per > 10000000 THEN 
   per1  =  DATE(INT(SUBSTRING(STRING(per),5,2)), 
                   INT(SUBSTRING(STRING(per),7,2)),
                   INT(SUBSTRING(STRING(per),1,4))) NO-ERROR.

   ELSE IF calldate = 1 THEN 
   per1 = DATE(INT(SUBSTRING(STRING(per),5,2)),
               1,
               INT(SUBSTRING(STRING(per),1,4))) NO-ERROR.
   ELSE DO:
      ASSIGN
      dd1    = DATE(INT(SUBSTRING(STRING(per),5,2)),
               1,
             INT(SUBSTRING(STRING(per),1,4)))
      per1   = dd1 + 26 NO-ERROR.

      IF per1 NE ? THEN REPEAT:
         IF month(per1) = month(dd1) THEN 
           per1 = per1 + 1.
         ELSE DO: per1 = per1 - 1. LEAVE. END.
      END.   
   END.

   RETURN per1.
END. /* FUNCTION */

&ENDIF
