/* -----------------------------------------------
  MODULE .......: INVRUNLOG.P
  FUNCTION .....: InvRunLog maintenance, create new / delete unused
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED  .....: 03.06.02 kl
  MODIFIED .....: 12.09.03/aam brand

  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF INPUT PARAMETER iPeriod1 AS I  NO-UNDO FORMAT "999999".
DEF INPUT PARAMETER iPeriod2 AS I  NO-UNDO FORMAT "999999".

DEF VAR iPeriod  AS I  NO-UNDO FORMAT "999999".
DEF VAR daPeriod AS DA NO-UNDO.
DEF VAR iTmp     AS I  NO-UNDO.
DEF VAR iMth     AS I  NO-UNDO.
DEF VAR iCode    AS I  NO-UNDO.
DEF VAR lMsg     AS C  NO-UNDO.

FUNCTION fGetCode RETURNS INTEGER
  (INPUT pCode AS INT):

   IF pCode mod 5 = 0 THEN pCode = pCode + 6.
   ELSE                    pCode = pCode + 1.

   RETURN pCode.

END FUNCTION.

FUNCTION fCheckPeriod RETURNS CHARACTER
  (INPUT pPeriod1 AS INT, INPUT pPeriod2 AS INT):

   DEF VAR lErrNum AS INT  NO-UNDO INIT 0.
   DEF VAR lErrMsg AS CHAR NO-UNDO INIT "".

   /* 0: do not add new periods */
   IF pPeriod1 = 0 AND pPeriod2 = 0 THEN lErrNum = 0.
   /* 1: not format 999999 */
   ELSE IF LENGTH(STRING(pPeriod1)) NE 6 OR
           LENGTH(STRING(pPeriod2)) NE 6 THEN lErrNum = 1.
   /* 2: to period month over 12 */
   ELSE IF INT(SUBSTR(STRING(pPeriod2),5)) NE 12 THEN lErrNum = 2.
   /* 3: period1 > period2 */
   ELSE IF pPeriod1 > pPeriod2 THEN lErrNum = 3.

   CASE lErrNum:
      WHEN 1 THEN lErrMsg = "Period(s) in wrong format, should be \"999999\"".
      WHEN 2 THEN lErrMsg = "Ending period is not end of a year:".
      WHEN 3 THEN lErrMsg = "Periods are in wrong order:".
   END.
   IF lErrNum NE 0 THEN lErrMSg = lErrMsg + CHR(10) +
      STRING(pPeriod1) + "-" + STRING(pPeriod2).

   RETURN lErrMsg.

END FUNCTION.

lMsg = fCheckPeriod(iPeriod1,iPeriod2).
IF lMsg  NE "" THEN DO:
   MESSAGE lMsg VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
ELSE IF iPeriod1 + iPeriod2 NE 0 THEN DO:

   ASSIGN iTmp = INT(iPeriod1 / 100).

   FOR EACH NatHoliday NO-LOCK WHERE
       YEAR(NatHoliday.Holiday) = iTmp:

      iCode = iCode + 1.

   END.         

   IF iCode = 0 THEN DO:
      MESSAGE 
         "Mid-week holidays are not defined for starting period" skip
         "This run is rejected !"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.

   ASSIGN 
      iCode = 0
      iTmp  = INT(iPeriod2 / 100).

   FOR EACH NatHoliday no-lock WHERE
       YEAR(NatHoliday.Holiday) = iTmp:

      iCode = iCode + 1.

   END.         
   IF iCode = 0 THEN DO:
      MESSAGE 
         "Mid-week holidays are not defined for ending period" skip
         "This run is rejected !"
      VIEW-AS ALERT-BOX ERROR.
     RETURN.
   END.

   DO iPeriod = iPeriod1 TO iPeriod2:

      ASSIGN
         daPeriod = DATE(iPeriod mod 100,1,int(iPeriod / 100))
         iMth     = MONTH(daPeriod)
         daPeriod = daPeriod - 1
         iCode    = 11.

      PERIOD:
      DO WHILE MONTH(daPeriod + 1) = iMth:

         ASSIGN daPeriod = daPeriod + 1.

         CASE WEEKDAY(daPeriod):
            WHEN 1 OR WHEN 7 THEN NEXT PERIOD.
            OTHERWISE DO:

               FIND FIRST NatHoliday WHERE
                          NatHoliday.Holiday = daPeriod
               NO-LOCK NO-ERROR.

               IF AVAIL NatHoliday THEN NEXT PERIOD.
               ELSE DO:

                  FOR EACH InvGroup no-lock WHERE
                           InvGroup.Brand    = gcBrand AND
                           InvGroup.BillPerm = TRUE:

                     FIND FIRST InvRunLog WHERE
                                InvRunLog.Brand   = gcBrand          AND
                                InvRunLog.Period  = iPeriod          AND
                                InvRunLog.Date    = daPeriod         AND
                                InvRunLog.InvCode = iCode            AND
                                InvRunLog.InvGroup = InvGroup.InvGroup
                     NO-LOCK NO-ERROR.
                     IF AVAIL InvRunLog THEN NEXT.

                     CREATE InvRunLog.
                     ASSIGN
                        InvRunLog.Brand   = gcBrand 
                        InvRunLog.Period  = iPeriod
                        InvRunLog.Date    = daPeriod
                        InvRunLog.InvCode = iCode
                        InvRunLog.InvGroup = InvGroup.InvGroup.

                  END.

                  IF month(daPeriod + 1) NE iMth AND iCode NE 45 THEN 
                  DO WHILE iCode < 45:

                     iCode = iCode + 1.

                     FOR EACH InvGroup no-lock WHERE
                              InvGroup.Brand    = gcBrand AND
                              InvGroup.BillPerm = TRUE:

                        FIND FIRST InvRunLog WHERE
                                   InvRunLog.Brand   = gcBrand  AND
                                   InvRunLog.Period  = iPeriod  AND
                                   InvRunLog.Date    = daPeriod AND
                                   InvRunLog.InvCode = iCode    AND
                                   InvRunLog.InvGroup = InvGroup.InvGroup
                        NO-LOCK NO-ERROR.
                        IF AVAIL InvRunLog THEN NEXT.

                        CREATE InvRunLog.
                        ASSIGN
                           InvRunLog.Brand   = gcBrand
                           InvRunLog.Period  = iPeriod
                           InvRunLog.Date    = daPeriod
                           InvRunLog.InvCode = iCode
                           InvRunLog.InvGroup = InvGroup.InvGroup.

                     END.

                  END.

               END. /* NOT NatHoliday */

            END. /* OTHERWISE */

         END. /* CASE WEEKDAY */

         IF iCode = 45 THEN LEAVE PERIOD.
         ELSE iCode = fGetCode(iCode).

      END. /* DO WHILE MONTH(daPeriod + 1) = iMth */

   END. /* iPeriod1 to iPeriod2 */

END. /* iPeriod1 + iPeriod2 NE 0 */

RUN pCleanLog.

PROCEDURE pCleanLog:

   FOR EACH InvRunLog EXCLUSIVE-LOCK WHERE
            InvRunLog.Brand = gcBrand AND
            InvRunLog.State = 0 AND
        NOT CAN-FIND(FIRST InvGroup WHERE
                           InvGroup.Brand    = gcBrand AND
                           InvGroup.InvGroup = InvRunLog.InvGroup):

      DELETE InvRunLog.

   END.

END PROCEDURE.
