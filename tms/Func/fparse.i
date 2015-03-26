/* ----------------------------------------------------------------------
  module .......: Func/fparse.i
  task .........: Parse Function
  application ..: tms
  author .......: vikas
  created ......: 25.08.11
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&FPARSE_I}" NE "YES" 
&THEN

&GLOBAL-DEFINE FPARSE_I YES

FUNCTION fParseKVP RETURNS CHAR (INPUT icKey    AS CHAR,
                                 INPUT icList   AS CHAR,
                                 INPUT icDelim  AS CHAR):

   DEF VAR liCount        AS INT  NO-UNDO.
   DEF VAR liNumEntries   AS INT  NO-UNDO.
   DEF VAR lcParam        AS CHAR NO-UNDO.
   DEF VAR lcParamKey     AS CHAR NO-UNDO.
   DEF VAR lcParamValue   AS CHAR NO-UNDO.

   liNumEntries = NUM-ENTRIES(icList, icDelim).

   DO liCount = 1 TO liNumEntries:
      lcParam = ENTRY(liCount, icList, icDelim).

      IF NUM-ENTRIES(lcParam, "=") <> 2 THEN NEXT.
      ASSIGN lcParamKey   = ENTRY(1, lcParam, "=")
             lcParamValue = ENTRY(2, lcParam, "=").

      IF lcParamKey = icKey THEN RETURN lcParamValue.
   END. /* DO liCount = 1 TO liNumEntries: */

   RETURN "".

END FUNCTION.

&ENDIF