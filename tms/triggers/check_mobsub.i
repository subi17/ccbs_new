/*
   Include file parameters:
      1 = table name
      2 = field name (which contains msseq value)
*/

DEFINE VARIABLE llMobSubIsAvailable  AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE llMobSubWasAvailable AS LOGICAL INITIAL FALSE NO-UNDO.

FUNCTION fCheckMobSub RETURNS LOGICAL
   (iiMsSeq AS INTEGER):

   FOR FIRST MobSub FIELDS (MsSeq) NO-LOCK WHERE
      MobSub.MsSeq = iiMsSeq:
      RETURN TRUE.
   END.

   RETURN FALSE.

END.

llMobSubIsAvailable = fCheckMobSub(INTEGER({1}.{2})).

IF NEW({1}) AND llMobSubIsAvailable = FALSE
THEN RETURN.

IF (NOT NEW({1})) AND {1}.{2} <> old{1}.{2}
THEN llMobSubWasAvailable = fCheckMobSub(INTEGER(old{1}.{2})).
ELSE llMobSubWasAvailable = llMobSubIsAvailable.

IF llMobSubIsAvailable = FALSE AND llMobSubWasAvailable = FALSE
THEN RETURN.