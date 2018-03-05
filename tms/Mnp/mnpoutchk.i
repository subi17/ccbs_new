&IF "{&MNPOUTCHK_I}" NE "YES"
&THEN
&GLOBAL-DEFINE MNPOUTCHK_I YES

{Syst/tmsconst.i}

FUNCTION fIsMNPOutOngoing RETURNS LOGICAL
(icCLI AS CHAR):

   DEF BUFFER bMNPSubChk FOR MNPSub.
   DEF BUFFER bMNPProcessChk FOR MNPProcess.

   FOR EACH bMNPSubChk WHERE
      bMNPSubChk.CLI = icCLI NO-LOCK,
      EACH bMNPProcessChk WHERE
           bMNPProcessChk.MNPSeq = bMNPSubChk.MNPSeq AND
           bMNPProcessChk.MNPType = {&MNP_TYPE_OUT} AND
           LOOKUP(STRING(bMNPProcessChk.StatusCode),"2,5") > 0 NO-LOCK:
      RETURN True.
   END.
   RETURN False.

END FUNCTION.

FUNCTION fGetMNPOutOngoing RETURNS CHAR
(icCLI AS CHAR):

   DEF BUFFER bMNPSubChk FOR MNPSub.
   DEF BUFFER bMNPProcessChk FOR MNPProcess.

   FOR EACH bMNPSubChk WHERE
      bMNPSubChk.CLI = icCLI NO-LOCK,
      EACH bMNPProcessChk WHERE
           bMNPProcessChk.MNPSeq = bMNPSubChk.MNPSeq AND
           bMNPProcessChk.MNPType = {&MNP_TYPE_OUT} AND
           LOOKUP(STRING(bMNPProcessChk.StatusCode),"2,5") > 0 NO-LOCK:
      RETURN bMNPProcessChk.portrequest.
   END.
   RETURN "".

END FUNCTION.

&ENDIF
