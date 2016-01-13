/* ----------------------------------------------------------------------
  module .......: Func/aes_decryption.i
  task .........: Make AES decrypting
  application ..: tms
  author .......: kaaikas
  created ......: 17.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&fDECRYPT}" NE "YES"
&THEN

&GLOBAL-DEFINE fDECRYPT YES


FUNCTION decryptData RETURNS CHAR (
   INPUT iprBinaryKey      AS RAW,
   INPUT iprEncryptedValue AS RAW,
   INPUT iprIV             AS RAW,
   INPUT ialgo             AS CHAR):

DEFINE VARIABLE        cDecryptedText    AS CHARACTER NO-UNDO.

ASSIGN
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = ialgo
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = iprBinaryKey
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = iprIV
    cDecryptedText = GET-STRING(DECRYPT (iprEncryptedValue),1).

RETURN cDecryptedText.

END FUNCTION.
&ENDIF
