/* ----------------------------------------------------------------------
  module .......: Func/aes_encryption.i
  task .........: Make AES encrypting
  application ..: tms
  author .......: kaaikas
  created ......: 17.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

&IF "{&fENCRYPT}" NE "YES"
&THEN

&GLOBAL-DEFINE fENCRYPT YES

FUNCTION encrypt_data RETURNS CHAR (
   INPUT icdata AS CHAR,  /* data to be encrypted */
   INPUT icAlgo AS CHAR,  /* used algorithm */
   INPUT icPw   AS CHAR): /* pass phrase for creating key */

DEFINE VARIABLE cClearText      AS CHARACTER NO-UNDO.
DEFINE VARIABLE rBinaryKey      AS RAW       NO-UNDO.
DEFINE VARIABLE rEncryptedValue AS RAW       NO-UNDO.
DEFINE VARIABLE cEncryptedText  AS CHARACTER NO-UNDO.
DEFINE VARIABLE rSalt           AS RAW       NO-UNDO.
DEFINE VARIABLE cSalt           AS CHAR      NO-UNDO.
DEFINE VARIABLE cbk             AS CHARACTER NO-UNDO.
DEF VAR cIV                     AS CHAR      NO-UNDO.
DEF VAR rIV                     AS RAW       NO-UNDO.
DEF VAR calgo                   AS CHAR      NO-UNDO.
DEFINE VARIABLE i AS INT64 NO-UNDO.

cIV = STRING(BASE64-ENCODE(GENERATE-UUID)).
rIV = BASE64-DECODE(cIV).

/* cAlgo = "AES_CFB_256".*/ /* encrypt algorithm */

ASSIGN
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-ALGORITHM = icAlgo
    /*rSalt = GENERATE-PBE-SALT*/
    rBinaryKey = GENERATE-PBE-KEY(icPw /*, rSalt*/)
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-KEY = rBinaryKey
    SECURITY-POLICY:SYMMETRIC-ENCRYPTION-IV = rIv 
    rEncryptedValue = Encrypt (icData)
    cEncryptedText = BASE64-ENCODE(rEncryptedValue)
    cbk = BASE64-ENCODE(rBinaryKey).
    /*cSalt = BASE64-ENCODE(rSalt).*/
    
RETURN cIV + cEncryptedText.
END FUNCTION.
&ENDIF

