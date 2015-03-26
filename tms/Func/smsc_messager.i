/* ----------------------------------------------------------------------------
  MODULE .......: SMSC_MESSAGER.I
  FUNCTION .....: SMSC Functions
  APPLICATION ..: TMS
  CREATED ......: 04.04.04 kl
  CHANGED ......: 15.09.04 kl fGetParam: return after first match

  Version ......: SHARK
  --------------------------------------------------------------------------- */

FUNCTION fSendFormat RETURNS CHARACTER
  (INPUT pcMessage AS CHARACTER):
  
   DEF VAR liLoop AS INTEGER NO-UNDO.
   
   liLoop = INDEX(pcMessage,"<SMSG>").
   DO WHILE liLoop > 0:
      SUBSTR(pcMessage,liLoop,6) = "op:msg".
      liLoop = INDEX(pcMessage,"<SMSG>").
   END.
   
   liLoop = INDEX(pcMessage,"<SOK>").
   DO WHILE liLoop > 0:
      SUBSTR(pcMessage,liLoop,5) = "op:ok".
      liLoop = INDEX(pcMessage,"<SOK>").
   END.

   liLoop = INDEX(pcMessage,"<LF>").
   DO WHILE liLoop > 0:
      SUBSTR(pcMessage,liLoop,4) = CHR(10).
      liLoop = INDEX(pcMessage,"<LF>").
   END.
   
   liLoop = INDEX(pcMessage,"<EMSG>").
   DO WHILE liLoop > 0:
      SUBSTR(pcMessage,liLoop,6) = "end:msg".
      liLoop = INDEX(pcMessage,"<EMSG>").
   END.

   liLoop = INDEX(pcMessage,"<EOK>").
   DO WHILE liLoop > 0:
      SUBSTR(pcMessage,liLoop,5) = "end:ok".
      liLoop = INDEX(pcMessage,"<EOK>").
   END.

   RETURN pcMessage.

END.

FUNCTION fGetParam RETURNS CHARACTER
  (INPUT pcMessage AS CHARACTER,
   INPUT pcParam   AS CHARACTER):

   DEFINE VARIABLE iLoop   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
   
   DO iLoop = 1 TO NUM-ENTRIES(pcMessage,CHR(10)):
      
      IF pcParam NE "" THEN DO:
         IF ENTRY(1,ENTRY(iLoop,pcMessage,CHR(10)),":") = pcParam THEN DO:
            cReturn = ENTRY(2,ENTRY(iLoop,pcMessage,CHR(10)),":").
            iLoop = NUM-ENTRIES(pcMessage,CHR(10)) + 1.
         END.
      END.
      ELSE
         cReturn = cReturn + ":" + 
                   ENTRY(1,ENTRY(iLoop,pcMessage,CHR(10)),":").
   END.
   IF cReturn BEGINS ":" THEN cReturn = SUBSTR(cReturn,2).
   
   RETURN cReturn.
   
END.

/* 
   Next functions are for encoding alphanum sms message sender field (oAdC)
   (see. https://luna.starnet.fi/display/XFERA/SMSC+specifications)
   (emi_ucp_specification_v46-5232006.pdf page 17)
*/

DEF VAR DEFAULT_ALPHABET AS CHAR CASE-SENSITIVE NO-UNDO.

/* The alphabet is written here in latin-1. Thus the source code
 * MUST be read in with latin-1 (cpinternal AND cpstream).
 * Non latin-1 characters are substituted with a @ sign (which is the first
 * and thus by lookup always found first).
 * Note that Progress still seems to have problems with some chars ALTHOUGH
 * they are in latin-1 */
DEFAULT_ALPHABET = "@~243$~245~350~351~371~354~362~307~n~330~370~r~305~345" +
                   "@_@@@@@@@@@@~306~346~337~311" +
                   " !""#@%&'()*+,-./" +
                   "0123456789:;<=>?" +
                   "~241ABCDEFGHIJKLMNO" +
                   "PQRSTUVWXYZA~326~321~334§" +
                   "abcdefghijklmno" +
                   /*"~277abcdefghijklmno" +*/
                   "pqrstuvwxyz~344~366~361~374~340".

FUNCTION _translate_default_alphabet RETURNS LOGICAL
        ( str AS CHAR,
          OUTPUT result AS INT EXTENT ):
    DEF VAR lii AS INT NO-UNDO.
    EXTENT(result) = LENGTH(str).
    DO lii = 1 TO LENGTH(str):
        result[lii] = INDEX(DEFAULT_ALPHABET, SUBSTRING(str, lii, 1)) - 1.
    END.
    RETURN TRUE.
END FUNCTION.

FUNCTION _tohex RETURNS CHAR ( inp AS INT ):
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR rawMeat AS RAW NO-UNDO.
    LENGTH(rawMeat) = 1.
    PUT-BYTE(rawMeat, 1) = inp.
    result = HEX-ENCODE(rawMeat).
    RETURN result.
END FUNCTION.

FUNCTION _pack_and_tohex RETURNS CHAR ( inpnums AS INT EXTENT ):
    DEF VAR result AS CHAR NO-UNDO.
    DEF VAR outByte AS INT NO-UNDO.
    DEF VAR leftInOut AS INT NO-UNDO.
    DEF VAR inByte AS INT NO-UNDO.
    DEF VAR lii AS INT NO-UNDO.
    DEF VAR transfer AS INT NO-UNDO.
    DEF VAR significant_nibbles AS INT NO-UNDO.
    result = "".
    outByte = 0.
    leftInOut = 8.
    DO lii = 1 TO EXTENT(inpnums):
        inByte = inpnums[lii].
        transfer = MIN(7, leftInOut).
        PUT-BITS(outByte, 9 - leftInOut, transfer) = inByte.
        leftInOut = leftInOut - transfer.
        IF leftInOut EQ 0 THEN DO:
            result = result + _tohex(outByte).
            IF transfer LT 7 THEN
                outByte = GET-BITS(inByte, transfer + 1, 7 - transfer).
            ELSE
                outByte = 0.
            leftInOut = 8 - (7 - transfer).
        END.
    END.
    IF leftInOut LT 8 THEN
        result = result + _tohex(outByte).
    significant_nibbles = LENGTH(result).
    IF leftInOut GE 4 THEN significant_nibbles = significant_nibbles - 1.
    RETURN _tohex(significant_nibbles) + result.
END FUNCTION.

FUNCTION encodeOAdC RETURNS CHAR ( str_in_latin AS CHAR ):
    DEF VAR sevenbitenc AS INTEGER EXTENT NO-UNDO.
    _translate_default_alphabet(SUBSTR(str_in_latin,1,11), OUTPUT sevenbitenc).
    RETURN UPPER(_pack_and_tohex(sevenbitenc)).
END FUNCTION.

FUNCTION fIsAlphaNum RETURNS LOGICAL
(icParam AS CHAR):
   
   DEF VAR i AS INTEGER NO-UNDO. 

   do i = 1 to length(icParam):
      if index("0123456789",SUBSTR(icParam,i,1)) = 0 then return TRUE.
   end.

   RETURN FALSE.

END FUNCTION. 
