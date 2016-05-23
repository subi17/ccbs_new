/* Testing tool for generating universal SIM ICCs to file for 
   adding new SIMs to stock. 
   Get some SIM files from production (/store/simcards/processed
   grep -l 89340411 * )
   copy this file to your folder where you are going to execute this
   generator and rename as 2000_usim.in

   Copy output file 2000_usim.out to /store/simcards/incoming_manual
   load SIMs to stock by CUI F4 - F6 - F1
   */

DEF STREAM sread.
DEF STREAM sout.
DEF VAR lcLine AS CHAR NO-UNDO.

input stream sread from "2000_usim.in".
output stream sout to "2000_usim.out.".


FUNCTION fCalculateLuhn RETURNS INTEGER (INPUT lcnumber AS CHAR):
   DEF VAR iinumbers AS INT EXTENT 18 NO-UNDO.
   DEF VAR liIndex AS INT NO-UNDO INIT 1.
   DEF VAR lcTempNumber AS CHAR NO-UNDO.
   DEF VAR liCheckSum AS INT NO-UNDO.

   DO liIndex = 1 TO 18:
      IF liIndex MODULO 2 EQ 0 THEN DO:
         iiNumbers[liIndex] =  INT(SUBSTRING(lcnumber,liIndex,1)) * 2.
         IF iiNumbers[liIndex] GE 10 THEN DO:
            lcTempNumber = STRING(iiNumbers[liIndex]).
            iiNumbers[liIndex] = INT(SUBSTRING(lcTempNumber,1,1)) +
                                 INT(SUBSTRING(lcTempNumber,2,1)).
         END.
      END.
      ELSE
         iiNumbers[liIndex] = INT(SUBSTRING(lcnumber,liIndex,1)).
   END.

   DO liIndex = 1 TO 18:
      liCheckSum = liCheckSum + iiNumbers[liIndex].
   END.
  
   liChecksum = liChecksum * 9.

   lcTempNumber = STRING(liCheckSum).

   liCheckSum = INT(SUBSTRING(lcTempNumber,LENGTH(lcTempNumber),1)).

   RETURN liCheckSum. 
   
END.

FUNCTION fmodifyUSimICC RETURNS CHAR (INPUT icNumber AS CHAR):
   DEF VAR lcModifiedNumber AS CHAR NO-UNDO.
   lcModifiedNumber = REPLACE(icNumber,"89340441","89340443").
   IF LENGTH(lcModifiedNumber) > 18 THEN DO:
      lcModifiedNumber = SUBSTRING(lcModifiedNumber,1,
                                   LENGTH(lcModifiedNumber) - 1).
      lcModifiedNumber = lcModifiedNumber + 
                         STRING(fCalculateLuhn(lcModifiedNumber)).
   END.
   RETURN lcModifiedNumber.
END.

DEF VAR lcNumber AS CHAR NO-UNDO.
repeat:
   lcNumber = "".
   import stream sread unformatted lcline.

   if lcLine begins "21404" OR lcLine begins "Ser_" then 
      assign
         lcNumber = (entry(2,lcline," ")).

   IF lcNumber > "" THEN
      PUT STREAM sOut UNFORMATTED
         REPLACE(lcline, lcNumber, fmodifyUSimICC(lcNumber)) SKIP.
   ELSE
      PUT STREAM sOut UNFORMATTED
         lcLine SKIP.
END.
