/*YPR-4470 */
DEFINE TEMP-TABLE ttHdrtext NO-UNDO LIKE hdrtext.
DEF BUFFER newhdrtext FOR hdrtext.

FUNCTION fChangeHdr1 RETURNS CHAR
   (icIn AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.


   lcRet = REPLACE(icIn, "UNOE BANK, S.A.",
                      "Banco Cetelem, S.A.U.").
   lcRet = REPLACE(lcRet, "A-08024796",
                       "A-78650348").
   lcRet = REPLACE(lcRet, "C/ Julián Camarillo 4, 28037 - Madrid",
                       "Madrid, c/ Retama, 3 (28045)").


   return lcRet.
END.

FUNCTION fChangeHdr2 RETURNS CHAR
   (icIn AS CHAR):
   DEF VAR lcRet AS CHAR NO-UNDO.


   lcRet = REPLACE(icIn, "UNOE",
                      "Banco Cetelem").

   return lcRet.
END.



FOR EACH  hdrtext where hdrtext.te-nro EQ 536:
   CREATE ttHdrtext.
   BUFFER-COPY hdrtext TO ttHdrtext.
   ttHdrText.te-text = fChangeHdr1(ttHdrText.te-text).
   ttHdrText.te-nro = 575.

   CREATE newhdrtext.
   BUFFER-COPY ttHdrText TO newhdrtext.


END.

FOR EACH  hdrtext where hdrtext.te-nro EQ 557:
   CREATE ttHdrtext.
   BUFFER-COPY hdrtext TO ttHdrtext.
   ttHdrText.te-text = fChangeHdr2(ttHdrText.te-text).
   ttHdrText.te-nro = 576.

   CREATE newhdrtext.
   BUFFER-COPY ttHdrText TO newhdrtext.


END.


