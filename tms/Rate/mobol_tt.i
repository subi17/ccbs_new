/* mobol_tt.i 

   changes:     29.04.05/aam put screen only if not batch run
*/

&IF "{&MOBOL_TT_I}" NE "YES"
&THEN

&GLOBAL-DEFINE MOBOL_TT_I YES

{Syst/commali.i}

DEFINE TEMP-TABLE ttTariff NO-UNDO LIKE Tariff.
DEFINE TEMP-TABLE ttBDest  NO-UNDO LIKE Bdest.
DEFINE TEMP-TABLE ttBNet   NO-UNDO LIKE bnet.
DEFINE TEMP-TABLE ttRatePref NO-UNDO LIKE RatePref.
DEFINE TEMP-TABLE ttPListConf NO-UNDO LIKE PListConf.
DEFINE TEMP-TABLE ttPriceList NO-UNDO LIKE PriceList.
DEFINE TEMP-TABLE ttTCC NO-UNDO LIKE TCC
    INDEX TCC TCC ValidTo.
DEFINE TEMP-TABLE ttCDRError  NO-UNDO LIKE CDRError.
DEFINE TEMP-TABLE ttServiceLimit NO-UNDO LIKE ServiceLimit.
DEFINE TEMP-TABLE ttIPRange NO-UNDO LIKE IPRange.
DEFINE TEMP-TABLE ttCliType NO-UNDO LIKE CliType.
DEFINE TEMP-TABLE ttMatrix  NO-UNDO LIKE Matrix.
DEFINE TEMP-TABLE ttMxItem  NO-UNDO LIKE MxItem.

FUNCTION fFillTariff RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttTariff.

   FOR EACH Tariff NO-LOCK WHERE
            Tariff.Brand = gcBrand:

      CREATE ttTariff.
      BUFFER-COPY Tariff TO ttTariff.
      
   END.


END.

FUNCTION fFillBDest RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttBDest.
   
   FOR EACH Bdest NO-LOCK WHERE 
            Bdest.Brand = gcBrand:
      
      CREATE ttBDest.
      BUFFER-COPY BDest TO ttBDest.
                  
   END.
                     
END.

FUNCTION fFillBNet RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttBNet.
   
   FOR EACH bnet NO-LOCK WHERE
            bNet.Brand = gcBrand:
      
      CREATE ttBNet.
      BUFFER-COPY bnet TO ttBNet.
                  
   END.
   
END.

FUNCTION fFillRatePref RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttRatePref.
   
   FOR EACH RatePref NO-LOCK WHERE 
            RatePref.Brand = gcBrand:
            
      CREATE ttRatePref.
      BUFFER-COPY RatePref TO ttRatePref.
   END.
   
END FUNCTION.

FUNCTION fFillPListConf RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPListConf.
   
   FOR EACH PListConf NO-LOCK WHERE 
            PListConf.Brand = gcBrand:
            
      CREATE ttPListConf.
      BUFFER-COPY PListConf TO ttPListConf.
   END.
   
END FUNCTION.

FUNCTION fFillPriceList RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPriceList.
   
   FOR EACH PriceList NO-LOCK WHERE 
            PriceList.Brand = gcBrand:
            
      CREATE ttPriceList.
      BUFFER-COPY PriceList TO ttPriceList.
   END.
   
END FUNCTION.

FUNCTION fFillTCC RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttTCC.
   
   FOR EACH TCC NO-LOCK WHERE 
            TCC.Brand = gcBrand:
      CREATE ttTCC.
      BUFFER-COPY TCC TO ttTCC.
   END.
   
END FUNCTION.

FUNCTION fFillCDRError RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttCDRError.
   
   FOR EACH CDRError NO-LOCK:
      CREATE ttCDRError.
      BUFFER-COPY CDRError TO ttCDRError.
   END.
   
END FUNCTION.

FUNCTION fFillServiceLimit RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttServiceLimit.
   
   FOR EACH ServiceLimit NO-LOCK:
      CREATE ttServiceLimit.
      BUFFER-COPY ServiceLimit TO ttServiceLimit.
   END.
   
END FUNCTION.

FUNCTION fFillIPRange RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttIPRange.
   
   FOR EACH IPRange NO-LOCK:
      CREATE ttIPRange.
      BUFFER-COPY IPRange TO ttIPRange.
   END.
   
END FUNCTION.

FUNCTION fFillCliType RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttCliType.
   
   FOR EACH CliType NO-LOCK:
      CREATE ttCliType.
      BUFFER-COPY CliType TO ttCliType.
   END.
   
END FUNCTION.

FUNCTION fFillMatrix RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttMatrix.
   
   FOR EACH Matrix NO-LOCK:
      CREATE ttMatrix.
      BUFFER-COPY Matrix TO ttMatrix.
   END.
   
END FUNCTION.

FUNCTION fFillMxItem RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttMxItem.
   
   FOR EACH MxItem NO-LOCK:
      CREATE ttMxItem.
      BUFFER-COPY MxItem TO ttMxItem.
   END.
   
END FUNCTION.

/* TEMP-TABLES filled */
FUNCTION fFillTT RETURNS LOGICAL:

   fFillTariff().
   fFillBDest().
   fFillBNet().
   fFillRatePref().
   fFillPListConf().
   fFillPriceList().
   fFillTCC().
   fFillCDRError().
   fFillServiceLimit().
   fFillIPRange().
   fFillCliType().
   fFillMatrix().
   fFillMxItem().

END.

&ENDIF
