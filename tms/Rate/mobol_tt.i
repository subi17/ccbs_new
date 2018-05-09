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
DEFINE TEMP-TABLE ttSLGAnalyse NO-UNDO LIKE SLGAnalyse.

FUNCTION fFillTariff RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttTariff.

   FOR EACH Tariff NO-LOCK WHERE
            Tariff.Brand = Syst.Var:gcBrand:

      CREATE ttTariff.
      BUFFER-COPY Tariff TO ttTariff.
      
   END.


END.

FUNCTION fFillBDest RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttBDest.
   
   FOR EACH Bdest NO-LOCK WHERE 
            Bdest.Brand = Syst.Var:gcBrand:
      
      CREATE ttBDest.
      BUFFER-COPY BDest TO ttBDest.
                  
   END.
                     
END.

FUNCTION fFillBNet RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttBNet.
   
   FOR EACH bnet NO-LOCK WHERE
            bNet.Brand = Syst.Var:gcBrand:
      
      CREATE ttBNet.
      BUFFER-COPY bnet TO ttBNet.
                  
   END.
   
END.

FUNCTION fFillRatePref RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttRatePref.
   
   FOR EACH RatePref NO-LOCK WHERE 
            RatePref.Brand = Syst.Var:gcBrand:
            
      CREATE ttRatePref.
      BUFFER-COPY RatePref TO ttRatePref.
   END.
   
END FUNCTION.

FUNCTION fFillPListConf RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPListConf.
   
   FOR EACH PListConf NO-LOCK WHERE 
            PListConf.Brand = Syst.Var:gcBrand:
            
      CREATE ttPListConf.
      BUFFER-COPY PListConf TO ttPListConf.
   END.
   
END FUNCTION.

FUNCTION fFillPriceList RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttPriceList.
   
   FOR EACH PriceList NO-LOCK WHERE 
            PriceList.Brand = Syst.Var:gcBrand:
            
      CREATE ttPriceList.
      BUFFER-COPY PriceList TO ttPriceList.
   END.
   
END FUNCTION.

FUNCTION fFillTCC RETURNS LOGICAL:

   EMPTY TEMP-TABLE ttTCC.
   
   FOR EACH TCC NO-LOCK WHERE 
            TCC.Brand = Syst.Var:gcBrand:
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

FUNCTION fFillSLGAnalyse RETURNS LOGICAL:
   
   DEFINE VARIABLE liKnt                         AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liCount                       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcMxValue                     AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcCliType                     AS CHARACTER NO-UNDO INIT "CONT10".
   DEFINE VARIABLE lcSubsTypePrefix              AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcAllowedBundles              AS CHARACTER NO-UNDO.

   DEFINE BUFFER bf_ttMxItem FOR ttMxItem.

   EMPTY TEMP-TABLE ttSLGAnalyse.

   FOR EACH ttCliType:

      lcSubsTypePrefix = (IF lcCliType BEGINS "CONTDSL" THEN 
                              "CONTDSL*,CONT*" 
                          ELSE IF lcCliType BEGINS "CONTFH" THEN 
                              "CONT*"     
                          ELSE IF lcCliType BEGINS "CONT" THEN 
                              "CONT*" 
                          ELSE IF lcCliType BEGINS "TARJ" THEN 
                              "TARJ*" 
                          ELSE "").

      ASSIGN lcSubsTypePrefix = lcSubsTypePrefix + (IF lcSubsTypePrefix <> "" THEN "," ELSE "") + ttCliType.CliType.

      IF lcSubsTypePrefix > "" THEN
      DO liCount = 1 TO NUM-ENTRIES(lcSubsTypePrefix):
          FOR EACH ttMatrix WHERE ttMatrix.Brand = Syst.Var:gcBrand AND ttMatrix.MXKey = "PERCONTR" NO-LOCK By ttMatrix.Prior:
              
              IF ttMatrix.MXRes <> 1 THEN 
                  NEXT.                                           
              
              ASSIGN lcMxValue = ENTRY(liCount,lcSubsTypePrefix).
              
              FOR EACH ttMxItem WHERE ttMxItem.MxSeq = ttMatrix.MxSeq AND ttMxItem.MxName = "SubsTypeTo" AND ttMxItem.MxValue = lcMxValue NO-LOCK:

                  FOR EACH bf_ttMxItem WHERE bf_ttMxItem.MxSeq = ttMxItem.MxSeq AND bf_ttMxItem.MXName = "PerContract":
                      IF LOOKUP(bf_ttMxItem.MxValue, lcAllowedBundles) = 0 THEN                 
                         ASSIGN lcAllowedBundles = lcAllowedBundles + (IF lcAllowedBundles <> "" THEN "," ELSE "") + bf_ttMxItem.MxValue.
                  END.

              END.

          END.
      END.

      DO liKnt = 1 TO NUM-ENTRIES(lcAllowedBundles):
          FOR EACH  SLGAnalyse NO-LOCK WHERE 
                    SLGAnalyse.Brand             = Syst.Var:gcBrand               AND 
                    SLGAnalyse.ServiceLimitGroup = ENTRY(liKnt, lcAllowedBundles) AND 
                    SLGAnalyse.CliType           = "*"                            AND
                    SLGAnalyse.ValidTo          >= ?                              USE-INDEX ServiceLimitGroup:
             CREATE ttSLGAnalyse.
             BUFFER-COPY SLGAnalyse EXCEPT CliType TO ttSLGAnalyse
                ASSIGN ttSLGAnalyse.CliType = ttCliType.CliType.        
          END.          
      END.

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
   fFillSLGAnalyse().

END.

&ENDIF
