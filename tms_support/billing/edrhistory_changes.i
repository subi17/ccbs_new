/* edrhistory_changes.i      03.07.12/aam
*/

{commali.i}
{callquery.i}

DEF TEMP-TABLE ttHistory NO-UNDO
    LIKE EDRHistory
    FIELD Rated AS CHAR.
    
DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
    FIELD CDRTable AS CHAR.
    

PROCEDURE pInitHistory:

   DEF INPUT  PARAMETER icCLI     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaDateSt AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiTimeSt  AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiDtlSeq  AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER ocGSMBnr  AS CHAR NO-UNDO.

   DEF VAR tthCDR AS HANDLE NO-UNDO.
   DEF VAR liErrorCodeOut AS INT  NO-UNDO. 
   
   DEF BUFFER bHistory FOR EDRHistory.

   EMPTY TEMP-TABLE ttHistory.

   FOR FIRST MobCDR NO-LOCK WHERE
             MobCDR.CLI = icCLI AND
             MobCDR.DateSt = idaDateSt AND
             MobCDR.TimeSt = iiTimeSt AND
             MobCDR.DtlSeq = iiDtlSeq:
      CREATE ttHistory.
      BUFFER-COPY MobCDR TO ttHistory.
      ASSIGN 
         ttHistory.Brand = gcBrand
         ttHistory.Rated = "Current"
         ttHistory.UpdateSource = "Current"
         ocGSMBnr = MobCDR.GSMBnr.
   END.

   /* check from old dbs if not in the latest */
   IF NOT CAN-FIND(FIRST ttHistory) THEN DO:
      tthCDR = TEMP-TABLE ttCall:HANDLE.
      EMPTY TEMP-TABLE ttCall.
     
      fMobCDRCollect(INPUT "post",
                     INPUT gcBrand,
                     INPUT katun,
                     INPUT idaDateSt,
                     INPUT idaDateSt,
                     INPUT 0,
                     INPUT "inv",
                     INPUT icCLI,
                     INPUT 0,
                     INPUT 0,
                     INPUT "",
                     INPUT "",
                     INPUT "",
                     INPUT 0,
                     INPUT-OUTPUT liErrorCodeOut,
                     INPUT-OUTPUT tthCDR).
      
      FOR FIRST ttCall WHERE
                ttCall.CLI = icCLI AND
                ttCall.DateSt = idaDateSt AND
                ttCall.TimeSt = iiTimeSt AND
                ttCall.DtlSeq = iiDtlSeq:
         CREATE ttHistory.
         BUFFER-COPY ttCall TO ttHistory.
         ASSIGN 
            ttHistory.Rated = "Current"
            ttHistory.UpdateSource = "Current"
            ocGSMBnr = ttCall.GSMBnr.
      END.

      EMPTY TEMP-TABLE ttCall.
      DELETE OBJECT tthCDR NO-ERROR.
   END.
   
   FOR EACH bHistory NO-LOCK WHERE
            bHistory.Brand  = gcBrand AND
            bHistory.CLI    = icCLI AND
            bHistory.DateSt = idaDateSt AND
            bHistory.TimeSt = iiTimeSt AND
            bHistory.DtlSeq = iiDtlSeq
   BY bHistory.UpdateDate DESC
   BY bHistory.UpdateTime DESC:
      CREATE ttHistory.
      BUFFER-COPY bHistory TO ttHistory.
      ttHistory.Rated = STRING(bHistory.UpdateDate,"99-99-99").
   END.
   
END PROCEDURE.

