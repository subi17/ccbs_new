/* YCO-457-3Gb-Upsell-legacy-creation.p 
   Purpose: add legacy tariffs to the list of compatible tariffs 
*/
DEF VAR liUpsells     AS INTEGER NO-UNDO.
DEF VAR liSLSeq       AS INTEGER NO-UNDO.
DEF VAR liMXSeq       AS INTEGER NO-UNDO.
DEF VAR liPackageID   AS INTEGER NO-UNDO.
DEF VAR liComponentID AS INTEGER NO-UNDO.
DEF VAR liBDLValue    AS INTEGER NO-UNDO.
DEF VAR liPrior       AS INTEGER NO-UNDO.
DEF VAR lSuccess      AS LOGICAL NO-UNDO.
DEF VAR dfrom         AS DATE FORMAT "99/99/99" NO-UNDO.
DEF VAR dto           AS DATE FORMAT "99/99/99" NO-UNDO.
DEF VAR iMx           AS INTEGER NO-UNDO.

DEF BUFFER b_MXItem FOR MXItem.

/* List of valid tariffs for this upsell */
DEF VAR cValidList AS CHAR INITIAL
   "CONT6,CONT7,CONT8,CONT9,CONTF11,CONTF20D,CONTF30,CONTF40,CONTF55,CONTF8,CONTM,CONTM2,CONT23,CONT24,CONTS12,CONTS15,CONTS16,CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONT28,CONT27,CONT31,CONTRD1,CONTRD2,CONTRD3,CONTRD4,CONTRD9".

/* List of upsells where to add the tariffs*/
DEF VAR cUpsell_Id      AS CHARACTER NO-UNDO INITIAL 
    "FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL,FID3GB_12m_R_UPSELL".

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".

FUNCTION fGetNextMXSeq RETURNS INTEGER ():
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
     RETURN bf_Matrix.MXSeq + 1.
   END.

   RETURN 1.
END FUNCTION.

FUNCTION fGetNextMatrixPriority RETURNS INTEGER (icKey AS CHARACTER):
   DEFINE BUFFER bf_Matrix FOR Matrix.

   FIND LAST bf_Matrix WHERE bf_Matrix.mxkey = icKey NO-LOCK NO-ERROR.
   IF AVAILABLE bf_Matrix THEN 
      RETURN (bf_Matrix.Prior + 1).
   ELSE 
      RETURN 1.  
END FUNCTION.


ASSIGN
   dfrom = TODAY
   dto   = DATE(12,31,2049).

FORM
  SKIP "This program will add legacy tariffs to upsells 3Gb for YCO-276" SKIP(1)
  "with codes FID3GB_R_UPSELL,FID3GB_3m_R_UPSELL,FID3GB_6m_R_UPSELL " SKIP 
  "and FID3GB_12m_R_UPSELL" SKIP
  WITH OVERLAY CENTERED ROW 6 TITLE " Add legacy Upsells - YCO-457 " NO-LABELS
  FRAME f-yco457.


MESSAGE "Are you sure you want to proceed with the process?" VIEW-AS ALERT-BOX QUESTION
   BUTTONS YES-NO UPDATE lGo AS LOGICAL.
   
IF lGo = FALSE THEN
DO:
   MESSAGE "Process cancelled by user" VIEW-AS ALERT-BOX INFORMATION.
   RETURN.
END.
     
blk-upsell:
DO TRANSACTION ON ERROR UNDO blk-upsell, LEAVE blk-upsell
               ON STOP  UNDO blk-upsell, LEAVE blk-upsell:
                  
  lsuccess = FALSE.

  DO liUpsells = 1 TO 4:

     /* Test and finding records */   
     FIND FIRST Matrix NO-LOCK 
          WHERE Matrix.brand  = Syst.Var:gcBrand                         
            AND Matrix.MXName = ENTRY(liUpsells,cUpsell_Id) + " Per.contract usage" 
            AND Matrix.MXKey  = "PERCONTR" NO-ERROR.
     IF NOT AVAIL Matrix THEN
     DO:
        MESSAGE "Error finding Matrix for:" ENTRY(liUpsells,cUpsell_Id) SKIP 
                "Aborting"
           VIEW-AS ALERT-BOX. 
        UNDO blk-upsell, LEAVE blk-upsell.
     END.               
   
     FIND FIRST MXItem NO-LOCK 
          WHERE MXItem.MXSeq   = Matrix.MXSeq
            AND MXItem.MXName  = "PerContract"
            AND MXItem.MXValue = ENTRY(liUpsells,cUpsell_Id) NO-ERROR.
     IF NOT AVAIL MXItem THEN
     DO:
        MESSAGE "Error finding MXItem for:" ENTRY(liUpsells,cUpsell_Id) SKIP 
                "Aborting"
           VIEW-AS ALERT-BOX. 
        UNDO blk-upsell, LEAVE blk-upsell.
     END.               
 
     /* Check if the program has already being executed */
     FIND FIRST b_MXItem NO-LOCK 
          WHERE b_MXItem.MXSeq  = MXItem.MXSeq
            AND b_MXItem.MXName = "SubsTypeTo"
            AND b_MXItem.MXValue = ENTRY(1,cValidList) NO-ERROR. 
     IF AVAIL b_MXItem THEN
     DO:
        MESSAGE "This YCO-457 configuration program has already being executed" SKIP 
                "Aborting"
           VIEW-AS ALERT-BOX. 
        UNDO blk-upsell, LEAVE blk-upsell.
     END.
 
     /* Creating new configuration */ 
     DO iMx = 1 TO NUM-ENTRIES(cValidList):
        CREATE b_MXItem.
        ASSIGN 
           b_MXItem.MXSeq   = MXItem.MXSeq            /* Matrix Sequence */
           b_MXItem.MXName  = "SubsTypeTo"            /* Name MXItem     */
           b_MXItem.MXValue = ENTRY(iMx,cValidList).  /* Matrix Value    */
     END.
  END.  /* DO liUpsells */
  
  /* Process OK */
  lSuccess = TRUE.
END.

IF lSuccess THEN
    MESSAGE "Upsells successfully created" VIEW-AS ALERT-BOX.
ELSE 
    MESSAGE "ERROR: Failed to create the upsells" VIEW-AS ALERT-BOX.

