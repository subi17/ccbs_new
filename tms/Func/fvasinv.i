/* fvasinv.i        01.07.03/aam
   
   30.03.04 kl  fixes after index changes

   logic for invoicing VASCDRs 
*/

{error_codes.i}

DEF TEMP-TABLE ttVASRow NO-UNDO
   FIELD BillItem AS CHAR
   FIELD ServAddr AS CHAR
   FIELD ServName AS CHAR
   FIELD FromDate AS DATE
   FIELD ToDate   AS DATE
   FIELD VasQty   AS INT
   FIELD ConAmt   AS DEC
   FIELD OperAmt  AS DEC
   FIELD InvAmt   AS DEC
   INDEX BillItem BillItem.

   
FUNCTION fVasInvRow RETURNS LOGICAL
   (iiInvSeq    AS INT,
    iiCustNum   AS INT,
    idtFromDate AS DATE,
    idtToDate   AS DATE,
    idtPerDate  AS DATE,
    OUTPUT odtFirstDate AS DATE).

   DEF VAR llOldCdr   AS LOG  NO-UNDO.
   DEF VAR lcServAddr AS CHAR NO-UNDO.
   DEF VAR lcServName AS CHAR NO-UNDO. 
   DEF VAR llInclvat  AS LOG  NO-UNDO.
   
   FIND FIRST VASOper WHERE 
              VASOper.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VASOper THEN RETURN FALSE.               
   
   EMPTY TEMP-TABLE ttVASRow.

   odtFirstDate = ?.
   
   FOR EACH VASCDR NO-LOCK USE-INDEX invseq WHERE
            VASCDR.InvCust     = iiCustNum AND
            VASCDR.InvSeq      = iiInvSeq  AND
            VASCDR.ServiceName > ""        AND 
            VASCDR.BillCode    > "": 
            
      IF idtPerDate NE ? AND VASCDR.DateSt > idtPerDate THEN NEXT.
      
      IF odtFirstDate = ? THEN odtFirstDate = VASCDR.datest.
      ELSE odtFirstDate = MIN(odtFirstDate,VASCDR.datest).
                  
      llOldCdr = (MONTH(VASCDR.datest) NE MONTH(idtToDate)). 

      /* vat handling for consumer prices 
         always excluded 
      */
      
      ASSIGN lcServAddr = /* e.g. MT-tickets may not have the correct 
                            address */
                          IF VASCDR.ErrorCode = {&CDR_ERROR_UNKNOWN_VAS_NAME}
                          THEN "UT"
                          ELSE VASCDR.ServiceAddress
             lcServName = IF VASCDR.ErrorCode = {&CDR_ERROR_UNKNOWN_VAS_NAME}
                          THEN ""
                          ELSE VASCDR.ServiceName.

      FIND FIRST ttVASRow WHERE
                 ttVASRow.BillItem      = VASCDR.BillCode         AND
                 ttVASRow.ServAddr      = lcServAddr            AND
                 ttVASRow.ServName      = lcServName            AND 
                 MONTH(ttVASRow.ToDate) = MONTH(VASCDR.Datest)
                 NO-ERROR.

      IF NOT AVAIL ttVASRow THEN DO:
            
         CREATE ttVASRow.
         ASSIGN ttVASRow.BillItem = VASCDR.BillCode
                ttVASRow.ServAddr = lcServAddr
                ttVASRow.ServName = lcServName.

         /* CURRENT invoicing period */
         IF NOT llOldCdr THEN ASSIGN
            ttVASRow.FromDate = idtFromdate
            ttVASRow.ToDate   = idtToDate.
         /* older calls */   
         ELSE ASSIGN
            ttVASRow.FromDate = VASCDR.datest
            ttVASRow.ToDate   = VASCDR.datest.
      END.

      /* DATE of oldest AND newest call when NOT CURRENT period */
      IF llOldCdr THEN ASSIGN 
         ttVASRow.FromDate = MIN(ttVASRow.FromDate,VASCDR.datest)
         ttVASRow.ToDate   = MAX(ttVASRow.ToDate,VASCDR.datest). 

      ASSIGN
         /* MT-tickets may contain more than 1 SMS */
         ttVASRow.VASQty  = ttVASRow.VASQty + (IF VASCDR.TermSMS > 0
                                               THEN VASCDR.TermSMS
                                               ELSE 1)
         /* billing fee is calculated from consumer prices */
         ttVASRow.ConAmt  = ttVASRow.ConAmt  + VASCDR.Amount
         ttVASRow.OperAmt = ttVASRow.OperAmt + VASCDR.VASPrice.

   END. /* VASCDR  */

   /* calculate invoicable amount & invoicing fee */
   FOR EACH ttVASRow:

      /* billed (credited) amount is 
         consumer price - operator price - billing fee (%) 
         minimum fee is checked for each service (and per each month) 
      */
      
      ttVASRow.InvAmt = -1 * ROUND(ttVASRow.ConAmt - ttVASRow.OperAmt,2).
      
      /* invoicing fee for consumer billing */
      IF ttVASRow.ServAddr NE "UT" AND
         ttVASRow.ConAmt > 0 
      THEN ttVASRow.InvAmt = ttVASRow.InvAmt + 
                           MAX(ROUND(VASOper.InvFee * ttVASRow.ConAmt / 100,2),
                               VASOper.MinFee). 
   END.

   RETURN (CAN-FIND(FIRST ttVASRow)).

END FUNCTION. 
