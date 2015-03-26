/* creprows.p      calculate InvASub AND InvCCN from call events 

10.12.2001/aam ASSIGN InvSeq.InvNum from Invoice.InvNum
12.12.2001/aam FIND nnatno just WITH an-nro 
20.05.2002/aam INPUT PARAMETER invno, 
               MobCDR added 
21.03.2003/aam check currency unit and convert sums if necessary,
               use temp-table so that sums don't get rounded too early
12.09.2003/aam brand               
30.03.04 kl    fixes after index changes
12.03.05/aam   use MobCdr.InvCust, not CustNum
12.04.05/aam   use InvASub.GenPrice to store MPM,
               vat included/excluded check,
               DataAmt added,
               parameter InvCCNColl determines whether InvCCN
               is collected or not
04.08.05/aam   use MobCDR.MPMAmt for mpm
26.09.05/aam   MPMRid and ServRid to InvASub (ttCLI)
*/

{commali.i}
{timestamp.i}
{fcurrency.i}
{cparam2.i}
{callquery.i}

DEF INPUT PARAMETER iInvno      AS INT NO-UNDO.
DEF INPUT PARAMETER iiSubInvNum AS INT  NO-UNDO.

DEF TEMP-TABLE ttInvASub NO-UNDO LIKE InvASub
   FIELD Net     AS DEC
   FIELD Gross   AS DEC
   FIELD VatIncl AS LOG
   FIELD MsSeq   AS INT
   INDEX MsSeq MsSeq CCN BillCode TariffNum.

DEF TEMP-TABLE ttInvCCN NO-UNDO LIKE InvCCN 
   FIELD Net     AS DEC
   FIELD Gross   AS DEC
   FIELD VatIncl AS LOG
   FIELD MsSeq   AS INT 
   INDEX MsSeq MsSeq CCN BillCode TariffNum.

DEF BUFFER bttInvASub FOR ttInvASub.
DEF BUFFER bttInvCCN  FOR ttInvCCN.

DEF VAR lCLI      AS c   NO-UNDO.
DEF VAR i         AS i   NO-UNDO.
DEF VAR lD2D      AS DEC NO-UNDO.
DEF VAR ldNet     AS DEC NO-UNDO.
DEF VAR ldGross   AS DEC NO-UNDO. 
DEF VAR ldVatFact AS DEC NO-UNDO. 
DEF VAR liInvCCN  AS INT NO-UNDO.

liInvCCN = fCParamI("InvCCNColl").

FIND Invoice no-lock where
    Invoice.InvNum = iInvno no-error.
IF NOT AVAILABLE Invoice THEN RETURN. 

IF iiSubInvNum > 0 THEN DO:
   FIND FIRST SubInvoice WHERE 
              SubInvoice.InvNum = Invoice.InvNum AND
              SubInvoice.SubInvNum = iiSubInvNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SubInvoice THEN RETURN.

   IF SubInvoice.InvSeq = 0 THEN RETURN.
END.   

DEFINE VARIABLE tthCDR         AS HANDLE    NO-UNDO.
DEFINE VARIABLE liERrorCodeOut AS INT       NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable AS CHAR.

tthCDR = TEMP-TABLE ttCall:HANDLE.

FOR EACH SubInvoice OF Invoice NO-LOCK WHERE
         (IF iiSubInvNum > 0 THEN SubInvoice.SubInvNum = iiSubInvNum
          ELSE TRUE):

   FOR EACH InvASub EXCLUSIVE-LOCK WHERE
            InvASub.InvNum = Invoice.InvNum AND
            InvASub.SubInvNum = SubInvoice.SubInvNum:
      DELETE InvASub.
   END.

   FOR EACH InvCCN EXCLUSIVE-LOCK WHERE 
            InvCCN.InvNum = Invoice.InvNum AND
            InvCCN.SubInvNum = SubInvoice.SubInvNum :
      DELETE InvCCN.
   END.
   
   EMPTY TEMP-TABLE ttCall.

   fMobCDRCollect(INPUT "post",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT IF Invoice.FirstCall NE ?   
                        THEN Invoice.FirstCall
                        ELSE Invoice.FromDate,
                  INPUT Invoice.ToDate,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT SubInvoice.InvSeq,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liErrorCodeOut,
                  INPUT-OUTPUT tthCDR).

   FOR EACH ttCall:

      /* is currency unit full or sub (assign ldnet & ldgross) */
      fCurrUnit(ttCall.Amount,              
                ttCall.MPMAmt,  /* mpm */
                ttCall.CurrUnit,
                "",
                ttCall.TariffNum,
                Invoice.Brand,
                OUTPUT ldNet,
                OUTPUT ldGross).

      /* webspeed calculations: CCN */
      IF liInvCCN = 1 THEN DO:
         FIND FIRST ttInvCCN where
                    ttInvCCN.MsSeq     = ttCall.MsSeq     AND
                    ttInvCCN.CCN       = ttCall.CCN       AND
                    ttInvCCN.BillCode  = ttCall.BillCode  AND
                    ttInvCCN.TariffNum = ttCall.TariffNum AND
                    ttInvCCN.VatIncl   = ttCall.VatIncl   
         NO-ERROR.
      
         IF NOT AVAIL ttInvCCN THEN DO:
            CREATE ttInvCCN.
            ASSIGN 
            ttInvCCN.MsSeq     = ttCall.MsSeq
            ttInvCCN.InvSeq    = ttCall.InvSeq
            ttInvCCN.InvNum    = Invoice.InvNum
            ttInvCCN.SubInvNum = SubInvoice.SubInvNum
            ttInvCCN.CCN       = ttCall.CCN
            ttInvCCN.BillCode  = ttCall.BillCode
            ttInvCCN.TariffNum = ttCall.TariffNum
            ttInvCCN.VatIncl   = ttCall.VatIncl.
         END.

         ASSIGN
         ttInvCCN.Qty     = ttInvCCN.Qty   + 1
         ttInvCCN.Min     = ttInvCCN.Min   + ttCall.billdur
         ttInvCCN.Net     = ttInvCCN.Net   + ldNet
         ttInvCCN.Gross   = ttInvCCN.Gross + ldGross
         ttInvCCN.DataAmt = ttInvCCN.DataAmt + ttCall.DataIn + ttCall.DataOut.

      END.
      
      lCLI = ttCall.CLI.

      /* IF known ASUB OR BSUB */
      FIND FIRST ttInvASub where
                 ttInvASub.MsSeq     = ttCall.MsSeq     AND
                 ttInvASub.CCN       = ttCall.CCN       AND
                 ttInvASub.BillCode  = ttCall.BillCode  AND
                 ttInvASub.TariffNum = ttCall.TariffNum AND 
                 ttInvASub.VatIncl   = ttCall.VatIncl   AND
                 ttInvASub.ServRid   = ttCall.ServRid   AND
                 ttInvASub.MPMRid    = ttCall.MPMRid 
      no-error.

      IF NOT AVAIL ttInvASub THEN DO:
         CREATE ttInvASub.
         ASSIGN ttInvASub.InvNum    = Invoice.InvNum
                ttInvASub.SubInvNum = SubInvoice.SubInvNum
                ttInvASub.MsSeq     = ttCall.MsSeq 
                ttInvASub.InvSeq    = ttCall.InvSeq 
                ttInvASub.CCN       = ttCall.CCN     
                ttInvASub.BillCode  = ttCall.BillCode   
                ttInvASub.CLI       = lCLI
                ttInvASub.TariffNum = ttCall.TariffNum
                ttInvASub.VatIncl   = ttCall.VatIncl
                ttInvASub.ServRid   = ttCall.ServRid   
                ttInvASub.MPMRid    = ttCall.MPMRid. 
      END.

      ASSIGN ttInvASub.Qty     = ttInvASub.Qty  + 1
             ttInvASub.Min     = ttInvASub.Min  + ttCall.billdur
             ttInvASub.Net     = ttInvASub.Net  + ldNet
             ttInvASub.Gross   = ttInvASub.Gross + ldGross.
             ttInvASub.DataAmt = ttInvASub.DataAmt + 
                                 ttCall.DataIn + ttCall.DataOut.

   END.

END.

DO:
    
   /* unify vat handling */
   FOR EACH ttInvCCN WHERE
            ttInvCCN.VatIncl NE Invoice.VatIncl,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            InvRow.SubInvNum = ttInvCCN.SubInvNum AND
            InvRow.BillCode = ttInvCCN.BillCode:
            
      ldVatFact = 1 + InvRow.VatPerc / 100.
      
      IF ttInvCCN.VatIncl 
      THEN ASSIGN ttInvCCN.Net   = ttInvCCN.Net / ldVatFact
                  ttInvCCN.Gross = ttInvCCN.Gross / ldVatFact.
      ELSE ASSIGN ttInvCCN.Net   = ttInvCCN.Net * ldVatFact
                  ttInvCCN.Gross = ttInvCCN.Gross * ldVatFact.
                  
      FIND FIRST bttInvCCN where
                 bttInvCCN.InvSeq    = ttInvCCN.InvSeq    AND
                 bttInvCCN.CCN       = ttInvCCN.CCN       AND
                 bttInvCCN.BillCode  = ttInvCCN.BillCode  AND
                 bttInvCCN.TariffNum = ttInvCCN.TariffNum AND
                 bttInvCCN.VatIncl   = Invoice.VatIncl
      NO-ERROR.

      IF AVAILABLE bttInvCCN THEN DO:
         ASSIGN 
         bttInvCCN.Qty     = bttInvCCN.Qty   + ttInvCCN.Qty
         bttInvCCN.Min     = bttInvCCN.Min   + ttInvCCN.Min
         bttInvCCN.Net     = bttInvCCN.Net   + ttInvCCN.Net
         bttInvCCN.Gross   = bttInvCCN.Gross + ttInvCCN.Gross
         bttInvCCN.DataAmt = bttInvCCN.DataAmt + ttInvCCN.DataAmt.
      
         DELETE ttInvCCN.
      END.

      ELSE ASSIGN ttInvCCN.VatIncl = Invoice.VatIncl.
                  
   END. 

   FOR EACH ttInvASub WHERE
            ttInvASub.VatIncl NE Invoice.VatIncl,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            InvRow.SubInvNum = ttInvASub.SubInvNum AND
            InvRow.BillCode = ttInvASub.BillCode:
            
      ldVatFact = 1 + InvRow.VatPerc / 100.
      
      IF ttInvASub.VatIncl 
      THEN ASSIGN ttInvASub.Net    = ttInvASub.Net / ldVatFact
                  ttInvASub.Gross  = ttInvASub.Gross / ldVatFact.
      ELSE ASSIGN ttInvASub.Net    = ttInvASub.Net * ldVatFact
                  ttInvASub.Gross  = ttInvASub.Gross * ldVatFact.
                  
      FIND FIRST bttInvASub where
                 bttInvASub.InvSeq    = ttInvASub.InvSeq    AND
                 bttInvASub.CLI       = ttInvASub.CLI       AND
                 bttInvASub.CCN       = ttInvASub.CCN       AND
                 bttInvASub.BillCode  = ttInvASub.BillCode  AND
                 bttInvASub.TariffNum = ttInvASub.TariffNum AND
                 bttInvASub.VatIncl   = Invoice.VatIncl     AND
                 bttInvASub.ServRid   = ttInvASub.ServRid   AND
                 bttInvASub.MPMRid    = ttInvASub.MPMRid 
      NO-ERROR.

      IF AVAILABLE bttInvASub THEN DO:
         ASSIGN 
         bttInvASub.Qty      = bttInvASub.Qty   + ttInvASub.Qty
         bttInvASub.Min      = bttInvASub.Min   + ttInvASub.Min
         bttInvASub.Net      = bttInvASub.Net   + ttInvASub.Net
         bttInvASub.Gross    = bttInvASub.Gross + ttInvASub.Gross
         bttInvASub.DataAmt  = bttInvASub.DataAmt + ttInvASub.DataAmt
         bttInvASub.FromDate = MIN(bttInvASub.FromDate,ttInvASub.FromDate)
         bttInvASub.ToDate   = MAX(bttInvASub.ToDate,ttInvASub.ToDate).

         DELETE ttInvASub.
      END.

      ELSE ASSIGN ttInvASub.VatIncl = Invoice.VatIncl.
                  
   END. 
 
   /* take to db */ 
   FOR EACH ttInvASub:

      ASSIGN ttInvASub.GenPrice = ttInvASub.Gross
             ttInvASub.MPMAmt   = ttInvASub.Gross   
             ttInvASub.Amt      = ttInvASub.Net.

      /* data amounts to Kb */
      ttInvASub.DataAmt = ttInvASub.DataAmt / 1024.
      
      CREATE InvASub.
      BUFFER-COPY ttInvASub TO InvASub.
   END.

   FOR EACH ttInvCCN:

      ASSIGN ttInvCCN.GenPrice = ttInvCCN.Gross
             ttInvCCN.Amt      = ttInvCCN.Net.

      /* data amounts to Kb */
      ttInvCCN.DataAmt = ttInvCCN.DataAmt / 1024.
 
      CREATE InvCCN.
      BUFFER-COPY ttInvCCN TO InvCCN.
   END.

END.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.


