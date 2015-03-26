/* nncore1.i        30.04.2003/aam

   common functions for all routines that print a fee report 

   callers: nncore1.p   
            invoice_xml.p

            27.10.2003/aam vat percent from invoice row
            08.12.2003/aam FixedFee & SingleFee.VATIncl (and other vat stuff),
                           row headers on 2 lines
            16.06.2004/aam LineName               
            05.08.2004/aam ffitem-, singlefee- and fatime-search need custnum
            20.01.2005/aam Memo[1] to LineName if CLI is empty
            02.02.2006/aam use msowner.invcust when finding cli
                                  
*/

DEF TEMP-TABLE ttFee NO-UNDO
   FIELD Type      AS INT
   FIELD Prod      AS CHAR
   FIELD ProdName  AS CHAR
   FIELD LineName  AS CHAR
   FIELD CLI       AS CHAR
   FIELD BegPer    AS INT
   FIELD EndPer    AS INT
   FIELD BegDate   AS DATE
   FIELD EndDate   AS DATE
   FIELD Memo      AS CHAR EXTENT 5
   FIELD Amt       AS DEC
   FIELD AmtNoVat  AS DEC 
   FIELD VatPerc   AS DEC 
   FIELD TotHeader AS CHAR 
   FIELD ProdTot   AS CHAR
   FIELD GrandTotHead AS CHAR
   FIELD GrandAmtTot  AS CHAR
   INDEX Fee Type Prod CLI.

DEF BUFFER bFee FOR ttFee. 
DEF BUFFER bEventCust FOR Customer.

DEF VAR lcFeeOrgName  AS CHAR NO-UNDO.
DEF VAR lcFeeProdName AS CHAR NO-UNDO.
DEF VAR lcFeeVat      AS CHAR NO-UNDO. 
DEF VAR liFeeLang     AS INT  NO-UNDO. 
                            
DEF VAR lcFeeHead     AS CHAR NO-UNDO EXTENT 20. 
DEF VAR liFeeCnt      AS INT  NO-UNDO. 
DEF VAR liFeePoint    AS INT  NO-UNDO. 
DEF VAR lcFeeCLI      AS CHAR NO-UNDO. 


FUNCTION fInitFeeValues RETURNS LOGICAL.

   liFeeLang = Customer.Language.

   ASSIGN lcFeeHead[1]   = fTeksti(57,liFeeLang)
          lcFeeHead[2]   = fTeksti(28,liFeeLang)
          lcFeeHead[3]   = fTeksti(94,liFeeLang)
          lcFeeHead[4]   = fTeksti(61,liFeeLang)
          lcFeeHead[5]   = fTeksti(62,liFeeLang)
          lcFeeHead[6]   = IF Invoice.VatIncl = FALSE
                           THEN fTeksti(150,liFeeLang)
                           ELSE fTeksti(151,liFeeLang)
          lcFeeHead[7]   = fTeksti(48,liFeeLang)
          lcFeeHead[8]   = fTeksti(60,liFeeLang)
          lcFeeHead[9]   = fTeksti(157,liFeeLang)
          lcEPLRepHead   = fTeksti(57,liFeeLang) 
          lcSpecDateHead = fTeksti(136,liFeeLang).

   /* divide headers to 2 lines */
   DO liFeeCnt = 3 TO 9:
      IF liFeeCnt = 7 THEN NEXT. 
      liFeePoint = INDEX(lcFeeHead[liFeeCnt],"!").
      IF liFeePoint > 1 THEN ASSIGN 
         lcFeeHead[liFeeCnt + 10] = SUBSTRING(lcFeeHead[liFeeCnt],1,
                                                  liFeePoint - 1)
         lcFeeHead[liFeeCnt]      = SUBSTRING(lcFeeHead[liFeeCnt],
                                                  liFeePoint + 1).
   END.
           
   ASSIGN
   lcFeeHead[6]  = FILL(" ",10 - LENGTH(lcFeeHead[6]))  + lcFeeHead[6]
   lcFeeHead[16] = FILL(" ",10 - LENGTH(lcFeeHead[16])) + lcFeeHead[16]
   lcFeeHead[9]  = FILL(" ",5  - LENGTH(lcFeeHead[9]))  + lcFeeHead[9]
   lcFeeHead[19] = FILL(" ",5  - LENGTH(lcFeeHead[19])) + lcFeeHead[19].
          
END FUNCTION.

FUNCTION fCollectFees RETURNS LOGICAL.

   DEF VAR ldVatFactor   AS DEC  NO-UNDO. 
   
   FOR EACH FFItem NO-LOCK WHERE
            FFItem.InvNum   = Invoice.InvNum     AND
            FFItem.Billed   = TRUE, 
      FIRST FixedFee OF FFItem NO-LOCK,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            InvRow.BillCode = FFItem.BillCode AND
            InvRow.RowType  = 3:

      lcFeeCLI = "".
      IF FixedFee.HostTable = "MobSub" THEN 
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq   = INTEGER(FixedFee.KeyValue) AND
                MsOwner.InvCust = FixedFee.CustNum:
         lcFeeCLI = MsOwner.CLI.
      END.       
          
      CREATE ttFee.
      ASSIGN ttFee.Type     = 0
             ttFee.Prod     = FixedFee.BillCode
             ttFee.CLI      = lcFeeCLI
             ttFee.BegPer   = IF FFItem.Concerns[1] > 0 
                              THEN FFItem.Concerns[1] 
                              ELSE FFItem.BillPeriod
             ttFee.EndPer   = IF FFItem.Concerns[2] > 0 
                              THEN FFItem.Concerns[2] 
                              ELSE FFItem.BillPeriod
             ldVatFactor    = 1 + InvRow.VatPerc / 100
             ttFee.Amt      = FFItem.Amt *
                              (IF FixedFee.VatIncl
                               THEN (IF Invoice.VatIncl 
                                     THEN 1
                                     ELSE 1 / ldVatFactor)
                               ELSE (IF Invoice.VatIncl
                                     THEN ldVatFactor
                                     ELSE 1)
                              )
             ttFee.VatPerc = InvRow.VatPerc.

      DO liFeeCnt = 1 TO 5:  
          ttFee.Memo[liFeeCnt] = FFItem.Memo[liFeeCnt].
      END.
   
   END.

   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.InvNum   = Invoice.InvNum     AND
            SingleFee.Billed   = TRUE,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            InvRow.BillCode = SingleFee.BillCode AND
            InvRow.RowType  = 4:

      lcFeeCLI = "".
      IF SingleFee.HostTable = "MobSub" THEN 
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue) AND
                MsOwner.InvCust = SingleFee.CustNum:
         lcFeeCLI = MsOwner.CLI.
      END.
 
      CREATE ttFee.
      ASSIGN ttFee.Type     = 1
             ttFee.Prod     = SingleFee.BillCode
             ttFee.CLI      = lcFeeCLI
             ttFee.BegPer   = IF SingleFee.Concerns[1] > 0 
                              THEN SingleFee.Concerns[1] 
                              ELSE SingleFee.BillPeriod
             ttFee.EndPer   = IF SingleFee.Concerns[2] > 0 
                              THEN SingleFee.Concerns[2] 
                              ELSE SingleFee.BillPeriod
             ldVatFactor    = 1 + InvRow.VatPerc / 100
             ttFee.Amt      = SingleFee.Amt *
                              (IF SingleFee.VatIncl
                               THEN (IF Invoice.VatIncl 
                                     THEN 1
                                     ELSE 1 / ldVatFactor)
                               ELSE (IF Invoice.VatIncl
                                     THEN ldVatFactor
                                     ELSE 1)
                              )       
             ttFee.VatPerc  = InvRow.VatPerc.

      DO liFeeCnt = 1 TO 5:  
          ttFee.Memo[liFeeCnt] = SingleFee.Memo[liFeeCnt].
      END.
   END.         


   FOR EACH FATime NO-LOCK WHERE
            FATime.invnum  = Invoice.InvNum,
      FIRST fatgroup NO-LOCK WHERE
            FatGroup.Brand   = gcBrand AND
            fatgroup.ftgrp   = FATime.FTGrp,
      FIRST InvRow OF Invoice NO-LOCK WHERE
            InvRow.RowType = 7 AND
            InvRow.BillCode  = FATGroup.BillCode:
          
      CREATE ttFee.
      ASSIGN ttFee.Type    = 2
             ttFee.Prod    = FATGroup.billCode
             ttFee.CLI     = FATIME.cli 
             ttFee.BegPer  = FATime.period  
             ttFee.EndPer  = FATime.period
             ldVatFactor   = 1 + InvRow.VatPerc / 100
             ttFee.Amt     = -1 * Fatime.used /
                             /* fatime always includes vat */
                             (IF Invoice.VatIncl
                              THEN 1
                              ELSE ldVatFactor)
             ttFee.VatPerc = InvRow.VatPerc.

      DO liFeeCnt = 1 TO 5:  
          ttFee.Memo[liFeeCnt] = Fatime.memo[liFeeCnt].
      END.
   END.         
 
END FUNCTION.

FUNCTION fFeeProdName RETURNS LOGICAL
   (INPUT idtDate AS DATE):

   FIND BillItem WHERE 
        BillItem.Brand    = gcBrand AND
        BillItem.BillCode = ttFee.Prod NO-LOCK NO-ERROR.
   
   lcFeeProdName = fTranslationName(gcBrand,
                                    1,
                                    ttFee.Prod,
                                    liFeeLang,
                                    idtDate).
                                    
   IF lcFeeProdName = ? OR lcFeeProdName = "" THEN 
      lcFeeProdName = IF AVAILABLE BillItem
                      THEN BillItem.BiName
                      ELSE "UNKNOWN".
                  
   lcFeeOrgName = if available BillItem then BillItem.BIName else "".
   if lcFeeOrgName = "" then assign lcFeeOrgName = ttFee.Prod.
   
END FUNCTION.


FUNCTION fFeeSetValues RETURNS LOGICAL.

   IF ttFee.Type = 0 THEN ASSIGN 
      ttFee.BegDate = fInt2date(ttFee.BegPer,1)
      ttFee.EndDate = fint2DATE(ttFee.EndPer,2).
   ELSE ASSIGN 
      ttFee.BegDate = ?
      ttFee.EndDate = ?. 
             
   ttFee.Memo[1]  = TRIM(REPLACE(ttFee.Memo[1],lcFeeOrgName,"")).
   
   /* cli is primary description */
   IF ttFee.CLI > ""  THEN ttFee.LineName = ttFee.CLI.
   
   ELSE DO:
      /* if memo fits then use it, otherwise billing item name */
      IF ttFee.Memo[1] > "" AND
         LENGTH(ttFee.Memo[1]) <= 50
      THEN ASSIGN ttFee.LineName = ttFee.Memo[1]
                  ttFee.Memo[1]  = "".
      ELSE ttFee.LineName = lcFeeProdName.
   END.
   
END FUNCTION.

