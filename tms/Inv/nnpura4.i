/* nnpura4.i       15.06.04/aam 

   callers: nnpura4.p       
            xmlinv.p

   changed:        26.10.04/aam parameter ilFullBNumber to fRep4SetLineValues
                   09.11.04/aam skip products listed in parameter SpecSkipProd
                   12.04.05/aam new columns -> more lcRep4Headers
                   04.08.05/aam use MobCDR.MPMAmt for mpm
                   18.01.06/aam fDispCustName()
*/

DEF VAR liRep4Lang        AS INT  NO-UNDO.
DEF VAR lcRep4CLIHeader   AS CHAR NO-UNDO.
DEF VAR lcRep4BItemHeader AS CHAR NO-UNDO.
DEF VAR lcRep4CCNHeader   AS CHAR NO-UNDO.

DEF VAR ldVatFactor   AS DEC   NO-UNDO. 
DEF VAR llVatUsage    AS INT   NO-UNDO.
DEF VAR llDispVAT     AS LOG   NO-UNDO. 
DEF VAR liTaff        AS INT   NO-UNDO. 
DEF VAR lcRep4Head    AS CHAR  NO-UNDO EXTENT 11. 
DEF VAR lcRep4UHead   AS CHAR  NO-UNDO EXTENT 11. 
DEF VAR lcRep4SubHead AS CHAR  NO-UNDO.
DEF VAR liMSSeq       AS INT   NO-UNDO. 
DEF VAR liPrCust      AS INT   NO-UNDO.
DEF VAR liCLIQty      AS INT   NO-UNDO. 
DEF VAR llRep4        AS LOG   NO-UNDO. 
DEF VAR lcRep4BSub    AS CHAR  NO-UNDO.
DEF VAR liRep4DataAmt AS INT   NO-UNDO. 
DEF VAR lcRep4Dur     AS CHAR  NO-UNDO. 

DEF TEMP-TABLE ttCall NO-UNDO
   FIELD Date       AS DATE
   FIELD TimeStart  AS INT
   FIELD CallCust   AS INT
   FIELD CLI        AS CHAR
   FIELD BillCode   AS CHAR
   FIELD CCN        AS INT
   FIELD Apiece     AS DEC 
   FIELD UnitPrice  AS DEC 
   FIELD Mpm        AS DEC
   FIELD Amt        AS DEC
   FIELD DataAmt    AS DEC 
   FIELD Pulses     AS INT
   /* billed total duration */
   FIELD BillDur    AS DEC
   /* duration per time zone */
   FIELD Duration   AS DEC
   FIELD BSub       AS CHAR
   FIELD BDest      AS CHAR
   FIELD BType      AS INT 
   FIELD VATIncl    AS LOG 
   /* for xml */
   FIELD CDur       AS CHAR
   FIELD CPrice     AS CHAR
   FIELD CApiece    AS CHAR
   FIELD CDataAmt   AS CHAR 
   INDEX CallCust VATIncl CallCust CLI BillCode CCN.

FUNCTION fRep4CustHeader RETURNS LOGICAL
   (iiInvNum    AS INT).

   ASSIGN 
   liCallCust = Customer.CustNum
   liInvCust  = Customer.InvCust
   liRepCust  = Customer.RepCust
   liRep4Lang = Customer.Language
   erisivu    = INDEX(Customer.RepCodes,"-") > 0
   sasnimi    = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                 BUFFER Customer)
   lasnimi    = sasnimi.

   IF liRepCust NE Customer.CustNum THEN DO:
      FIND FIRST xCustomer WHERE xCustomer.CustNum = liRepCust
      NO-LOCK NO-ERROR.
      IF AVAILABLE xCustomer 
      THEN liRep4Lang   = xCustomer.Language.
   END.

   IF liInvCust NE Customer.CustNum THEN DO:
      FIND FIRST xCustomer WHERE xCustomer.CustNum = liInvCust
      NO-LOCK NO-ERROR.
      IF AVAILABLE xCustomer THEN 
         lasnimi = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                    BUFFER xCustomer).
      ELSE lasnimi = "".
   END.
   
   /* header txts */
   ASSIGN lcRepHeader    = fTeksti(19,liRep4Lang)
          lcEPLRepHead   = lcRepHeader
          lcRep4Head     = ""
          lcRep4Head[1]  = fTeksti(30,liRep4Lang)
          lcRep4Head[2]  = fTeksti(21,liRep4Lang)
          lcRep4Head[3]  = fTeksti(22,liRep4Lang)
          lcRep4Head[4]  = fTeksti(29,liRep4Lang)
          lcRep4Head[5]  = fTeksti(95,liRep4Lang)
          lcRep4Head[9]  = fTeksti(17,liRep4Lang)
          lcRep4Head[10] = fTeksti(192,liRep4Lang)
          lcRep4Head[11] = fTeksti(193,liRep4Lang)
          otsi[1]        = fTeksti(1,liRep4Lang)
          otsi[3]        = fTeksti(3,liRep4Lang)
          otsi[6]        = fTeksti(6,liRep4Lang)
          otsi[14]       = fTeksti(14,liRep4Lang)
          otsi[16]       = fTeksti(16,liRep4Lang)
          otsi[28]       = fTeksti(28,liRep4Lang)
          otsi[50]       = fTeksti(50,liRep4Lang)
          liCLIQty       = 0.
                
   IF llDispVat = FALSE THEN ASSIGN 
          lcRep4Head[6] = fTeksti(153,liRep4Lang)
          lcRep4Head[7] = fTeksti(155,liRep4Lang)
          lcRep4Head[8] = fTeksti(150,liRep4Lang).
   ELSE ASSIGN                 
          lcRep4Head[6] = fTeksti(154,liRep4Lang)
          lcRep4Head[7] = fTeksti(156,liRep4Lang)
          lcRep4Head[8] = fTeksti(151,liRep4Lang).

   DO i = 1 TO 11:
      liTaff = INDEX(lcRep4Head[i],"!").
      IF liTaff > 1 THEN ASSIGN 
         lcRep4UHead[i] = SUBSTRING(lcRep4Head[i],1,liTaff - 1)
         lcRep4Head[i]  = SUBSTRING(lcRep4Head[i],liTaff + 1).
      END.
  
   ASSIGN 
   lcRep4Head[5]   = FILL(" ",7 -  LENGTH(lcRep4Head[5]))   + lcRep4Head[5]
   lcRep4UHead[5]  = FILL(" ",7 -  LENGTH(lcRep4UHead[5]))  + lcRep4UHead[5]
   lcRep4Head[6]   = FILL(" ",9 -  LENGTH(lcRep4Head[6]))   + lcRep4Head[6]
   lcRep4UHead[6]  = FILL(" ",9 -  LENGTH(lcRep4UHead[6]))  + lcRep4UHead[6]
   lcRep4Head[7]   = FILL(" ",8 -  LENGTH(lcRep4Head[7]))   + lcRep4Head[7]
   lcRep4UHead[7]  = FILL(" ",8 -  LENGTH(lcRep4UHead[7]))  + lcRep4UHead[7]
   lcRep4Head[8]   = FILL(" ",11 - LENGTH(lcRep4Head[8]))   + lcRep4Head[8]
   lcRep4UHead[8]  = FILL(" ",11 - LENGTH(lcRep4UHead[8]))  + lcRep4UHead[8]
   lcRep4Head[9]   = FILL(" ",5  - LENGTH(lcRep4Head[9]))   + lcRep4Head[9]
   lcRep4UHead[9]  = FILL(" ",5  - LENGTH(lcRep4UHead[9]))  + lcRep4UHead[9]
   lcRep4Head[10]  = FILL(" ",11 - LENGTH(lcRep4Head[10]))  + lcRep4Head[10]
   lcRep4UHead[10] = FILL(" ",11 - LENGTH(lcRep4UHead[10])) + lcRep4UHead[10]
   lcRep4Head[11]  = FILL(" ",11 - LENGTH(lcRep4Head[11]))  + lcRep4Head[11]
   lcRep4UHead[11] = FILL(" ",11 - LENGTH(lcRep4UHead[11])) + lcRep4UHead[11].

   IF iiInvNum > 0 
   THEN lcRep4SubHead = fTeksti(65,liRep4Lang) + " " + STRING(iiInvNum).

   lcSpecDateHead = fTeksti(136,liRep4Lang).
   
   RETURN TRUE. 

END FUNCTION.

/* Is this  a PNP number */
FUNCTION fIsPNP RETURNS LOGICAL
  (INPUT  iCustNum AS INT,
   INPUT  iBSub    AS CHAR).
 
   RETURN FALSE.
END.   

FUNCTION fVatIncl RETURNS LOGICAL
   (ilCallIncl AS LOG,
    icBillCode AS CHAR,
    iiFromType AS INT,
    iiToType   AS INT,
    iiInvNum   AS INT).
    
   ldVatFactor = 1.
   
   /* is call's vat method different than report's general method */
   IF ilCallIncl NE llDispVAT AND
      llVATUsage <= 2
   THEN DO:
      
      /* get vat% from invoice row or through product */
      IF iiInvNum > 0 THEN DO:
         /* time period is irrelevant, vat is based on product code */
         FIND FIRST InvRow OF Invoice NO-LOCK WHERE
                    InvRow.BillCode = icBillCode AND
                    InvRow.RowType >= iiFromType AND
                    InvRow.RowType <= iiToType NO-ERROR.
         IF AVAILABLE InvRow 
         THEN ldVATFactor = 1 + InvRow.VatPerc / 100.
         
      END. 
      ELSE FOR FIRST BillItem NO-LOCK WHERE
                     BillItem.Brand    = gcBrand AND
                     BillItem.BillCode = icBillCode,
               FIRST VatCode OF BillItem NO-LOCK:
               
          ldVATFactor = 1 + VatCode.VatPerc / 100.
      END.
      
      /* call includes vat but report shouldn't -> remove vat */
      IF ilCallIncl THEN ldVatFactor = 1 / ldVatFactor.
      
      ttCall.Amt = ttCall.Amt * ldVatFactor.
   END.   
 
END FUNCTION.

FUNCTION fUnitPrice RETURNS LOGICAL
   (iiTariff AS INT,
    iiSecTar AS INT,
    icType   AS CHAR).
 
   DEF VAR liDur AS INT EXTENT 6 NO-UNDO.
    
   IF icType = "fix" THEN DO:
      IF NOT AVAILABLE FixCDR THEN RETURN FALSE.
      DO liTaff = 1 TO 6:
         liDur[liTaff] = FixCDR.TBDurat[liTaff].
      END.
   END.
   ELSE IF icType = "mob" THEN DO:
      IF NOT AVAILABLE MobCDR THEN RETURN FALSE.
      
      liDur[1] = MobCdr.BillDur.
      
   END.
  
   llRep4 = FALSE.
      
   /* old type of mob data call */
   IF iiSecTar > 0 THEN DO:
      FIND Tariff WHERE 
           Tariff.Brand     = gcBrand AND
           Tariff.TariffNum = iiSecTar NO-LOCK NO-ERROR.
      IF AVAILABLE Tariff THEN ASSIGN 
         ttCall.UnitPrice = Tariff.Price[1] * ldVatFactor
         ttCall.Apiece    = Tariff.StartCharge[1] * ldVatFactor
         ttCall.Duration  = ttCall.BillDur
         llRep4             = TRUE.
   END.         
   
   FIND Tariff WHERE 
        Tariff.Brand     = gcBrand AND
        Tariff.TariffNum = iiTariff NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Tariff THEN RETURN FALSE.

   /* sms */
   IF icType = "mob" AND
      ttCall.billdur = 0 AND
      ttCall.DataAmt = 0 AND
      llRep4 = FALSE
   THEN ttCall.Apiece = Tariff.StartCharge[1] * ldVatFactor.

   /* gprs */
   ELSE IF icType = "mob"     AND 
           ttCall.DataAmt > 0 AND
           llRep4 = FALSE
   THEN ASSIGN ttCall.UnitPrice = Tariff.Price[1] * ldVatFactor
               ttCall.Apiece    = Tariff.StartCharge[1] * ldVatFactor.

   ELSE DO liTaff = 1 TO 6:
 
      IF liDur[liTaff] = 0 THEN NEXT.
          
      /* first one to decimal fields (usually only one price) */
      IF NOT llRep4 THEN DO:
         ASSIGN 
         llRep4           = TRUE
         ttCall.UnitPrice = Tariff.Price[liTaff] * ldVatFactor
         ttCall.APiece    = Tariff.StartCharge[liTaff] * ldVatFactor
         ttCall.Duration  = liDur[liTaff].

         /* pulse charge */
         IF icType = "mob" AND MobCDR.Charge NE 0 THEN DO:
         END.
      END.
                  
      ELSE DO:
         
         /* same price, just add duration */   
         IF Tariff.Price[liTaff]    * ldVatFactor = ttCall.UnitPrice AND
            Tariff.StartCharge[liTaff] * ldVatFactor = ttCall.APiece
         THEN ttCall.Duration = ttCall.Duration + liDur[liTaff].
         
         /* price differs */
         ELSE ASSIGN 
         ttCall.CPrice    = ttCall.CPrice + 
                            (IF ttCall.CPrice > ""
                             THEN "/"
                             ELSE "") +
                            STRING(Tariff.Price[liTaff] * ldVatFactor)
         ttCall.CApiece   = ttCall.CAPiece + 
                            (IF ttCall.CAPiece > ""
                             THEN "/"
                             ELSE "") +
                            STRING(Tariff.StartCharge[liTaff] * ldVatFactor)
         ttCall.CDur      = ttCall.CDur + 
                            (IF ttCall.CDur > ""
                             THEN "/"
                             ELSE "") +
                            STRING(liDur[liTaff]).
      END.
   END.
 
END.
 
FUNCTION fCollFixCDR RETURNS LOGICAL
   (iiInvNum AS INT).

   /* all products are not printed */
   IF LOOKUP(FixCDR.BillCode,lcSkipProd) > 0 THEN RETURN FALSE.
   
   CREATE ttCall.
   ASSIGN ttCall.CallCust   = FixCDR.CustNum
          ttCall.BillCode   = FixCDR.BillCode
          ttCall.CCN        = FixCDR.CCN
          ttCall.VATIncl    = llDispVAT
          ttCall.BillDur    = FixCDR.Duration
          ttCall.BSub       = FixCDR.BSub
          ttCall.Date       = FixCDR.Date
          ttCall.TimeStart  = FixCDR.TimeStart
          ttCall.BDest      = FixCDR.BDest.
          
    /* is currency unit full or sub (assign ldnet & ldgross) */
    fCurrUnit(FixCDR.GrossPrice - FixCDR.DiscValue,
              FixCDR.GrossPrice,
              FixCDR.CurrUnit,
              "",
              FixCDR.TariffID,
              gcBrand,
              OUTPUT ttCall.Amt,
              OUTPUT ldGross).   

     ttCall.CLI = FixCDR.CLI.

   /* should vat be removed/added */
   fVatIncl(FixCDR.VatIncl,
            FixCDR.BillCode,
            1,
            1,
            iiInvNum).
            
   /* unit price (uses ldVatFactor from fVatIncl) */
   IF FixCDR.GrossPrice > 0 THEN DO:
      fUnitPrice(FixCDR.TariffID,
                 0,
                 "fix"). 
   END.
   ELSE ttCall.Duration = ttCall.BillDur.
           
END FUNCTION.

FUNCTION fCollMobCDR RETURNS LOGICAL
   (iiInvNum AS INT).

   /* all products are not printed */
   IF LOOKUP(MobCDR.BillCode,lcSkipProd) > 0 THEN RETURN FALSE.

   CREATE ttCall.
   ASSIGN ttCall.CallCust   = MobCDR.CustNum
          ttCall.BillCode   = MobCDR.BillCode
          ttCall.CCN        = MobCDR.ccn
          ttCall.VATIncl    = llDispVat
          ttCall.BillDur    = MobCDR.BillDur
          ttCall.BSub       = MobCDR.GSMBNr
          ttCall.Date       = MobCDR.DateSt
          ttCall.TimeStart  = MobCDR.TimeStart
          ttCall.DataAmt    = IF MobCDR.DataIn = ? OR MobCDR.DataOut = ?
                              THEN 0
                              ELSE MobCDR.DataIn + MobCDR.DataOut
          ttCall.Pulses     = MobCDR.Pulses                     
          ttCall.BDest      = MobCDR.BDest
          ttCall.BType      = MobCDR.BType.

   /* is currency unit full or sub (assign ldnet & ldgross) */
   fCurrUnit(MobCDR.Amount,
             MobCDR.MPMAmt,
             MobCDR.CurrUnit,
             "",
             MobCDR.TariffNum,
             gcBrand,
             OUTPUT ttCall.Amt,
             OUTPUT ttCall.Mpm).   

   ttCall.CLI = MobCDR.CLI.

   /* should vat be removed/added */
   fVatIncl(MobCDR.VatIncl,
            MobCDR.BillCode,
            2,
            99,
            iiInvNum).

   /* unit price (uses ldVatFactor from fVatIncl) */
   IF MobCDR.Amount > 0 THEN DO:

      fUnitPrice(MobCDR.TariffNum,
                 0,
                 "mob").
                                               
   END.
   ELSE ttCall.Duration = ttCall.BillDur.
   
END FUNCTION.

FUNCTION fCollInvCDR RETURNS LOGICAL
   (icCLI AS CHAR).
   
   EMPTY TEMP-TABLE ttCall.
   
   /* disp method of this report */
   ASSIGN llDispVAT  = Invoice.VATIncl
          llVatUsage = Invoice.VatUsage.
    
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      FOR EACH FixCDR NO-LOCK WHERE
               FixCDR.InvSeq = SubInvoice.InvSeq AND
              (IF icCLI NE "" 
               THEN FixCDR.CLI = icCLI 
               ELSE TRUE):
     
         fCollFixCDR(Invoice.InvNum).
      END.
   
      FOR EACH MobCDR NO-LOCK WHERE
               MobCDR.InvCust = Invoice.CustNum  AND
               MobCDR.InvSeq  = SubInvoice.InvSeq   AND
              (IF icCLI NE "" 
               THEN MobCDR.CLI = icCLI
              ELSE TRUE):
       
         fCollMobCDR(Invoice.InvNum).
      END.   
   END. 

END FUNCTION.

FUNCTION fRep4SetPrintValues RETURNS LOGICAL
   (idDur     AS DEC,
    idDataAmt AS DEC).

   ASSIGN 
   /* duration into hh:mm format */
   lcRep4Dur     = fSec2C(idDur,10)
   /* data amounts to Kb */
   liRep4DataAmt = INT(idDataAmt / 1024).
END.

FUNCTION fRep4SetLineValues RETURNS LOGICAL
   (ilFullBNumber AS LOG).

   lcRep4BSub  = ttCall.BSub.
   if lcRep4BSub begins "00000" THEN lcRep4BSub = substr(lcRep4BSub,6).

   IF NOT ilFullBNumber THEN
          /* Modify BSUB FOR reporting: fXBSub uses {&country} */
   lcRep4BSub = DYNAMIC-FUNCTION("fHideBSub" IN ghFunc1,
                                 ttcall.bsub,
                                 ttcall.callcust,
                                 ttcall.bdest,
                                 ttCall.BType,
                                 "",
                                 TRUE).
 
   fRep4SetPrintValues(ttCall.Duration,
                       ttCall.DataAmt).
 
END FUNCTION.

