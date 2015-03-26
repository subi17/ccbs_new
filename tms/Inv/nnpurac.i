/* nnpurac.i    19.03.03/aam
   common functions for call specifications

                09.09.03/aam brand
                09.11.04/aam SpecSkipProd
                04.05.05/aam fDispMPM
                15.12.05/aam user name from customer, not msowner
*/

{fgetclis.i}

DEF BUFFER xCustomer FOR Customer.

DEF VAR liRepCust  LIKE Customer.CustNum  NO-UNDO.
DEF VAR liCallCust LIKE Customer.CustNum  NO-UNDO.
DEF VAR liInvCust  LIKE Customer.CustNum  NO-UNDO.
DEF VAR liRateCust LIKE Customer.CustNum  NO-UNDO.

DEF VAR SAsNimi       AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR RAsNimi       AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR LAsNimi       AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR AAsNimi       AS CHAR FORMAT "x(30)"          NO-UNDO.

DEF VAR company     AS CHAR                 NO-UNDO.
DEF VAR tila1       AS LOG                  NO-UNDO.
DEF VAR tila2       AS LOG                  NO-UNDO.
def var viiva1      as char format "x(117)" NO-UNDO.
DEF VAR viiva2      LIKE viiva1.           
DEF VAR viiva3      LIKE viiva1.
DEF VAR viiva4      LIKE viiva1.
DEF VAR sl          AS INT                  NO-UNDO.
DEF VAR rl          AS INT                  NO-UNDO.
DEF VAR lev         AS INT init 117         NO-UNDO.                
def var ke          as log format "Yes/No"  NO-UNDO init "No".
def var dmanimi     as char format "x(30)"  NO-UNDO.
def var atil        as char format "x(50)"  NO-UNDO.

DEF VAR xBillCodeName AS CHAR NO-UNDO.
DEF VAR xInvStamp     AS DEC  NO-UNDO. 
DEF VAR lcTimeFormat  AS CHAR NO-UNDO INIT "zzzz99".
DEF VAR ldNet         AS DEC  NO-UNDO. 
DEF VAR ldGross       AS DEC  NO-UNDO. 
DEF VAR ldVAT         AS DEC  NO-UNDO.
DEF VAR lcVAT         as char no-undo format "x(40)".
DEF VAR llVatUsed     AS LOG  NO-UNDO.
DEF VAR kieli         AS INT  NO-UNDO.
DEF VAR teksti        AS CHAR NO-UNDO.
DEF VAR rc            AS INT  NO-UNDO.
DEF VAR i             AS INT  NO-UNDO.
DEF VAR erisivu       AS LOG  NO-UNDO.

def var atyyp      as char format "x(2)" NO-UNDO.
def var ertpris    as char format "x(6)" NO-UNDO.
DEF VAR esc        AS CHAR NO-UNDO.
DEF VAR hinnat     AS CHAR NO-UNDO.
DEF VAR krmin      AS CHAR NO-UNDO.
DEF VAR h          AS INT  NO-UNDO.
DEF VAR ldHinta    AS DE   NO-UNDO.

DEF VAR lcRepHeader    AS CHAR NO-UNDO. 
DEF VAR otsi           AS CHAR NO-UNDO EXTENT 160.
DEF VAR lcEffOn        AS CHAR NO-UNDO. 
DEF VAR lcSpecDateHead AS CHAR NO-UNDO.
DEF VAR lcSkipProd     AS CHAR NO-UNDO. 

/* freephone prefix */
def var fri-b-prefix     as char no-undo init "020".

esc = chr(027).

/* products that will not be printed */
lcSkipProd = fCParamC("SpecSkipProd").

form header
  skip(1)
  otsi[4] format "x(20)" AT 5
     liRepCust  FORMAT ">>>>>>>9"
     RAsNimi 
     otsi[36] format "x(50)" AT 65 
     SKIP
  otsi[1] format "x(20)" AT 5
     liCallCust FORMAT ">>>>>>>9"
     SAsNimi 
     otsi[37] format "x(50)" AT 65 
     SKIP
  otsi[3] format "x(20)" AT 5
     liInvCust  FORMAT ">>>>>>>9"
     LAsNimi 
     otsi[39] format "x(50)" AT 65 
     SKIP
  otsi[2] format "x(20)" AT 5
     liRateCust FORMAT ">>>>>>>9"
     AAsNimi 
  SKIP(2)
WITH width 130 NO-LABEL no-box FRAME fasotsi.


/* discount type */
FUNCTION fDiscType RETURNS CHARACTER
   (iiDiscType  AS INT,
    iiTariff    AS INT).

   DEF VAR lcDType AS CHAR NO-UNDO.

   /* first check if pricelist is dedicated */
   FOR FIRST Tariff NO-LOCK WHERE
             Tariff.Brand     = gcBrand AND   
             Tariff.TariffNum = iiTariff:

       IF Tariff.CustNum > 0
       THEN lcDType = "N".
   END.

   IF lcDType = "" THEN 
   CASE iiDiscType:
   WHEN 0 THEN lcDType = "".
   WHEN 1 THEN lcDType = "N%".
   WHEN 2 THEN lcDType = "PR". 
   OTHERWISE lcDType = "".
   END CASE.  

   RETURN lcDType.

END FUNCTION.

FUNCTION fBillItemVat RETURNS DECIMAL
   (iiInvNum   AS INT,
    icBillCode AS CHAR,
    ilVatUsed  AS LOGIC).

   DEF VAR ldBIVatPerc AS DEC NO-UNDO.

   ldBIVatPerc = 0.

   IF iiInvNum NE 0 THEN 
   FOR FIRST InvRow NO-LOCK WHERE
             InvRow.InvNum   = iiInvNum  AND
             InvRow.BillCode = icBillCode:
      ldBIVatPerc = InvRow.VatPerc.
   END.

   ELSE IF ilVatUsed THEN 
   FOR FIRST BillItem NO-LOCK WHERE 
             BillItem.Brand    = gcBrand AND
             BillItem.BillCode = icBillCode,
       FIRST VatCode NO-LOCK WHERE 
             VatCode.VatCode = BillItem.VatCode:
      ldBIVatPerc = VatCode.VatPerc.
   END.

   RETURN ldBIVatPerc. 

END FUNCTION.

/* VAT header; % included in prices */
FUNCTION fVatTitle RETURNS CHARACTER
   (iiLanguage AS INT,
    iiInvNum   AS INT,
    icBillCode AS CHAR,
    ilVatUsed  AS LOG,
    ilVatIncl  AS LOG).

   DEF VAR liErr     AS INT  NO-UNDO.
   DEF VAR lcHeader  AS CHAR NO-UNDO.
   DEF VAR ldVatPerc AS DEC  NO-UNDO. 

   ASSIGN lcHeader = otsi[72]
          ldVatPerc = 0.

   IF ilVatIncl AND ilVatUsed THEN DO:
      ldVatPerc = fBillItemVat(iiInvNum,
                               icBillCode,
                               ilVatUsed).
   END.

   lcHeader = lcHeader + " " + STRING(ldVatPerc,"z9.99") + " %".

   RETURN lcHeader. 

END FUNCTION.

FUNCTION fCLIOwner RETURNS LOGICAL
   (icCLI     AS CHAR,
    iiCustNum AS INT,
    idtDate   AS DATE).

   xInvStamp = YEAR(idtDate) * 10000 +
               MONTH(idtDate) * 100  +
               DAY(idtDate). 

   /* FIRST try TO FIND a mobile CLI */
   FOR FIRST MSOwner NO-LOCK WHERE 
             MSOwner.Brand    = gcBrand    AND
             MSOwner.CLI      =  icCLI     AND
             MSOwner.TSBegin <= xInvStamp  AND
             MSOwner.TSEnd   >= xInvStamp,
       FIRST xCustomer NO-LOCK WHERE
             xCustomer.CustNum = MsOwner.CustNum:

      IF NOT can-find(FIRST wCLI WHERE wCLI.CLI = icCLI) 
      THEN DO:

         CREATE wCLI.
         ASSIGN wCLI.CLI     = icCLI
                wCLI.CustNum = MSOwner.CustNum
                wCLI.OwnerID = RECID(MSOwner).
                wCLI.Owner   = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                BUFFER xCustomer).
      END.                       
   END.

   IF CAN-FIND(FIRST wCLI WHERE wCLI.CLI = icCLI) 
   THEN RETURN TRUE.

   /* FIND fixed CLI IF mobile was NOT found */
   FOR FIRST CLI NO-LOCK WHERE 
             CLI.CLI      = icCLI       AND
             CLI.CrStamp <= xInvStamp   AND
             CLI.ClStamp >= xInvStamp:

      CREATE wCLI.
      ASSIGN wCLI.CLI     = icCLI
             wCLI.Owner   = CLI.OwnerName
             wCLI.CustNum = CLI.CustNum
             wCLI.OwnerID = -1 * INTEGER(RECID(CLI)).
   END.          

   IF CAN-FIND(FIRST wCLI WHERE wCLI.CLI = icCLI) 
   THEN RETURN TRUE.

   ELSE IF iiCustNum > 0 THEN DO:
      CREATE wCLI.
      ASSIGN wCLI.CLI     = icCLI
             wCLI.CustNum = iiCustNum.
   END.

END FUNCTION.

FUNCTION fCLIHeader RETURNS CHARACTER
   (icCLI   AS CHAR,
    idtDate AS DATE).

   DEF VAR lcCLIHeader AS CHAR NO-UNDO.

   lcCLIHeader = icCLI.

   FIND FIRST wCLI WHERE
              wCLI.CLI = icCLI 
   NO-ERROR.

   /* if not yet determined, try to find owner now */
   IF NOT AVAILABLE wCLI AND idtDate NE ? THEN DO:
      fCLIOwner(icCLI,
                0,
                idtDate).

      FIND FIRST wCLI WHERE
                 wCLI.CLI = icCLI 
      NO-ERROR.
   END.    

   IF AVAIL wCLI AND wCLI.Owner ne "" 
   THEN lcCLIHeader = lcCLIHeader + " (" + wCLI.Owner + ")".

   RETURN lcCLIHeader. 

END FUNCTION. 

FUNCTION fCustHeader RETURNS LOGICAL.

   ASSIGN 
   liCallCust = Customer.CustNum
   liRepCust  = Customer.RepCust
   liInvCust  = Customer.InvCust
   liRateCust = Customer.RateCust
   kieli      = Customer.Language
   erisivu    = index(Customer.RepCodes,"-") > 0.

   /* company name */
   FIND invgroup of Customer NO-LOCK NO-ERROR.
   IF AVAIL invgroup THEN company = InvGroup.CompName.
   ELSE                   company = ynimi.

   /* Haetaan RaportointitCustomer */
   FIND FIRST xCustomer WHERE xCustomer.CustNum = liRepCust
      NO-LOCK NO-ERROR.
   IF AVAILABLE xCustomer 
   THEN kieli   = xCustomer.Language.

   /* Haetaan asiakkaan kielen mukaiset otsikkotekstit */
   DO i = 1 TO 160:
      otsi[i] = fTeksti(i,kieli).
   END.

   lcSpecDateHead = fTeksti(136,kieli).
   lcEPLRepHead   = otsi[57].
   
   /* Haetaan soittoCustomer */
   SAsNimi = otsi[18]. 
   FIND FIRST xCustomer WHERE xCustomer.CustNum = liCallCust
      NO-LOCK NO-ERROR.
   IF AVAILABLE xCustomer THEN 
      SAsNimi = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                 BUFFER xCustomer).

   /* Haetaan laskutusCustomer */
   ASSIGN LAsNimi   = otsi[18]
          llVatUsed = TRUE. 
   FIND FIRST xCustomer WHERE xCustomer.CustNum = liInvCust
      NO-LOCK NO-ERROR.
   IF AVAILABLE xCustomer THEN ASSIGN 
      LAsNimi   = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                   BUFFER xCustomer)
      llVatUsed = (xCustomer.VatUsage < 3). 

   RETURN TRUE. 

END FUNCTION.

FUNCTION fDispMPM RETURNS LOGICAL
   (icBillCode AS CHAR).

   FIND BillItem WHERE
        BillItem.Brand    = gcBrand AND
        BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BillItem THEN RETURN FALSE.
   
   RETURN BillItem.DispMPM.
   
END FUNCTION.

