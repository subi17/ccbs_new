/*-----------------------------------------------------------------------------
  MODULE .......: mobprodrep1.p
  FUNCTION .....: Product statistics information for mobile calls
  APPLICATION ..: tms
  CREATED ......: 28.04.03 tk converted from nnptla1
  MODIFIED .....: 04.08.03 tk invseq.billed
                  12.09.03 jp Brand
  VERSION ......: M15
----------------------------------------------------------------------------- */
{commali.i}
{fcurrency.i}

DEF SHARED TEMP-TABLE nnpvti
   FIELD pt-tuno AS CHAR
   FIELD pt-min  AS INT
   FIELD pt-kpl  AS INT
   FIELD pt-mk   AS DEC
   INDEX pt-tuno AS UNIQUE pt-tuno.


DEF INPUT  PARAMETER pvm1 AS DA NO-UNDO.
DEF INPUT  PARAMETER pvm2 AS DA NO-UNDO.

DEF VAR kpl      AS I     NO-UNDO.
DEF VAR xPrice   AS DEC   NO-UNDO.
DEF VAR ykpl    LIKE nnpvti.pt-kpl NO-UNDO.
DEF VAR ymin    LIKE nnpvti.pt-min NO-UNDO. 
DEF VAR ysum    LIKE nnpvti.pt-mk  NO-UNDO.
DEF VAR ldRate  AS DEC             NO-UNDO. 

EMPTY TEMP-TABLE nnpvti.

MESSAGE "New data from period"
        STRING(pvm1,"99-99-9999") "-" STRING(pvm2,"99-99-9999").

FOR EACH MobCDR NO-LOCK WHERE
         MobCDR.Date >= pvm1 AND
         MobCDR.Date <= pvm2 AND
         MobCDR.ErrorCode = 0,
   FIRST InvSeq NO-LOCK WHERE
         InvSeq.InvSeq = MobCDR.InvSeq AND
         InvSeq.Billed,      
   FIRST Customer NO-LOCK WHERE 
         Customer.CustNum = MobCDR.InvCust AND 
         Customer.Brand   = gcBrand :

   /* skip groups that are not invoiced */
   FIND InvGroup OF Customer  WHERE
        InvGroup.Brand = gcBrand NO-LOCK.
   IF InvGroup.BillPerm = FALSE THEN NEXT.

   kpl = kpl + 1.
   IF kpl MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY kpl FORMAT ">>>>>>>9"
              MobCDR.Date FORMAT "99-99-9999"
         WITH ROW 10 CENTERED NO-LABELS OVERLAY TITLE " Calls "
         FRAME fQty.
   END.

   /* currency unit sub or full */
   ASSIGN xPrice = (MobCDR.GrossAmt - MobCDR.DiscValue) /
                   (IF MobCDR.CurrUnit THEN 1 ELSE 100).

   /* currency */
   ldRate = fCurrRate(Customer.Currency,
                      MobCDR.Date).
   IF ldRate NE 1 THEN 
      xPrice = fToHomeCurr(xPrice,
                           ldRate). 

   /* remove VAT */
   IF MobCDR.VatIncl THEN DO:

      IF Customer.VatUsage < 3 THEN
      FOR FIRST BillItem NO-LOCK WHERE
                BillItem.BillCode = MobCDR.BillCode,
          FIRST VatCode NO-LOCK WHERE
                VatCode.VatCode = BillItem.VatCode:

          xPrice = xPrice / (1 + VatCode.VatPerc / 100). 
      END.      

   END.   

   FIND FIRST nnpvti WHERE
              nnpvti.pt-tuno = MobCDR.BillCode NO-ERROR.

   IF NOT AVAILABLE nnpvti THEN DO:
      CREATE nnpvti. 
      ASSIGN
      nnpvti.pt-tuno = MobCDR.BillCode.
   END.

   ASSIGN 
      nnpvti.pt-kpl  = nnpvti.pt-kpl + 1
      nnpvti.pt-min  = nnpvti.pt-min + MobCDR.BillDur
      nnpvti.pt-mk   = nnpvti.pt-mk  + xPrice.

END.

FOR EACH nnpvti:
   ASSIGN ykpl          = ykpl + nnpvti.pt-kpl
          ymin          = ymin + nnpvti.pt-min
          ysum          = ysum + nnpvti.pt-mk
          nnpvti.pt-min = INTEGER(nnpvti.pt-min / 60).
END.

CREATE nnpvti.
ASSIGN
nnpvti.pt-tuno = "TOTAL"
nnpvti.pt-kpl  = ykpl
nnpvti.pt-min  = INTEGER(ymin / 60)
nnpvti.pt-mk   = ysum.

HIDE FRAME fQty NO-PAUSE. 

