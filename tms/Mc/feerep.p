/* -----------------------------------------------------------------
  MODULE .......: feerep.p
  TASK .........: 
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 17.03.04
  CHANGED ......: 
  Version ......: MTV3
  ------------------------------------------------------------------ */
{testpaa.i}
{excel.i}
{accdatfi.i}
{date.i}

DEF VAR /* INPUT PARAMETER */ feedate1  AS DA  NO-UNDO.
DEF VAR /* INPUT PARAMETER */ feedate2  AS DA  NO-UNDO.

def var lDate as da no-undo.

assign 
   ldate = 03/18/04
   feedate1 = ldate - 1
   feedate2 = feedate1.


output stream excel to value("/tmp/fees" + 
                              fdatefmt(ldate,"yyyymmdd") + 
                              ".txt").
                                     
DEF VAR xPrice      AS DE NO-UNDO.
DEF VAR xVATPrice   AS DE NO-UNDO.
DEF VAR xNoVATPrice AS DE NO-UNDO.
DEF VAR ldRate      AS DE NO-UNDO.
def var i           as I  NO-UNDO.
DEF VAR iFDate1     AS I  NO-UNDO.
def var iFDate2     AS I  NO-UNDO.
def var remamt      AS DE NO-UNDO.
def var iFPer1      AS I  NO-UNDO.
def var iFPer2      AS I  NO-UNDO.
def var numform     as c  no-undo.
DEF VAR xBilled     AS LOG NO-UNDO.

DEF TEMP-TABLE ttFFees
   FIELD FeeDate    AS DA
   FIELD BIGroup    AS CH
   field bgname     as ch
   FIELD BillCode   AS CH
   Field biname     as ch
   FIELD Count      AS I
   FIELD VATPRice   AS DE
   FIELD NoVATPrice AS DE
   INDEX BillCode IS PRIMARY UNIQUE FeeDate BIGroup BillCode. 


ASSIGN
   iFDate1 = 10000 * YEAR(Feedate1) + 100 * MONTH(Feedate1) + DAY(Feedate1)
   iFDate2 = 10000 * YEAR(Feedate2) + 100 * MONTH(Feedate2) + DAY(Feedate2)
   iFPer1  = 100 * YEAR(Feedate1) + MONTH(Feedate1)
   iFPer2  = 100 * YEAR(Feedate2) + MONTH(Feedate2)
   numform = session:numeric-format
   session:numeric-format = "AMERICAN".

message ifdate1 ifdate2 view-as alert-box.   
   
message "Printing in progress, wait ...".

FOR EACH Customer NO-LOCK: 

   /* skip groups that are not invoiced */
   FIND InvGroup OF Customer NO-LOCK.
   IF InvGroup.BillPerm = FALSE THEN NEXT.

   put screen row 1 col 4 string(customer.custnum).
   
   FOR EACH FixedFee NO-LOCK WHERE
         FixedFee.CustNum = Customer.CustNum AND
         FixedFee.InUse,
      EACH FFItem OF FixedFee WHERE
           FFItem.Concerns[1] <= iFDate2 AND
           FFItem.Concerns[2] >= iFDate1:


      xPrice = fPeriodize(FeeDate1,
                          FeeDate2, 
                          ffitem.concerns[1], 
                          ffitem.concerns[2], 
                          ffitem.amt, 
                          remamt).
      
      /* remove VAT */
      IF FixedFee.VatIncl THEN ASSIGN
         xVATPrice   = xPrice
         xNoVATPrice = xPrice / 1.22.
      ELSE ASSIGN
         xVATPRICE   = xPrice * 1.22
         xNoVATPrice = xPrice.
          
      find first billitem no-lock where billitem.billcode = ffitem.billcode.
      find first bitemgroup no-lock where 
                 bitemgroup.bigroup = billitem.bigroup.
      
      FIND ttFFees WHERE 
           ttffees.feedate = feedate1 and
           ttFFees.BIGroup = bitemgroup.bigroup and
           ttFFees.BillCode = FFItem.BillCode NO-ERROR.

      IF NOT AVAIL ttFFees THEN DO:
      
         CREATE ttFFees.
         ASSIGN
            ttFFees.FeeDate  = feedate1
            ttFFees.bigroup  = billitem.bigroup
            ttFFees.BGName   = bitemgroup.bigname
            ttFFees.BillCode = FFItem.BillCode
            ttFfees.biname   = billitem.biname.
      END.

      ASSIGN   
         ttFFees.Count      = ttFFees.Count + 1
         ttFFees.VATPrice   = ttFFees.VATPrice + xVATPrice 
         ttFFEes.NoVATPrice = ttFFees.NoVATPrice + xNoVATPrice.
         
   END.
       
   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.CustNum = Customer.CustNum AND
            SingleFee.BillCode NE "deposit":

      ASSIGN liPer1 = IF SingleFee.Concerns[1] = 0 OR
                         SingleFee.Concerns[1] = ?
                      THEN SingleFee.BillPeriod
                      ELSE SingleFee.Concerns[1]
             liPer2 = IF SingleFee.Concerns[2] = 0 OR
                         SingleFee.Concerns[2] = ?
                      THEN liPer1
                      ELSE SingleFee.Concerns[2].
 
      /* check for erroneous period */
      DATE(INT(SUBSTRING(STRING(liPer1),5,2)),
           1, 
           INT(SUBSTRING(STRING(liPer1),1,4))) no-error.
                
      IF ERROR-STATUS:ERROR THEN ASSIGN
         liPer1 = SingleFee.BillPeriod
         liPer2 = SingleFee.BillPeriod.

      DATE(INT(SUBSTRING(STRING(liPer1),5,2)),
           1, 
           INT(SUBSTRING(STRING(liPer1),1,4))) no-error.

      IF ERROR-STATUS:ERROR THEN NEXT.

      /* check if fee is included in given period */
      IF fInt2Date(liPer1,1) > FeeDate2 OR
         fInt2Date(liPer2,2) < FeeDate1 THEN NEXT.
      
      /* calculate amount in period */
      xPrice = fPeriodize(FeeDate1,
                          FeeDate2, 
                          liPer1,
                          liPer2,
                          SingleFee.amt, 
                          remamt).
       
      /* remove VAT */
      IF SingleFee.VatIncl THEN ASSIGN
         xVATPrice   = xPrice
         xNoVATPrice = xPrice / 1.22.
      ELSE ASSIGN
         xVATPRICE   = xPrice * 1.22
         xNoVATPrice = xPrice.
          
          
      find first billitem no-lock where 
                 billitem.billcode = singlefee.billcode no-error.
      find first bitemgroup no-lock where 
                 bitemgroup.bigroup = billitem.bigroup no-error.
      
      FIND ttFFees WHERE 
           ttffees.feedate = feedate1 and
           ttFFees.BIGroup = bitemgroup.bigroup and
           ttFFees.BillCode = Singlefee.BillCode NO-ERROR.

      IF NOT AVAIL ttFFees THEN DO:
         
         CREATE ttFFees.
         ASSIGN
            ttFFees.FeeDate  = feedate1
            ttFFees.bigroup  = billitem.bigroup
            ttFFees.BGName   = bitemgroup.bigname
            ttFFees.BillCode = singlefee.BillCode
            ttFfees.biname   = billitem.biname.
      END.

      ASSIGN   
         ttFFees.Count      = ttFFees.Count + 1
         ttFFees.VATPrice   = ttFFees.VATPrice + xVATPrice
         ttFFees.NoVATPrice = ttFFees.NoVATPrice + xNoVATPrice.
         

   END.


END.

/** PRINTING STARTS ***/

put stream excel unformatted 
   
   "FeeDate" tab
   "BIGroup" tab
   "BGNAme"   tab
   "BillCode" tab 
   "BillName" tab
   "Amount" tab
   "ALV 0%" tab
   "ALV 22%" skip.

FOR EACH ttFFees NO-LOCK 
BREAK BY ttFFees.BIGroup:

   ACCUMULATE 
      ttFFees.Count      (TOTAL)
      ttFFees.NoVATPrice (TOTAL)
      ttFFees.VATPrice   (TOTAL).


   PUT STREAM excel UNFORMATTED
      ttFFees.FeeDate             tab
      ttFFees.BIGroup             tab
      ttFFees.BGName              tab
      ttFFees.BillCode            tab  
      ttFFees.biname              tab
      ttFFees.Count               tab 
      ROUND(ttFFees.NoVATPrice,2) tab 
      ROUND(ttFFees.VATPrice,2)   skip.
 
END.

output stream excel close.

session:numeric-format = numform.
