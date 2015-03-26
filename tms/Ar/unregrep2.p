/*-----------------------------------------------------------------------------
  MODULE .......: UNREGREP2.P
  FUNCTION .....: Print unregistered payments
  APPLICATION ..: 
  AUTHOR .......: tk
  CREATED ......: 12.08.02
  MODIFIED .....: 13.11.02/aam state 2 noted 
                  11.09.03/aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{commali.i}
{utumaa.i}

DEF INPUT PARAMETER iDate AS DA NO-UNDO.
DEF INPUT PARAMETER iSpec  AS LO NO-UNDO.

def var viiva1 as char format "x(78)" no-undo.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR order AS INT.
DEF VAR sl AS INT NO-UNDO.
DEF VAR rl AS INT NO-UNDO.
DEF VAR rlx AS INT NO-UNDO.
DEF VAR lev AS INT init 78 NO-UNDO.

DEF VAR xSessionNum AS CHAR NO-UNDO.

DEF VAR gtotalsum  AS DE NO-UNDO.
DEF VAR gbookedsum AS DE NO-UNDO.
DEF VAR gremainsum AS DE NO-UNDO.
DEF VAR qty        AS I  NO-UNDO.
DEF VAR totqty     AS I  NO-UNDO.


DEF VAR loop AS i no-undo.

DEF TEMP-TABLE wBal NO-UNDO
   FIELD Source AS CHAR
   FIELD Amount AS INT
   FIELD Total  AS DEC
   FIELD Paid   AS DEC
   FIELD Remain AS DEC
   INDEX Source AS UNIQUE Source.

DEF TEMP-TABLE wDef NO-UNDO
   FIELD Source   AS CHAR
   FIELD AccDate  AS DA
   FIELD PaymDate AS DA
   FIELD CustName AS C
   FIELD Sum      AS DE
   FIELD Paid     AS DE
   FIELD Remain   AS DE
   INDEX AccDate  AccDate.

ASSIGN 
    xSessionNum = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European"
    viiva1 = fill("=",lev)
    viiva2 = fill("=",lev)
    viiva3 = fill("-",lev)
    viiva4 = fill("-",lev).

form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(28)" 
      "Unregistered Payments" AT 32
      "Page" AT 68  
      sl format "ZZZZ9" SKIP
      pvm format "99.99.9999" AT 68 SKIP
   viiva2 AT 1 skip(1)

   WITH width 95 NO-LABEL no-box FRAME sivuotsi.

FUNCTION CheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF rl >= skayt1 - iAddLine THEN DO:
        {uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        view STREAM tul FRAME sivuotsi.  
        ASSIGN rl = 12.

        IF iSpec = TRUE THEN DO:
           PUT STREAM tul UNFORMATTED
              "AccDate" AT 5
              "PaymDate" AT 15
              "CustName" AT 25 
              "Sum"      TO 50
              "Booked"   TO 63
              "Remaining" TO 76
              SKIP
              viiva3 SKIP.
        END.
    END.
    RETURN TRUE.
END.

ASSIGN sl = 1
       rl = 0.

FOR EACH UnregPaym WHERE 
         UnregPaym.Brand   = gcBrand AND
         UnregPaym.AccDate <= iDate  AND
         UnregPaym.State NE 2
no-lock.
   FIND wBal WHERE wBal.Source = UnregPaym.PaymSrc NO-ERROR.
   IF NOT AVAIL wBal THEN DO:
      CREATE wBal.
      ASSIGN
         wBal.Source = UnregPaym.PaymSrc.
   END.

   ASSIGN
      wBal.Amount = wBal.Amount + 1
      wBal.Total = wBal.Total + UnregPaym.PaidAmt.

   IF iSpec = TRUE THEN DO:  
      CREATE wDef.
         ASSIGN 
            wDef.Source   = wBal.Source
            wDef.AccDate  = UnregPaym.AccDate
            wDef.PaymDate = UnregPaym.PaymDate
            wDef.CustName = UnregPaym.CustName
            wDef.Sum      = UnregPaym.PaidAmt
            wDef.Paid     = 0.
   END.

   FOR EACH UnregLog WHERE 
            UnregLog.UrSeq = UnregPaym.UrSeq  AND
            UnregLog.AccDate <= iDate 
   no-lock.
      wBal.Paid = wBal.Paid + UnregLog.Amount.
      IF iSpec = TRUE THEN wDef.Paid  = wDef.Paid + UnregLog.Amount.
   end.

   IF iSpec THEN wDef.Remain = wDef.Sum - wDef.Paid.
   wBal.Remain = wBal.Total - wBal.Paid.

END.

ASSIGN sl = 1. 

VIEW STREAM tul FRAME sivuotsi.

IF iSpec = TRUE THEN DO:
   PUT STREAM tul UNFORMATTED
      "AccDate" AT 5
      "PaymDate" AT 15
      "CustName" AT 25 
      "Sum"      TO 50
      "Booked"   TO 63
      "Remaining" TO 76
      SKIP viiva3 SKIP.
END.

ASSIGN rl = 12.

IF iSpec = TRUE THEN DO:
   FOR EACH wBal NO-LOCK.
      qty = 0.
      CheckPage(5).
      PUT STREAM tul UNFORMATTED
         "Payment source: " wBal.Source SKIP.
      rl = rl + 5.

      FOR EACH wDef WHERE wDef.Source = wBal.Source no-lock.
         qty = qty + 1.
         CheckPage(1).
         PUT STREAM tul UNFORMATTED
            wDef.AccDate  AT 4
            wDef.PaymDate AT 15
            wDef.CustName AT 25 FORMAT "x(15)"
            WDef.Sum      TO 50 FORMAT "->>>,>>9.99"            
            wDef.Paid     TO 63 FORMAT "->>>,>>9.99"
            wDef.Remain   TO 76 FORMAT "->>>,>>9.99"
            SKIP.
         rl = rl + 1.
         ACCUMULATE wDef.Sum (TOTAL).
         ACCUMULATE wDef.Paid (TOTAL).
         ACCUMULATE wDef.Remain (TOTAL).

      END.

      CheckPage(3).

      PUT STREAM tul UNFORMATTED
         viiva3 SKIP
         wBal.Source " TOTAL, " string(qty) " pcs."  
         (ACCUM TOTAL wDef.Sum)    TO 50 FORMAT "->>>,>>9.99"
         (ACCUM TOTAL wDef.Paid)   TO 63 FORMAT "->>>,>>9.99"         
         (ACCUM TOTAL wDef.Remain) TO 76 FORMAT "->>>,>>9.99"         
         SKIP(2).
       ASSIGN 
          rl = rl + 3
          gtotalsum = gtotalsum + (ACCUM TOTAL wDef.Sum)
          gbookedsum = gbookedsum + (ACCUM TOTAL wDef.Paid)
          gremainsum = gremainsum + (ACCUM TOTAL wDef.Remain)
          totqty = totqty + qty.

      ACCUMULATE wBal.Total (TOTAL).
   END.     

   CheckPage(3).
   PUT STREAM tul UNFORMATTED
      viiva1 SKIP
      "GRAND TOTAL, " string(totqty) " pcs."
      gtotalsum  to 50 FORMAT "->>>,>>9.99"
      gbookedsum to 63 FORMAT "->>>,>>9.99"
      gremainsum to 76 FORMAT "->>>,>>9.99"
      SKIP.
   rl = rl + 3.
END.

ELSE DO:
   PUT STREAM tul UNFORMATTED
      "Payment Source"  AT 5
      "Payments"        TO 35
      "Sum"             TO 50
      "Booked"          TO 63
      "Remaining"       TO 76
      SKIP
      viiva3
      SKIP.
   rl = rl + 2.

   FOR EACH wBal no-lock.
      CheckPage(4).
      PUT STREAM tul UNFORMATTED
      wBal.Source AT 5
      wBal.Amount TO 35
      wBal.Total  TO 50 FORMAT "->>>,>>9.99"
      wBal.Paid   TO 63 FORMAT "->>>,>>9.99"
      wBal.Remain TO 76 FORMAT "->>>,>>9.99"
      SKIP.
      ACCUMULATE wBal.Amount (TOTAL).
      ACCUMULATE wBal.Total  (TOTAL).
      ACCUMULATE wBal.Paid   (TOTAL).
      ACCUMULATE wBal.Remain (TOTAL).
      rl = rl + 1.
   END.

   PUT STREAM tul UNFORMATTED
      viiva3 SKIP
      "GRAND TOTAL" AT 5
      (ACCUM TOTAL wBal.Amount) TO 35
      (ACCUM TOTAL wBal.Total)  TO 50 FORMAT "->>>,>>9.99"
      (ACCUM TOTAL wBal.Paid)   TO 63 FORMAT "->>>,>>9.99"
      (ACCUM TOTAL wBal.Remain) TO 76 FORMAT "->>>,>>9.99"
      SKIP.
      rl = rl + 2.

END.

{uprfeed.i rl}

ASSIGN SESSION:NUMERIC-FORMAT = xSessionNum.

