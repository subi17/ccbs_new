/* -------------------------------------------------------------------------
  MODULE .......: callvalueic.p
  FUNCTION .....: invoice customer's calls value 
  APPLICATION ..: TMS
  CREATED ......: 30.01.06/aam (from nnmlaen2)
  MODIFIED .....: 
  Version ......: M15
  ------------------------------------------------------------------------ */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobcdr'}
{Func/fvatfact.i}
{Func/callquery.i}


DEF  INPUT PARAMETER iiInvCust AS INT NO-UNDO.

DEF VAR lkm1        AS INT  NO-UNDO.
DEF VAR lkm2        AS INT  NO-UNDO.
def var summa1      AS DEC  NO-UNDO format "-z,zzz,zzz.99".
def var summa2      AS DEC  NO-UNDO format "-z,zzz,zzz.99".
def var summa1a     AS DEC  NO-UNDO format "-z,zzz,zzz.99".
def var summa2a     AS DEC  NO-UNDO format "-z,zzz,zzz.99".
def var pvm1        AS DATE NO-UNDO format "99.99.99".
def var pvm2        AS DATE NO-UNDO format "99.99.99".
DEF VAR ldVatFactor AS DEC  NO-UNDO. 
DEF VAR lcTaxZone   AS CHAR NO-UNDO. 
DEF VAR ldaTaxDate AS DATE NO-UNDO. 

DEF TEMP-TABLE ttCAll NO-UNDO LIKE Mobcdr 
   FIELD CDRTable AS CHAR 
   INDEX cli cli datest Timestart  spocmt.
 
DEF BUFFER bInvCust FOR Customer.


form
   SKIP(1)
"  Note: This program shows the total value of mobile calls"    SKIP
"        being made from this invoice customer's users."        SKIP(1)
"        calls within .......:" 
   pvm1
      help "Earliest Date of call" 
   "-" 
   pvm2 
      help "Latest Date of call" SKIP(1)
"    Not Billed " lkm1 "calls, EUR" summa1 "/" summa1a "w/VAT " SKIP
"    Billed     " lkm2 "calls, EUR" summa2 "/" summa2a "w/VAT " SKIP(1)
WITH
   row 5 col 3 overlay no-labels title " " + ynimi +
   " Total value of calls, inv.cust. " + string(Customer.CustNum) + " " 
   FRAME rajat.


FIND Customer WHERE Customer.CustNum = iiInvCust NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Unknown customer"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lcTaxZone = fRegionTaxZone(Customer.Region).

ASSIGN pvm1 = date(month(TODAY),1,year(TODAY))
       pvm2 = pvm1 + 40
       pvm2 = date(month(pvm2),1,year(pvm2)) - 1.

PAUSE 0.
VIEW FRAME rajat. 

rajat:
repeat WITH FRAME rajat:

   ehto = 9. 
   RUN ufkey.

   UPDATE pvm1 
          pvm2
             validate (input pvm2 >= input pvm1,"Incorrect order !").

   toimi:
   repeat WITH FRAME toimi:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
   END.

   ASSIGN lkm1 = 0 lkm2 = 0 summa1 = 0 summa2 = 0.
   
   message "Calculating ...".                   

   DEFINE VARIABLE tthCDR         AS HANDLE    NO-UNDO.
   DEFINE VARIABLE liErrorCodeOut AS INT       NO-UNDO.

   tthCDR = TEMP-TABLE ttCall:HANDLE.

   EMPTY TEMP-TABLE ttCall.
    
   fMobCDRCollect(INPUT "post",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT pvm1,
                  INPUT pvm2,
                  INPUT iiInvCust,
                  INPUT "inv",
                  INPUT "",
                  INPUT 0,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liErrorCodeOut,
                  INPUT-OUTPUT tthCDR).
   
   FOR EACH ttCall    no-lock,  
      FIRST  InvSeq NO-LOCK WHERE 
             InvSeq.InvSeq = ttCall.InvSeq .

        IF ttCall.ErrorCode > 0 THEN NEXT. 

        /* vat */
        ldaTaxDate = TODAY.
        IF InvSeq.Billed = TRUE THEN DO:
           FIND Invoice WHERE
                Invoice.Invnum = InvSeq.Invnum NO-LOCK NO-ERROR.
           IF AVAIL Invoice THEN ldaTaxDate = Invoice.InvDate.
        END.
        ldVatFactor = fVatFactor(Customer.VatUsage,     
                                 lcTaxZone,
                                 ttCall.BillCode,
                                 ldaTaxDate).
 
        if invseq.billed = TRUE then do:
           lkm2 = lkm2 + 1.
           if ttCall.VatIncl then assign
              summa2  = summa2  + ttCall.Amount / ldVatFactor
              summa2a = summa2a + ttCall.Amount.
           else assign 
              summa2  = summa2  + ttCall.Amount
              summa2a = summa2a + ttCall.Amount * ldVatFactor.
        end.
           
        else if invseq.billed = false then do:
           lkm1 = lkm1 + 1.
           if ttCall.VatIncl then assign
              summa1  = summa1  + ttCall.Amount / ldVatFactor
              summa1a = summa1a + ttCall.Amount.
           else assign 
              summa1  = summa1  + ttCall.Amount
              summa1a = summa1a + ttCall.Amount * ldVatFactor.
        end.

   END.
   PAUSE 0.

   DISP lkm1 lkm2 summa1   summa1a   summa2   summa2a .

   PAUSE MESSAGE "Press ENTER to continue !".
   
   LEAVE rajat.
END.

HIDE MESSAGE NO-PAUSE.
HIDE FRAME rajat NO-PAUSE.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.











