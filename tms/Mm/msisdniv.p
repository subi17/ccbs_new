/* -------------------------------------------------------------------------
  MODULE .......: MSISDNiv.p
  FUNCTION .....: Mobile Subscriber ISDN number invoice VALUE                 
  APPLICATION ..: NN & TN
  CREATED ......: 04.04.2000 jp
  MODIFIED .....: 27.09.2001 jp use InvSeq
                  13.03.2003 tk tokens
                  04.04.2003 tk ttCall.Pricelist no longer exist =>
                                removed vat handling
                  28.05.2003 tk numbers were 100 times too big              
                  10.09.03   jp Brand
  Version ......: M15
  ------------------------------------------------------------------------ */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'Mobcdr'}
{callquery.i}
{fvatfact.i}

DEF INPUT PARAMETER    MsSeq LIKE MobSub.MsSeq  NO-UNDO.

DEF TEMP-TABLE ttCAll NO-UNDO LIKE Mobcdr
   FIELD CDRTable AS CHAR 
   INDEX cli cli datest Timestart  spocmt.
   
DEF VAR alvpros   AS DE NO-UNDO init 22.
DEF VAR lkm1      AS i  NO-UNDO.
DEF VAR lkm2      AS i  NO-UNDO.
def var summa1    as de no-undo format "-z,zzz,zzz.99".
def var summa2    as de no-undo format "-z,zzz,zzz.99".
def var summa1a   as de no-undo format "-z,zzz,zzz.99".
def var summa2a   as de no-undo format "-z,zzz,zzz.99".
def var pvm1      as da no-undo format "99.99.99".
def var pvm2      as da no-undo format "99.99.99".
DEF VAR lccli     AS C  NO-UNDO.

DEF VAR liCustNum  AS INT  NO-UNDO.
DEF VAR lcTaxClass AS CHAR NO-UNDO.
DEF VAR lcTaxZone  AS CHAR NO-UNDO.
DEF VAR ldVatPerc  AS DEC  NO-UNDO.
DEF VAR ldaTaxDate AS DATE NO-UNDO. 

FIND MobSub where MobSub.MsSeq = MsSeq no-lock no-error.


if avail mobsub then ASSIGN
   lccli = mobsub.cli
   liCustNum = MobSub.InvCust.
ELSE  DO:
   find first msowner where 
              msowner.msseq = msseq NO-LOCK NO-ERROR.
   IF avail msowner then ASSIGN
      lccli = msowner.cli
      liCustNum = MsOwner.InvCust.
END.   


form
   skip(1)
"  Note: This program shows the total value of mobile calls"    skip
"        made by this Mobile Subscriber.                   "    skip(1)
"        calls within .......:" pvm1
help "Earliest Date of call" "-" pvm2 
help "Latest Date of call" skip(1)

"    Not Billed " lkm1 "calls, " summa1 "/" summa1a "w/VAT "
SKIP
"    Billed     " lkm2 "calls, " summa2 "/" summa2a "w/VAT " 
skip(1)
WITH
   row 5 col 3 overlay no-labels title " " + ynimi +
   " Total Value of Calls,  "  + string(substr(lcCLI,1,16)) + ") " FRAME rajat.


pvm1 = date(month(TODAY),1,year(TODAY)).
pvm2 = pvm1 + 40.
pvm2 = date(month(pvm2),1,year(pvm2)) - 1.

/* tax handling according to customer */
FIND FIRST Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Invoice customer for subscription was not found"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

lcTaxZone = fRegionTaxZone(Customer.Region).
 
rajat:
repeat WITH FRAME rajat:

   PAUSE 0.
   ehto = 9. RUN ufkey.

   UPDATE pvm1 pvm2
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
   DEFINE VARIABLE liERrorCodeOut AS INT       NO-UNDO.

   tthCDR = TEMP-TABLE ttCall:HANDLE.

   EMPTY TEMP-TABLE ttCall.
    
   fMobCDRCollect(INPUT "post,pre",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT pvm1,
                  INPUT pvm2,
                  INPUT 0,
                  INPUT "",
                  INPUT lcCLI,
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

      ldaTaxDate = TODAY.
      IF InvSeq.Billed = TRUE THEN DO:
         FIND Invoice WHERE
              Invoice.InvNum = InvSeq.InvNum NO-LOCK NO-ERROR.
         IF AVAIL Invoice THEN ldaTaxDate = Invoice.InvDate.
      END.

      ldVatPerc = fVatFactor(Customer.VatUsage,     
                             lcTaxZone,
                             ttCall.BillCode,
                             ldaTaxDate).
       
      IF InvSeq.Billed = TRUE THEN DO:
         lkm2 = lkm2 + 1.
         IF ttCall.VATincl = TRUE THEN ASSIGN
            summa2a = summa2a + ttCall.Amount
            summa2  = summa2 + ttCall.Amount  / ldVatPerc.
         ELSE ASSIGN
            summa2a = summa2a + ttCall.Amount * ldVatPerc
            summa2  = summa2 + ttCall.Amount.
      END.      

      ELSE DO:
         lkm1 = lkm1 + 1.
         
         IF ttCall.VATincl = TRUE THEN ASSIGN
            summa1a = summa1a + ttCall.Amount
            summa1  = summa1 + ttCall.Amount  / ldVatPerc.
         ELSE ASSIGN
            summa1a = summa1a + ttCall.Amount * ldVatPerc
            summa1  = summa1 + ttCall.Amount.
      END.
   END.
   
   PAUSE 0.
   
   DISP lkm1 lkm2 summa1 summa1a summa2 summa2a.

   message "Press ENTER to continue !".
   PAUSE no-message.
   LEAVE rajat.
END.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

EMPTY TEMP-TABLE ttCall.
IF VALID-HANDLE(tthCDR) THEN DELETE OBJECT tthCDR NO-ERROR.


