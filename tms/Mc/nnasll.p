/* ------------------------------------------------------
  MODULE .......: NNASLL.P
  FUNCTION .....: Laajan asiakasluettelon print-line
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 07.02.1997
  MODIFIED .....: 24.02.1997 pt laasno muutettu CustNum:ksi
                  27.02.1997 pt tulost. resk.saldo ja korkovelkak
                  11.03.1997 pt (-) merkki tot2 ja tot1 -kenttiin
                  12.03.1997 tt as-limi[3] nAytetAAn aina + ALV:llisenA
                             ja tot2:sta vAhennetAn myOs limi[4] ja limi[5]
                  04.05.1997 pt 2x2 eri jArjestystA
                  16.10.1997 pt plname, igname
                  22.01.1998 kl ohti into Salesman
                  29.01.1998 kl as-myyja => Salesman
                  31.03.1998 pt InvGroup PriceList Size ConnType
                  07.05.1998 kl resellers
                  13.05.1998 kl PriceList, InvGroup, Reseller -parameters
                  05.08.1998 pt ConnType
                  09.12.1998 pt cday
                  09.01.1999 pt CustGroup
                  28.05.2002 kl RateBSub.BillCode
                  26.09.2002/aam customer balances in CustBal and CustCount,
                                 PriceList and RateCust removed
                  14.11.2002/jr New memo                
                  07.03.2003/aam customer.balance[2] -> CreditLimit
                  12.09.2003/aam brand
                  16.02.2004/aam fdivtxt.i
 Version ......: M15
  ------------------------------------------------------ */

{commali.i}

{utumaa.i}
{cparam2.i}
{fcustbal.i}
{fdivtxt.i}

DEF INPUT PARAMETER CustGroup LIKE CustGroup.CustGroup       NO-UNDO.
DEF INPUT PARAMETER asno1   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER asno2   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER myyja1  AS c                      NO-UNDO.
DEF INPUT PARAMETER myyja2  AS c                      NO-UNDO.
DEF INPUT PARAMETER kateg   LIKE Customer.Category       NO-UNDO.
def input parameter apvm1   as Date format "99-99-99" NO-UNDO.
def input parameter apvm2   as Date format "99-99-99" NO-UNDO.
def input parameter ppvm1   as Date format "99-99-99" NO-UNDO.
def input parameter ppvm2   as Date format "99-99-99" NO-UNDO.
DEF INPUT PARAMETER cday    AS DA                     NO-UNDO.
DEF INPUT PARAMETER ConnType    AS lo                     NO-UNDO.
DEF INPUT PARAMETER InvGroup AS c                      NO-UNDO.
DEF INPUT PARAMETER Reseller AS c                      NO-UNDO.
DEF INPUT PARAMETER order1   AS i                     NO-UNDO.
DEF INPUT PARAMETER order2   AS i                     NO-UNDO.

def var jar1 as c init ",Salesman / "                 NO-UNDO.
def var jar2 as c init "CustNumber,CustName"          NO-UNDO.

DEF BUFFER asi   FOR Customer.

def var ke                as lo format "Yes/No" NO-UNDO.
DEF VAR sl                AS INT.
DEF VAR rl                AS INT.
DEF VAR rlx               AS INT.
DEF VAR lev               AS INT init 78.

DEF VAR dkatnimi          AS c  NO-UNDO.
def var tot1              as de no-undo format "zzz,zzz,zz9-".
def var tot2              as de no-undo format "zzz,zzz,zz9-".
DEF VAR krmin             AS DE NO-UNDO.
def var aslanimi          as char format "x(22)" NO-UNDO.
def var asranimi          as char format "x(22)" NO-UNDO.
def var asrenimi          as char format "x(22)" NO-UNDO.
def var mynimi            as char format "x(12)" NO-UNDO.
def var rsname            as char format "x(12)" NO-UNDO.
def var plname            as char format "x(22)" NO-UNDO.
def var igname            as char format "x(22)" NO-UNDO.
DEF VAR i                 AS i                   NO-UNDO.
def var kaytalv as dec format "z,zzz,zz9-"       NO-UNDO.
DEF VAR fake              AS DA                  NO-UNDO EXTENT 4.
DEF VAR callcheck         AS c                   NO-UNDO.
def var hdr0              as c    format "x(89)" NO-UNDO.
def var hdr1              as c    format "x(89)" NO-UNDO.
def var hdr2              as c    format "x(89)" NO-UNDO.
def var hdr3              as c    format "x(89)" NO-UNDO.

DEF VAR ldCustAP          AS DEC                 NO-UNDO. 
DEF VAR ldCustOP          AS DEC                 NO-UNDO. 
DEF VAR ldCustBal         AS DEC                 NO-UNDO.
DEF VAR ldCustInt         AS DEC                 NO-UNDO. 
DEF VAR xGenText          AS CHAR                NO-UNDO.
DEF VAR lii               AS INT                 NO-UNDO.

IF apvm1 < 1/1/1990 THEN fake[1] = ?. ELSE fake[1] = apvm1.
IF apvm2 > 1/1/9999 THEN fake[2] = ?. ELSE fake[2] = apvm2.
IF ppvm1 < 1/1/1990 THEN fake[3] = ?. ELSE fake[3] = ppvm1.
IF ppvm2 > 1/1/9999 THEN fake[4] = ?. ELSE fake[4] = ppvm2.

if cday = ? then callcheck = "NO Calls CHECKED".
else             callcheck = "NO Calls SINCE " + string(cday,"99.99.99").

form header
   fill("=",90) format "x(90)" SKIP
   ynimi "LARGE CUSTOMER LIST" at 33 "Page" at 83 sl format "ZZZ9" TO 90 SKIP

   "SORTED BY " + entry(order1,jar1) + entry(order2,jar2)  at 33 format "x(36)"
   string(pvm,"99-99-99") TO 90 SKIP
   fill("-",90) format "x(90)" SKIP
   hdr0 AT 2 SKIP
   hdr1 AT 2 SKIP
   hdr2 AT 2 SKIP
   hdr3 AT 2 SKIP

   fill("=",90) format "x(90)" SKIP

WITH
   width 90 NO-LABEL no-box FRAME sivuots.

form
   {nnasse.f}
WITH no-box side-labels FRAME perus.

ASSIGN sl = 0 rl = 0.

message "Printing in process, cancel: press 'END'".

print-line:
FOR
    EACH Customer no-lock            where
         Customer.Brand     = gcBrand AND
        (if CustGroup ne "" THEN can-find(CGMember where
                                        CGMember.Brand     = gcBrand AND
                                        CGMember.CustGroup = CustGroup  AND
                                        CGMember.CustNum  = Customer.CustNum)
                          ELSE TRUE)                                       AND

         Customer.CustNum  >= asno1  AND
         Customer.CustNum  <= asno2  AND
         Customer.Salesman >= myyja1 AND
         Customer.Salesman <= myyja2 AND
        (if kateg   ne ""         
         THEN Customer.Category   = kateg   ELSE TRUE) AND
        (IF apvm1   NE 1/1/1900   
         THEN Customer.ContrBeg >= apvm1   ELSE TRUE) AND
        (IF apvm2   NE 12/31/9999 
         THEN Customer.ContrBeg <= apvm2   ELSE TRUE) AND
        (IF ppvm1   NE 1/1/1900   
         THEN Customer.ContrEnd >= ppvm1   ELSE TRUE) AND
        (IF ppvm2   NE 12/31/9999 
         THEN Customer.ContrEnd <= ppvm2   ELSE TRUE) AND
        (IF ConnType    NE ?          
         THEN Customer.ConnType     = ConnType    ELSE TRUE) AND
        (if InvGroup ne ""        
         then Customer.InvGroup  = InvGroup else true) and         
        (if Reseller ne ""         
         THEN Customer.Reseller  = Reseller ELSE TRUE) AND

        /* DO we want TO FIND ONLY passive customers ? */
        (IF cday NE ? THEN NOT can-find(FIRST FixCDR where
                                          FixCDR.InvCust = Customer.CustNum AND
                                          FixCDR.Date >= cday) ELSE TRUE)

     by (if order1 = 1 then "-"
                     else string(Customer.Salesman,"999") )
     by (if order2 = 1 then string(Customer.CustNum,"9999999")
                     ELSE Customer.CustName ):

   /* onko kjA pyytAnyt keskeytystA ? */
   READKEY PAUSE 0.
   nap = keylabel(LASTKEY).
   if nap = "END" THEN DO:
      message "Do You really want to cancel (Y/N) ?"
      UPDATE ke.
      IF ke THEN DO:
         display stream tul "Printout cancelled !" WITH NO-LABEL no-box.
         rl = rl + 1.
         LEAVE print-line.
      END.
   END.

   FIND FIRST Salesman where
              Salesman.Brand    = gcBrand AND
              Salesman.Salesman = Customer.Salesman no-lock no-error.
   IF AVAIL Salesman THEN mynimi = Salesman.SmName.
   else                   mynimi = "! UNKNOWN !".

   FIND FIRST Reseller where
              Reseller.Brand    = gcBrand AND
              Reseller.Reseller = Customer.Reseller no-lock no-error.
   IF AVAIL Reseller THEN rsname = Reseller.RsName.
   else                 rsname = "! UNKNOWN !".

   FIND FIRST asi where asi.CustNum = Customer.InvCust no-lock no-error.
   IF AVAIL asi THEN aslanimi = asi.CustName.

   FIND FIRST asi where asi.CustNum = Customer.PaymCust no-lock no-error.
   IF AVAIL asi THEN asrenimi = asi.CustName.

   FIND FIRST asi where asi.CustNum = Customer.RepCust no-lock no-error.
   IF AVAIL asi THEN asranimi = asi.CustName.

   ASSIGN ldCustAP  = fGetCustBal(Customer.CustNum,"TOTAL","AP")
          ldCustOP  = fGetCustBal(Customer.CustNum,"TOTAL","OP") 
          ldCustBal = fGetCustBal(Customer.CustNum,"TOTAL","ARBAL")
          ldCustInt = fGetCustBal(Customer.CustNum,"TOTAL","INT"). 
   FIND CustCount OF Customer NO-LOCK NO-ERROR. 

   IF AVAILABLE CustCount THEN 
      kaytalv = CustCount.Unbilled.
   ELSE kaytalv = 0.

   ASSIGN tot1 = ldCustAP + ldCustOP + Customer.CreditLimit
          tot2 = tot1 - kaytalv - ldCustBal - ldCustInt.

   dkatnimi = "".
   FIND FIRST CustCat where 
              CustCat.Brand    = gcBrand AND
              CustCat.Category = Customer.Category 
      no-lock no-error.
   IF AVAIL CustCat THEN ASSIGN dkatnimi = CustCat.CatName.

   igname = "".
   FIND FIRST InvGroup of Customer.
   IF AVAIL InvGroup THEN igname = InvGroup.IGName.

   /* 1 customer / page */

   /* show criteria */

   hdr0 =  "".
   if CustGroup ne "" THEN DO:
      FIND CustGroup where 
           CustGroup.Brand     = gcBrand AND
           CustGroup.CustGroup = CustGroup no-lock.
      hdr0 = "Customers in an External Group '" + CustGroup + "': " +
      CustGroup.CGName.
   END.
   hdr1 =  "CustNos.: "   + string(asno1)  + " - " + string(asno2)  +
           ", Salesmen: " + string(myyja1) + " - " + string(myyja2) +
           ", Category: " + string(kateg) + ", " + callcheck.

   hdr2 =  "Contr. begun: " + string(apvm1,"99.99.9999") + " - " +
           string(apvm2,"99.99.9999") +
           ", Contr. ended:" + string(ppvm1,"99.99.9999") + " - " +
           string(ppvm2,"99.99.9999").

   hdr3 =  "Inv.Group: " + InvGroup +
           ", Reseller: " + Reseller + ", ConnType. ".
           if ConnType ne ? then   hdr3 = hdr3 + string(ConnType,"D/I").
           else                hdr3 = hdr3 + "Both Dir and Indir".

   sl = 1.
   view STREAM tul FRAME sivuots.

   /* basic data of a customer */
   DISPLAY STREAM tul
     aslanimi asrenimi asranimi mynimi rsname tot1 tot2
     dkatnimi kaytalv plname igname Customer.Size Customer.ConnType
     Customer.CustNum Customer.OrgId
     Customer.SearchName Customer.CustName Customer.COName Customer.Contact
     Customer.Phone  Customer.Fax Customer.Email
     Customer.Address Customer.ZipCode Customer.PostOffice Customer.Country
     Customer.InvCust Customer.RepCust Customer.PaymCust
     Customer.Salesman Customer.Reseller Customer.RepCodes Customer.Category
     ldCustAP Customer.CreditLimit ldCustBal ldCustInt
     Customer.PaymTerm Customer.Language Customer.ContrBeg Customer.ContrEnd
     Customer.InvGroup 
   WITH FRAME perus.

   put stream tul fill ("-",78) format "x(78)" SKIP.
   rl = 27.

   /* is there a memo text ? */
   IF CAN-FIND(FIRST memo WHERE
                     memo.Brand     = gcBrand AND
                     memo.HostTable = "Customer" AND
                     memo.KeyValue  = STRING(Customer.CustNum) AND
                     memo.memotext NE "") THEN
   DO:
      FOR EACH memo WHERE
               memo.Brand     = gcBrand AND
               memo.HostTable = "Customer" AND
               memo.KeyValue  = STRING(Customer.CustNum) AND
               memo.memotext NE "" NO-LOCK:

         xGenText = fseparateinvotxt(memo.memotext,
                                     60).

         DO lii = 1 TO NUM-ENTRIES(xGenText,CHR(9)):
            PUT STREAM tul UNFORMATTED 
            ENTRY(lii,xGenText,CHR(9)) FORMAT "X(60)" AT 5 SKIP.
            rl = rl + 1.
         END.
      END.
      PUT STREAM tul FILL("-",78) FORMAT "x(78)" SKIP.
      rl = rl + 1.
   END.

   /* THEN ALL A-sub nos. */
   put stream tul "Customer's all A-sub nos.:" skip(1). rl = rl + 2.

   FOR EACH CLISer no-lock where
            CLISer.CustNum = Customer.CustNum:

      /* NEW page ? */
      IF rl >= skayt1 THEN DO:
         PUT STREAM tul UNFORMATTED chr(12).
         ASSIGN rlx = 0 sl = sl + 1 rl = 9.
         view STREAM tul FRAME sivuots.
      END.

      PUT STREAM tul
          CLISer.CLIFrom  format "x(10)"
          " - "
          CLISer.CLITo  format "x(10)"
          space(4)
          "Totally"
          (decimal(CLISer.CLITo) - decimal(CLISer.CLIFrom) + 1)
          format "zzzzz9"
          SKIP.
          rl = rl + 1.
   END.

   IF can-find(FIRST Invoice where Invoice.CustNum = Customer.CustNum) THEN DO:

      PUT STREAM tul
      fill ("-",78) format "x(78)" SKIP
      "Customer's 1 - 5 latest INVOICES:" skip(1).
      rl = rl + 3.
      i = 0.
      FOR EACH Invoice no-lock where
               Invoice.CustNum = Customer.CustNum:

          /* NEW page ? */
          IF rl >= skayt1 THEN DO:
             PUT STREAM tul UNFORMATTED chr(12).
             ASSIGN rlx = 0 sl = sl + 1 rl = 9.
             view STREAM tul FRAME sivuots.
          END.

          PUT STREAM tul
          "No "
          Invoice.InvNum     format "zzzzzzz9" space(2)
          "Printed "
          Invoice.InvDate     format "99-99-9999" space(2)
          "Amount: "
          Invoice.InvAmt  format "z,zzz,zz9.99-" 
          "Dueday: "
          Invoice.DueDate    format "99-99-9999"
          SKIP.
          rl = rl + 1.
          i = i + 1.
          IF i = 5 THEN LEAVE.
      END.
   END.

   /* NEXT customer */
   PUT STREAM tul UNFORMATTED chr(12).

END. /* print-line (FOR EACH) */

/* align LAST page */
PUT STREAM tul UNFORMATTED chr(12).

