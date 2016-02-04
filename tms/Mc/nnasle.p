/* --------------------------------------------------------------------------
  MODULE .......: NNASLE.P
  FUNCTION .....: Print large customer list into a TAB separated ASCII File
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 07.02.1997
  changeD ......: 04.05.1997 pt 2x2 eri jArjestystA
                  29.01.1998 kl as-myyja => Salesman
                  10.02.1998 kl hi-perus => hi-stchrg[i]
                  12.05.1998 kl Reseller
                  13.05.1998 kl PriceList, InvGroup, Reseller -parameters
                  05.08.1998 pt ConnType
                  09.12.1998 pt cday
                  09.01.1999 pt CustGroup
                  20.06.2000 kl Customer.
                  28.05.2002 kl RateBSub.BillCode
                  26.09.2002/aam customer balances in CustBal and CustCount
                  14.11.2002/jr  New Memo
                  07.03.2003/aam customer.balance[2] -> CreditLimit
                  12.09.2003/aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

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
DEF INPUT PARAMETER exPaymFile  AS c                      NO-UNDO.
DEF INPUT PARAMETER order1   AS i                      NO-UNDO.
DEF INPUT PARAMETER order2   AS i                      NO-UNDO.

DEF VAR i          AS i  NO-UNDO.
DEF VAR tab        AS c  NO-UNDO.
DEF VAR cx         AS c  NO-UNDO.
DEF VAR x          AS DE NO-UNDO.
DEF VAR x1         AS DE NO-UNDO.
DEF VAR x2         AS DE NO-UNDO.
DEF VAR krmin      AS DE NO-UNDO.
DEF VAR fake       AS DA NO-UNDO EXTENT 4.
DEF VAR callcheck  AS c  NO-UNDO.

tab    = chr(9).

DEF NEW shared STREAM excel.
OUTPUT STREAM excel TO value(exPaymFile) page-size 0.

message "Printing going on ...".

IF apvm1 < 1/1/1990 THEN fake[1] = ?. ELSE fake[1] = apvm1.
IF apvm2 > 1/1/9999 THEN fake[2] = ?. ELSE fake[2] = apvm2.
IF ppvm1 < 1/1/1990 THEN fake[3] = ?. ELSE fake[3] = ppvm1.
IF ppvm2 > 1/1/9999 THEN fake[4] = ?. ELSE fake[4] = ppvm2.

if cday = ? then callcheck = "NO Calls CHECKED".
else             callcheck = "NO Calls SINCE " + string(cday,"99.99.99").


PUT STREAM excel UNFORMATTED
   "Ext. CustGroup:" tab (if CustGroup ne "" then CustGroup else "NONE").
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED
   "Cust. nos. :" tab asno1 " - " asno2.
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED
   "Salesman:" tab myyja1 " - " myyja2.
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED
   "Category:" tab kateg.
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED
   "Contract begun:" tab.
IF fake[1] NE ? THEN PUT STREAM excel UNFORMATTED fake[1].
put stream excel unformatted " - ".
IF fake[2] NE ? THEN PUT STREAM excel UNFORMATTED fake[2].
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED
   "Contract ended:".
IF fake[3] NE ? THEN PUT STREAM excel UNFORMATTED fake[3].
   put stream excel unformatted " - ".
IF fake[4] NE ? THEN PUT STREAM excel UNFORMATTED fake[4].
RUN Syst/uexskip(1).
put stream excel unformatted "Connection:" tab.
if ConnType = ? then put stream excel "ALL".
else put stream excel ConnType format "Direct/Indirect".
RUN Syst/uexskip(1).
put stream excel unformatted "Inv. Group:" tab InvGroup.
RUN Syst/uexskip(1).
PUT STREAM excel UNFORMATTED callcheck.
RUN Syst/uexskip(1).


FOR
   EACH  Customer no-lock  where
        (if CustGroup ne "" THEN can-find(CGMember where
                                        CGMember.Brand    = gcBrand AND
                                        CGMember.CustGroup = CustGroup  AND
                                        CGMember.CustNum  = Customer.CustNum)
                          ELSE TRUE)                                         AND
         Customer.CustNum  >= asno1 AND
         Customer.CustNum  <= asno2 AND
        (if kateg ne ""         THEN Customer.Category = kateg     ELSE TRUE) AND
        (IF apvm1 NE 1/1/1900   THEN Customer.ContrBeg >= apvm1   ELSE TRUE) AND
        (IF apvm2 NE 12/31/9999 THEN Customer.ContrBeg <= apvm2   ELSE TRUE) AND
        (IF ppvm1 NE 1/1/1900   THEN Customer.ContrEnd >= ppvm1   ELSE TRUE) AND
        (IF ppvm2 NE 12/31/9999 THEN Customer.ContrEnd <= ppvm2   ELSE TRUE) AND
        (if InvGroup ne ""       THEN Customer.InvGroup  = InvGroup ELSE TRUE) AND
        (if Reseller ne ""       THEN Customer.Reseller  = Reseller ELSE TRUE) AND
        (if myyja1  ne ""       THEN Customer.Salesman >= myyja1  ELSE TRUE) AND
        (if myyja2  ne ""       THEN Customer.Salesman <= myyja2  ELSE TRUE) AND
        (IF ConnType    NE ?        THEN Customer.ConnType     = ConnType    ELSE TRUE) AND
        /* DO we want TO FIND ONLY passive customers ? */
        (IF cday NE ? THEN NOT can-find(FIRST FixCDR where
                                          FixCDR.InvCust = Customer.CustNum AND
                                          FixCDR.Date >= cday) ELSE TRUE)

   by (if order1 = 1 then "-"
                 ELSE Customer.Salesman)
   by (if order2 = 1 then string(Customer.CustNum,"9999999")
                 ELSE Customer.CustName ):

   PUT SCREEN ROW 23 col 40 string(Customer.CustNum).

   FIND CustBal OF Customer NO-LOCK NO-ERROR.
   FIND CustCount OF Customer NO-LOCK NO-ERROR.

   PUT STREAM excel UNFORMATTED

      Customer.CustNum      tab
      Customer.SearchName     tab
      Customer.InvCust     tab
      Customer.PaymCust     tab
      Customer.RepCust    tab
      Customer.CustName     tab
      Customer.Contact    tab
      Customer.Phone      tab
      Customer.Fax      tab
      Customer.Email    tab
      Customer.COName      tab
      Customer.Address      tab
      Customer.ZipCode    tab
      Customer.PostOffice      tab
      Customer.Country      tab
      Customer.Salesman     tab
      Customer.Reseller     tab
      Customer.RepCodes    tab
      Customer.CreditLimit  tab
      (IF AVAILABLE CustCount 
       THEN CustCount.Unbilled
       ELSE 0)             TAB
      (IF AVAILABLE CustBal
       THEN CustBal.Debt
       ELSE 0)             TAB
      (IF AVAILABLE CustBal
       THEN CustBal.Interest
       ELSE 0)             TAB
      Customer.Language     tab
      Customer.PaymTerm    tab
      Customer.OrgId      tab
      Customer.Category      tab
      Customer.ContrBeg     tab
      Customer.ContrEnd     tab
      Customer.ClaimPerm       tab
      Customer.InterestPerm       tab
      Customer.CashDiscDate     tab
      Customer.CashDiscPerc      tab
      Customer.ConnType        tab.

   /* memo */
   IF CAN-FIND(FIRST memo WHERE
               memo.Brand     = gcBrand AND
               memo.HostTable = "Customer" AND
               memo.KeyValue  = STRING(Customer.CustNum) AND
               memo.memotext NE "") THEN
   DO:
      PUT STREAM excel UNFORMATTED "$memo:".
      FOR EACH memo WHERE
               memo.Brand     = gcBrand AND
               memo.HostTable = "Customer" AND
               memo.KeyValue  = STRING(Customer.CustNum) AND
               memo.memotext NE "" NO-LOCK:

         PUT STREAM excel UNFORMATTED memo.memotext tab.
      END.   
   END.   

   put stream excel unformatted "$A-sub. nos:" tab.
   /* A-nr */
   FOR EACH CLISer no-lock where
            CLISer.CustNum = Customer.CustNum:
      ASSIGN
      x1 = decimal(CLIFrom)
      x2 = decimal(CLITo).

      /* muodostetaan yksittAiset numerot etunollineen */
      DO x = x1 TO x2 TRANS.
         cx = string(x,fill("9",length(CLIFrom))).
         PUT STREAM excel UNFORMATTED cx tab.
      END.
   END.


   /* fakturor */
   IF can-find(FIRST Invoice where Invoice.CustNum = Customer.CustNum) THEN DO:

      put stream excel "$Invoices:" tab.
      FOR EACH Invoice of Customer no-lock:

          put stream excel "$No:" tab

          Invoice.InvNum     format "zzzzzzz9"      tab
          Invoice.InvDate     format "99.99.9999"    tab
          Invoice.InvAmt  format "z,zzz,zz9.99-" tab
          Invoice.DueDate    format "99.99.9999"    tab.
      END.
   END.

   if opsys = "unix" THEN PUT STREAM excel UNFORMATTED chr(13) chr(10).

   else if opsys = "msdos" THEN
   PUT STREAM excel SKIP.
END.

OUTPUT STREAM excel CLOSE.
PAUSE 0 no-message.
message "Printing ready, created File is:'" + exPaymFile + "' - press ENTER !".
PAUSE no-message.

