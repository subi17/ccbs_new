/* --------------------------------------------------------------------------
  MODULE .......: NNASLs.P
  FUNCTION .....: print SMALL customer list either TO paper OR File
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 04.05.1997
  MODIFIED  ....: 04.05.1997
                  02.01.1998 kl <> ohti into Salesman
                  29.01.1998 kl <> as-myyja => Salesman
                  06.04.1998 kl <> one Salesman => SmName
                  12.05.1998 kl <> added Reseller & rsname
                  13.05.1998 kl PriceList, InvGroup, Reseller -parameters
                  05.08.1998 pt ConnType
                  02.11.1998 pt pr-code into excel OUTPUT File (2nd col.)
                  09.12.1998 pt cday
                  09.01.1999 pt CustGroup
                  29.05.2002 aam FORMAT FOR CustNum
                  26.09.2002 aam PriceList removed 
                  12.09.2003 aam brand
  Version ......: M15
  ------------------------------------------------------------------------ */

{Syst/commali.i}

{Syst/utumaa.i}

DEF INPUT PARAMETER CustGroup LIKE CustGroup.CustGroup       NO-UNDO.
DEF INPUT PARAMETER asno1   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER asno2   LIKE Customer.CustNum       NO-UNDO.
DEF INPUT PARAMETER myyja1  LIKE Customer.Salesman      NO-UNDO.
DEF INPUT PARAMETER myyja2  LIKE Customer.Salesman      NO-UNDO.
DEF INPUT PARAMETER kateg   LIKE Customer.Category       NO-UNDO.
def input parameter aday1   as Date format "99-99-99" NO-UNDO.
def input parameter aday2   as Date format "99-99-99" NO-UNDO.
def input parameter pday1   as Date format "99-99-99" NO-UNDO.
def input parameter pday2   as Date format "99-99-99" NO-UNDO.
DEF INPUT PARAMETER cday    AS Date                   NO-UNDO.
DEF INPUT PARAMETER ConnType    AS lo                     NO-UNDO.
DEF INPUT PARAMETER InvGroup AS c                      NO-UNDO.
DEF INPUT PARAMETER Reseller AS c                      NO-UNDO.
DEF INPUT PARAMETER order1   AS i                     NO-UNDO.
DEF INPUT PARAMETER order2   AS i                     NO-UNDO.
DEF INPUT PARAMETER excel   AS lo                     NO-UNDO.

DEF INPUT PARAMETER exPaymFile  AS c                      NO-UNDO.

DEF NEW shared STREAM excel.

DEF VAR x-cg-code LIKE CustGroup.CustGroup                 NO-UNDO.
def var jar1    as c init ",Salesman By "             NO-UNDO.
def var jar2    as c init "Cust.no,Cust. Name"        NO-UNDO.
DEF VAR tab     AS c                                  NO-UNDO.
def var viiva1  as char format "x(169)".
DEF VAR viiva2  LIKE viiva1.
DEF VAR viiva3  LIKE viiva1.
DEF VAR sl      AS INT.
DEF VAR rl      AS INT.
DEF VAR rlx     AS INT.
DEF VAR lev     AS INT init 169.
def var ke      as log format "Yes/No"   init "No".
DEF VAR edmyyja AS c   NO-UNDO.
DEF VAR mynimi  AS c   NO-UNDO.
DEF VAR rsname  AS c   NO-UNDO.
DEF VAR fake    AS DA  NO-UNDO EXTENT 4.
DEF VAR pr-code AS c   NO-UNDO.
DEF VAR callcheck AS c NO-UNDO.

ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)
tab    = chr(9).

IF aday1 < 1/1/1990 THEN fake[1] = ?. ELSE fake[1] = aday1.
IF aday2 > 1/1/9999 THEN fake[2] = ?. ELSE fake[2] = aday2.
IF pday1 < 1/1/1990 THEN fake[3] = ?. ELSE fake[3] = pday1.
IF pday2 > 1/1/9999 THEN fake[4] = ?. ELSE fake[4] = pday2.

if cday = ? then callcheck = "NO Calls CHECKED".
else             callcheck = "NO Calls SINCE " + string(cday,"99.99.99").
x-cg-code = (if CustGroup ne "" then CustGroup else "NONE").


form header
   viiva1  AT 2 SKIP
   ynimi at 2 "SMALL CUSTOMER LIST" at 64 "Page" AT 161
   sl format "ZZZZ9" TO 170
   SKIP
   "By " + entry(order1,jar1) + entry(order2,jar2)
   at 64 format "x(36)"
   string(pvm,"99-99-99") TO 170 SKIP

   /* show criterias */
   "External Customer Group:" AT 6 x-cg-code SKIP
   "Cust#:" at 6  asno1 "-" asno2 "Salesman:" at 35 myyja1 "-" myyja2
   "Category:"  AT 65 kateg
   "Contr. begun:" at 84  fake[1] format "99-99-99"  when fake[1] NE ?
   "-" fake[2] format "99-99-99"  when fake[2] NE ?
   "Contr. ended:" at 121 fake[3] format "99-99-99"  when fake[1] NE ?
   "-" fake[4] format "99-99-99"  when fake[2] NE ? SKIP

   "Inv.Group:" AT 35 InvGroup
   "Reseller:" at 65 Reseller "Connection:" at 84 ConnType format "Direct/Indirect"
   callcheck format "x(23)"
   SKIP

   SKIP
   viiva2               AT 2 skip(1)
   "CustNo"             TO 8
   "Customer's Name"    AT 10
   "Customer's Address" AT 41
   "Post address"       AT 72
   "Contact"            AT 106
   "Ctr from"           AT 137
   "Ctr end"            AT 146
   "Telephone"          AT 155 SKIP
   viiva3               AT 2 SKIP
WITH width 170 NO-LABEL no-box FRAME sivuots.

form
   Customer.CustNum   format ">>>>>>>9" TO 8
   Customer.CustName  format "x(30)"    AT 10
   Customer.Address   format "x(30)"    AT 41
   Customer.ZipCode format "x(8)"     AT 72
   Customer.PostOffice   format "x(24)"    AT 81
   Customer.Contact format "x(30)"    AT 106
   Customer.ContrBeg  format "99-99-99" AT 137
   Customer.ContrEnd  format "99-99-99" AT 146
   Customer.ConnType     format "D/I"      AT 155
   Customer.Phone   format "x(16)"    AT 157 SKIP
WITH width 175 NO-LABELS no-box FRAME asline.

ASSIGN
sl = 0
rl = skayt1.

message "Printing ...".
IF excel THEN DO:
   OUTPUT STREAM excel TO value(exPaymFile) page-size 0.
   PUT STREAM excel UNFORMATTED
   "External CustGroup:" tab (if CustGroup ne "" then CustGroup else "NONE").
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Customers:" tab asno1 " - " asno2.
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Salesman:" tab myyja1 " - " myyja2.
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Category:" tab kateg.
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Contr. begun  :" tab.
   IF fake[1] NE ? THEN PUT STREAM excel UNFORMATTED fake[1].
   put stream excel unformatted " - ".
   IF fake[2] NE ? THEN PUT STREAM excel UNFORMATTED fake[2].
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Contr. ended  :".
   IF fake[3] NE ? THEN PUT STREAM excel UNFORMATTED fake[3].
   put stream excel unformatted " - ".
   IF fake[4] NE ? THEN PUT STREAM excel UNFORMATTED fake[4].
   RUN uexskip(1).
   put stream excel unformatted "Connection:" tab.
   if ConnType = ? then put stream excel unformatted "ALL".  ELSE
   put stream excel ConnType format "Direct/Indirect".
   RUN uexskip(1).
   PUT STREAM excel UNFORMATTED
   "Fakt. grupp" tab InvGroup.
   RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
   "Reseller:" tab Reseller.
   RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
   callcheck.
   RUN uexskip(1).

   PUT STREAM excel UNFORMATTED
   "CustNo"          tab
   "Project"         tab
   "SmanC"           tab
   "SmanName"        tab
   "RSCode"          tab
   "RSName"          tab
   "Customer's Name" tab
   "Customer's address" tab
   "PostNo"          tab
   "City"            tab
   "Contact"         tab
   "CtrFrom"         tab
   "CtrTill"         tab
   "Connection"      tab
   "Size"            tab
   "Telephone"       tab
   "Email"           .
   RUN uexskip(1).

END.
print-line:
FOR
    EACH Customer no-lock            where
        (if CustGroup ne "" THEN can-find(CGMember where
                                        CGMember.Brand    = gcBrand AND
                                        CGMember.CustGroup = CustGroup  AND
                                        CGMember.CustNum  = Customer.CustNum)
                          ELSE TRUE)                                         AND
         Customer.CustNum  >= asno1  AND
         Customer.CustNum  <= asno2  AND
         Customer.Salesman >= myyja1 AND
         Customer.Salesman <= myyja2 AND
        (if kateg ne ""         THEN Customer.Category   = kateg   ELSE TRUE) AND
        (IF aday1 NE 1/1/1900   THEN Customer.ContrBeg >= aday1   ELSE TRUE) AND
        (IF aday2 NE 12/31/9999 THEN Customer.ContrBeg <= aday2   ELSE TRUE) AND
        (IF pday1 NE 1/1/1900   THEN Customer.ContrEnd >= pday1   ELSE TRUE) AND
        (IF pday2 NE 12/31/9999 THEN Customer.ContrEnd <= pday2   ELSE TRUE) AND
        (IF ConnType    NE ?        THEN Customer.ConnType     = ConnType    ELSE TRUE) AND
        (if InvGroup ne ""       THEN Customer.InvGroup  = InvGroup ELSE TRUE) AND
        (if Reseller ne ""       THEN Customer.Reseller  = Reseller ELSE TRUE) AND
        /* DO we want TO FIND ONLY passive customers ? */
        (IF cday NE ? THEN NOT can-find(FIRST FixCDR where
                                          FixCDR.InvCust = Customer.CustNum AND
                                          FixCDR.Date >= cday) ELSE TRUE)

by (if order1 = 1 then "-"
                 ELSE Customer.Salesman)
by (if order2 = 1 then string(Customer.CustNum,"9999999")
                 ELSE Customer.CustName ):

   /* asiakasta hoitavan myyjAn nimi */
   FIND Salesman where 
        Salesman.Brand    = gcBrand AND
        Salesman.Salesman = Customer.Salesman
   no-lock no-error.
   IF AVAIL Salesman THEN ASSIGN mynimi = Salesman.SmName.
   else                          mynimi = "!! UNKNOWN !!".

   if Customer.Reseller ne "" THEN DO:
      FIND Reseller where 
           Reseller.Brand    = gcBrand AND
           Reseller.Reseller = Customer.Reseller
      no-lock no-error.
      IF AVAIL Reseller THEN ASSIGN rsname = Reseller.RsName.
      else                        rsname = "!! UNKNOWN !!".
   END.
   else rsname = "".

   /* Medium AND large Size customers have an unique project code, i.e.
      cust.no with leading zeros + one extra "0".  SMALL customers have
      NO project code */

   if Customer.Size = "S" then pr-code = "00000000".
   else pr-code = string(Customer.CustNum,"9999999") + "0".

   IF excel THEN DO:
      PUT STREAM excel UNFORMATTED
           Customer.CustNum          tab
           pr-code                 tab
           Customer.Salesman         tab
           mynimi                  tab
           Customer.Reseller         tab
           rsname                  tab
           Customer.CustName         tab
           Customer.Address          tab
           Customer.ZipCode        tab
           Customer.PostOffice          tab
           Customer.Contact        tab
           Customer.ContrBeg         tab
           Customer.ContrEnd         tab
           Customer.ConnType  format "Dir/Indir"   tab
           Customer.Size            tab
           Customer.Phone          tab
           Customer.Email.

      RUN uexskip(1).
      NEXT print-line.
   END.

   /* tarvitaanko uusi sivu */
   IF rl >= skayt1 - (IF order1 = 1 THEN 0 ELSE 3) THEN DO:
      IF sl > 0 THEN PUT STREAM tul skip(spit1 - rl).
      ASSIGN
      rlx = 0
      sl = sl + 1
      rl = 10.
      view STREAM tul FRAME sivuots.
   END.

   /* uusi myyja ? */
   IF order1 > 1 AND Customer.Salesman NE edmyyja THEN DO:
      PUT STREAM tul
      skip(1)
      "Salesman:"                      AT 2 space(1)
      Customer.Salesman  format "x(8)"   space(1)
      mynimi           format "x(30)"
      "Reseller:"                    AT 45 space(1)
      Customer.Reseller  format "x(8)"  space(1)
      rsname           format "x(30)"
      SKIP
      fill ("-",43)    format "x(43)" AT 2 SKIP.
      ASSIGN rl = rl + 3 rlx = 0.
   END.

   DISPLAY STREAM tul
           Customer.CustNum
           Customer.CustName
           Customer.Address
           Customer.ZipCode
           Customer.PostOffice
           Customer.Contact
           Customer.ContrBeg
           Customer.ContrEnd
           Customer.Phone
           Customer.ConnType
   WITH FRAME asline.
   DOWN STREAM tul WITH FRAME asline.

   edmyyja = Customer.Salesman.

   /* line- ja katkolaskurit */
   ASSIGN
   rl = rl + 1
   rlx = rlx + 1.
   IF rlx = 5 THEN DO:
      /* tyhjA line joka 5:nnen vAliin */
      ASSIGN rl = rl + 1  rlx = 0.
      PUT STREAM tul skip(1).
   END.

END. /* print-line (FOR EACH) */

/* vielA viimeinen sivu kohdalleen */
IF excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   PAUSE 0 no-message.
   message "File '" + exPaymFile + "' is ready - hit ENTER !".
   PAUSE no-message.
END.
ELSE PUT STREAM tul skip(spit1 - rl).

