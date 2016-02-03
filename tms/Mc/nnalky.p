/* ---------------------------------------------------------------------------
  MODULE .......: NNALKY.P
  FUNCTION .....: Startup parameters FOR various customer listing programs
  SOVELLUTUS ...: NN
  AUTHOR .......: TT
  CREATED ......: 06.02.1997
  changePVM ....: 22.04.1997 pt,  RUN nnasle
                  04.05.1997 pt, 2 x 2 jArjestystA ym.
                  07.05.1998 kl, myyja1 & 2 from INT into CHAR
                  13.05.1998 kl, PriceList InvGroup Reseller
                  02.11.1998 pt, exdir: default BY user
                  11.11.1998 pt, NEW FUNCTION into F7: RUN nnxorcu
                  09.01.1999 pt, CustGroup
                  26.09.2002/aam PriceList removed 
                  12.09.2003/aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}                        

{Syst/utumaa.i "new"}


DEF VAR asno1   LIKE Customer.CustNum       NO-UNDO init 0.
DEF VAR asno2   LIKE Customer.CustNum       NO-UNDO init 9999999.
def var Category   like Customer.Category       no-undo init "".
def var myyja1  as c    format "x(8)"     NO-UNDO.
def var myyja2  as c    format "x(8)"     NO-UNDO.
def var apvm1   as Date format "99-99-99" NO-UNDO.
def var apvm2   as Date format "99-99-99" NO-UNDO.
def var ppvm1   as Date format "99-99-99" NO-UNDO.
def var ppvm2   as Date format "99-99-99" NO-UNDO.
def var cday    as Date format "99-99-99" NO-UNDO.
DEF VAR PriceList AS c                      NO-UNDO.
DEF VAR InvGroup AS c                      NO-UNDO.
DEF VAR Reseller AS c                      NO-UNDO.
DEF VAR CustGroup LIKE CustGroup.CustGroup       NO-UNDO.
DEF VAR ConnType LIKE Customer.ConnType            NO-UNDO init ?.
DEF VAR i       AS i                      NO-UNDO.
DEF VAR order1  AS i                      NO-UNDO.
DEF VAR order2  AS i                      NO-UNDO.

def var jar1    as c    init "-,Salesman" NO-UNDO.
def var jar2    as c    init "CustNo,CustName" NO-UNDO.
DEF VAR j1      AS c                     NO-UNDO.
DEF VAR j2      AS c                     NO-UNDO.
DEF VAR exdir   AS c                     NO-UNDO.
def var exFile  as c format "x(40)"      NO-UNDO.


/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
      ASSIGN exdir = TMSUser.RepDir.
END.
if opsys = "unix" then exFile = exdir + "/custlist.txt".
else                   exFile = "custlist.txt".


form
   j1   format "x(16)" help "How to sort the customer list"
WITH
   overlay row 13 col 44 2 down no-label title " 1. SortOrder " FRAME jar1.

form
   j2   format "x(16)" help "How to sort the customer list"
WITH
   overlay row 14 col 44 2 down no-label title " 2. SortOrder " FRAME jar2.



form
   skip(1)
   "   Ext. Customer Group ...:" CustGroup NO-LABEL
   help "Code of an External Customer Group  (empty: NONE)"              SKIP

   "   Customer Numbers ......:" asno1 NO-LABEL
   help "Smallest customer number to be printed" " - " asno2 NO-LABEL
   help "Largest customer number to be printed"                          SKIP

   "   Salesman Codes ........: " myyja1 NO-LABEL TO 36
   help "First Salesman whose customers shall be printed"
   " - " myyja2 NO-LABEL
   help "Last Salesman whose customers shall be printed"                 SKIP

   Category   label "   Customer Category ....."
   help "Customer Category Code"                                         SKIP

   apvm1   label "   Contract done ........."
   help "Earliest day of a contract" " - " apvm2 NO-LABEL
   help "Latest day of a contract"                                       SKIP

   ppvm1   label "   Contract cancelled ...."
   help "Earliest day of a possible cancellation of a contract"
   " - " ppvm2 NO-LABEL
   help "Latest day of a possible cancellation of a contract"            SKIP

   cday    label "   Check if no Calls since"
   help "Pick ONLY customers who DO NOT have any Calls since this day"

   "(use this to find passive customers)"                                SKIP
   ConnType    label "   Type Of Connection ...."
   help "Connection (I)ndirect, (D)irect, (?) = ALL"                     SKIP

   InvGroup label "   Invoicing Group ......."
   help "Customer's Inv. Group Code;  EMPTY: ALL Invoicing Groups"       SKIP

   Reseller label "   Agent/Reseller ........"
   help "Customer's Agent/Reseller Code;  EMPTY: ALL agents/resellers"   SKIP
   skip(1)
   j1      label "   1. Sort Order.........." format "x(16)"       SKIP
   j2      label "   2. Sort Order ........." format "x(16)"       skip(1)
   exFile  label "   Name of Output File"
           help "Name for output File (ASCII-format, File RepType '.txt')"
   SKIP(1) 
with width 80 title color value(ctc) " CUSTOMER LISTS " side-labels
   COLOR value(cfc) OVERLAY FRAME rajat.

cfc = "sel". RUN ufcolor.
PAUSE 0 no-message.

ASSIGN
  CustGroup = ""
  myyja1 = "1"  myyja2 = "999"
  asno1  = 1    asno2  = 9999999
  order1 = 1    order2  = 2
  j1     = entry(order1,jar1)
  j2     = entry(order2,jar2).

DISPLAY asno1 asno2 Category apvm1 apvm2 ConnType ppvm1 ppvm2 j1 j2
WITH FRAME rajat.


rajat:
repeat WITH FRAME rajat:

   ehto = 9. RUN ufkey.
   UPDATE
      CustGroup validate(input CustGroup = "" OR
         can-find(FIRST CustGroup where
                        CustGroup.Brand     = gcBrand AND
                        CustGroup.CustGroup = input CustGroup), "NONE FOUND !")
      asno1
      asno2 validate(INPUT asno2 >= INPUT asno1,
         "Incorrect Order !")
      myyja1
      myyja2 validate(input myyja2 >= input myyja1, "Incorrect Order !")
      Category validate(input Category = "" or
          can-find(first CustCat where                              
                         CustCat.Brand    = gcBrand AND
                         CustCat.Category = input Category), "NONE FOUND !")
      apvm1
      apvm2 validate(input apvm2 >= input apvm1, "Invalid Order !")
      ppvm1
      ppvm2 validate(input ppvm2 >= input ppvm1, "Invalid Order !")
      cday
      ConnType
      InvGroup validate(input InvGroup = "" OR 
          can-find(FIRST InvGroup where
                         InvGroup.Brand    = gcBrand AND
                         InvGroup.InvGroup = input InvGroup), "NONE FOUND !")
      Reseller validate(input Reseller = "" OR 
          can-find(FIRST Reseller where
                         Reseller.Brand    = gcBrand AND
                         Reseller.Reseller = input Reseller), "NONE FOUND !")
      j1 j2
      WITH FRAME rajat EDITING:
            if frame-field = "j1" THEN DO WITH FRAME jar1:
               PAUSE 0.
               CLEAR FRAME jar1 ALL.
               DO i = 1 TO 2:
                  DISP entry(i,jar1) @ j1.
                  IF i = 1 THEN DOWN.
                  ELSE up 1.
               END.
               DOWN (order1 - 1).
               CHOOSE ROW j1 {Syst/uchoose.i} no-error.
               order1 = frame-line(jar1).
               HIDE FRAME jar1 no-pause.
               DISP entry(order1,jar1) @ j1 WITH FRAME rajat.
               apply keycode("enter").
            END.
            if frame-field = "j2" THEN DO WITH FRAME jar2:
               PAUSE 0.
               CLEAR FRAME jar2 ALL.
               DO i = 1 TO 2:
                  DISP entry(i,jar2) @ j2.
                  IF i = 1 THEN DOWN.
                  ELSE up 1.
               END.
               DOWN (order2 - 1).
               CHOOSE ROW j2 {Syst/uchoose.i} no-error.
               order2 = frame-line(jar2).
               HIDE FRAME jar2 no-pause.
               DISP entry(order2,jar2) @ j2 WITH FRAME rajat.
               LEAVE.
            END.
            READKEY.
            APPLY LASTKEY.

   END.  /* EDITING */

toimi:
   repeat WITH FRAME rajat:
      ASSIGN ufk = 0 ufk[1] = 132 ufk[3] = 807 ufk[4] = 847
                     ufk[5] = 808 ufk[6] = 847 ufk[7] = 997 ufk[8] = 8
             ehto = 0.

      RUN ufkey.

      IF toimi = 1 THEN  NEXT  RAJAT.
      IF toimi = 8 THEN  LEAVE RAJAT.

      IF toimi = 4 OR toimi = 6 OR toimi = 7 THEN DO:
         /* Ask Name FOR Excel / XOR File */
         if toimi = 7 then exFile = exdir + "/" + "xorcod.txt".
         ehto = 9. RUN ufkey.
         UPDATE exFile WITH FRAME rajat.
         if exFile = "" THEN NEXT toimi.
      END.

      /* refine the parameters */
      IF toimi = 3 OR
         toimi = 4 OR
         toimi = 5 OR
         toimi = 6 OR
         toimi = 7
      THEN DO:

         IF asno1 = ? THEN asno1 = 0.
         IF asno2 = ? OR asno2 = 0 THEN asno2 = 9999999.
         IF apvm1 = ? THEN apvm1 = 1/1/1900.
         IF apvm2 = ? THEN apvm2 = 12/31/9999.
         IF ppvm1 = ? THEN ppvm1 = 1/1/1900.
         IF ppvm2 = ? THEN ppvm2 = 12/31/9999.
      END.

      /* large printout */
      IF toimi = 5 THEN DO:
         ASSIGN
         tuni1 = "nnasll"
         tuni2 = "".
         tila =true.
         {Syst/tmsreport.i "return"}

         RUN nnasll(CustGroup,
                    asno1,asno2,
                    myyja1,myyja2,
                    Category,
                    apvm1,apvm2,
                    ppvm1,ppvm2,
                    cday,
                    ConnType,
                    InvGroup,
                    Reseller,
                    order1,order2).
         LEAVE toimi.
      END.

      /* Large Excel/ascii printout */
      IF toimi = 6 THEN DO:
         RUN nnasle(CustGroup,
                    asno1,asno2,
                    myyja1,myyja2,
                    Category,
                    apvm1,apvm2,
                    ppvm1,ppvm2,
                    cday,
                    ConnType,
                    InvGroup,
                    Reseller,
                    exFile,
                    order1,order2).
         LEAVE toimi.
      END.

      /* A brief list (either onto paper of Excel) */
      IF toimi = 3 OR toimi = 4 THEN DO:

         IF toimi = 3 THEN DO: /* ask AND open the printer */
            ASSIGN
            tuni1 = "nnasls"
            tuni2 = "".
            tila =true.
            {Syst/tmsreport.i "return"}
         END.

         RUN nnasls( CustGroup,
                     asno1,asno2,
                     myyja1,myyja2,
                     Category,
                     apvm1,apvm2,
                     ppvm1,ppvm2,
                     cday,
                     ConnType,
                     InvGroup,
                     Reseller,
                     order1,order2,
                     (toimi = 4),
                     exFile).
         LEAVE TOIMI.
      END.

      /* EXPORT customer codes FOR XOR */
      IF toimi = 7 THEN DO:
         IF cday NE ? THEN DO:
            message "NOTE: No call check with this function - press ENTER !".
            PAUSE no-message.
         END.

         RUN nnxorcu( CustGroup,
                      asno1,asno2,
                      myyja1,myyja2,
                      Category,
                      apvm1,apvm2,
                      ppvm1,ppvm2,
                      ConnType,
                      PriceList,
                      InvGroup,
                      Reseller,
                      exFile).
         LEAVE toimi.
      END.

   END. /* toimi */

   /* CLOSE the printer STREAM IF a paper report was done */
   IF toimi = 3 OR toimi = 5 THEN DO:
      tila = FALSE.
      {Syst/tmsreport.i}.
   END.
   LEAVE rajat.
END.

HIDE MESSAGE       no-pause.
HIDE FRAME rajat   no-pause.

