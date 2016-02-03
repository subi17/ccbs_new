/*---------------------------------------------------------------------------
  MODULE .......: NNCGGM.P
  TASK .........: Gather members into a Customer Group
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 02-12-98
  CHANGED ......: 21.01.99 pt selection criteria corrected IF as-ponro = ?
                              also check whether Customer had bills 
                  21.05.02/tk selection BY CustPP            
                  21.08.02/jp Save previous criteria
                  25.09.02/jp display names
                  19.03.03 aam run tasks when changes occur (fecgtask)
                  02.06.03 aam clitype & eMail, layout,
                               check pricelist through priceplan
                  16.09.03/aam brand                               
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/eventval.i}
{Func/fecgtask.i}
{Func/timestamp.i}

DEF BUFFER rcust FOR Customer.
DEF BUFFER new-cgmember FOR CGMember.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   DEFINE VARIABLE lhnew-CGMember AS HANDLE NO-UNDO.
   lhnew-CGMember = BUFFER new-CGMember:HANDLE.
   RUN StarEventInitialize(lhnew-CGMember).


   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhCGMember).
   END.

END.



DEF INPUT PARAMETER  CustGroup  LIKE CustGroup.CustGroup NO-UNDO .  

DEF VAR ProdPack   AS CHAR        NO-UNDO. 
DEF VAR xCustGroup LIKE CustGroup.CustGroup      NO-UNDO.
DEF VAR Salesman   LIKE Salesman.SalesMan        NO-UNDO.
DEF VAR Reseller   LIKE Reseller.reseller        NO-UNDO.
DEF VAR cbdate1    AS DA                         NO-UNDO.
DEF VAR cbdate2    AS DA                         NO-UNDO.
DEF VAR bidate1    AS DA                         NO-UNDO.
DEF VAR bidate2    AS DA                         NO-UNDO.
DEF VAR abbrev     LIKE Customer.SearchName          NO-UNDO.
DEF VAR Size       LIKE Customer.Size            NO-UNDO.
DEF VAR ConnType   LIKE Customer.conntype             NO-UNDO.
DEF VAR InvGroup   LIKE InvGroup.InvGroup        NO-UNDO.
DEF VAR PriceList  LIKE PriceList.PriceList      NO-UNDO.
DEF VAR rd-cust-nr LIKE Customer.InvCust        NO-UNDO.
DEF VAR Category   LIKE Customer.Category        NO-UNDO.
def var as-ponro    as c  format "x(6)"           NO-UNDO.
def var sz-s       as lo format "S/-"            NO-UNDO.
def var sz-m       as lo format "M/-"            NO-UNDO.
def var sz-l       as lo format "L/-"            NO-UNDO.
def var sz-xl      as lo format "XL/-"           NO-UNDO.
def var co-d       as lo format "Direct/-"       NO-UNDO.
def var co-i       as lo format "Indirect/-"     NO-UNDO.
def var ok         as lo format "Yes/No"         NO-UNDO.
DEF VAR sizes      AS c                          NO-UNDO.
DEF VAR conns      AS c                          NO-UNDO.
DEF VAR amt1       AS i                          NO-UNDO.
DEF VAR amt2       AS i                          NO-UNDO.
DEF VAR amt3       AS I                          NO-UNDO.
DEF VAR Comma      AS C                          NO-UNDO INIT ",".
def var bicheck    as lo format "Check/Omit"     NO-UNDO.
DEF VAR updatemode AS LO                         NO-UNDO.

DEF VAR lcTask     AS CHAR                       NO-UNDO.
DEF VAR lcCLIType  AS CHAR                       NO-UNDO.
DEF VAR lleMail    AS LOGIC                      NO-UNDO. 
DEF VAR ldStamp    AS DEC                        NO-UNDO. 
DEF VAR llFound    AS LOG                        NO-UNDO. 
DEF VAR liCount    AS INT                        NO-UNDO. 
DEF VAR lcCheck    AS CHAR                       NO-UNDO. 

form
skip

" Note: This program allows You to gather members into a Customer "
"       group, using the combination of parameters shown below."  skip(1)

"Customers in Group ..:" AT 2 
   xCustGroup
   help "Seek only members in this group; EMPTY = seek ALL Customers"
   CustGroup.CGName format "x(30)" AT 36
   SKIP

"Customer Category ...:" AT 2
   Category
   help "Customers in this category, EMPTY = ALL"
   CustCat.CatName AT 36
   SKIP

"Invoicing Group .....:" AT 2
   InvGroup
   help "Customers in this invoicing group; EMPTY = ALL"
   InvGroup.Igname format "x(24)" AT 36
   SKIP

"Price List ..........:" AT 2
   PriceList
   help "Customers with this Price list; EMPTY = ALL"
   PriceList.PLName AT 36
   SKIP

"Rate/Discnt Customer :" AT 2
   rd-cust-nr format "zzzzzzz"
   Help "Customers that have this rate/disc. -cust.; EMPTY = ALL"
   rcust.CustName format "x(24)"  AT 36
   SKIP

"Salesman ............:" AT 2
   Salesman
   help "Customers with this Salesman code; EMPTY = ALL"
   Salesman.SmName AT 36
   SKIP

"Reseller ............:" AT 2
   Reseller
   help "Customers with this reseller code; EMPTY = ALL"
   Reseller.rsName AT 36
   SKIP

"Abbreviation ........:" AT 2  
   abbrev
   help "Customers with this abbreviation; EMPTY = ALL"
   SKIP

"Postal Code .........:" AT 2
   as-ponro
   help "Leading digits of Customers' postal code; EMPTY = ALL"
   SKIP(1)

"Bills found during ..:" AT 2 
   bidate1 format "99-99-99"
   help "Earliest Invoice date"
   "-" 
   bidate2 format "99-99-99"
   help "Latest Invoice date"
   bicheck
   help "Do You want to check whether there are bills ? (C)heck/(O)mit"
   SKIP

"CLI Type ............:" AT 2
   lcCLIType FORMAT "X(25)" 
   HELP "Comma separated list; 'FIXED' or mobile CLI type (EMPTY = ALL)"
   SKIP

"Customers with Size .:" AT 2 
   sz-s
   help "(S)MALL Customers (if not wanted, enter '-')"
   sz-m
   help "(M)EDIUM Size Customers (if not wanted, enter '-')"
   sz-l
   help "(L)ARGE customers (if not wanted, enter '-')"
   sz-xl
   help "E(X)TRA LARGE customers (if not wanted, enter '-')"

"Contract begun:" AT 44
   cbdate1 format "99-99-99"
   help "Earliest Date when contract was committed"
   "-" 
   cbdate2 format "99-99-99"
   help "Latest Date when contract was committed"
   SKIP

"Type of Connection ..:" AT 2
   co-d
   help "DIRECT connected customers (if not wanted, enter '-')"
   co-i
   help "INDIRECTLY connected customers (if not wanted, enter '-')"

"EMail address :" AT 44
   lleMail FORMAT "Yes/No"
   HELP "Only customers who have an eMail address"
  SKIP

WITH
   centered OVERLAY NO-LABEL ROW 1
   title " Gather members into customer group '" + CustGroup + "' " +
         "(" + gcBrand + ") "
   FRAME rajat.

form
   amt1 label "Checked" amt2 label "Accepted"  amt3 label "Removed"
WITH
   row 3 centered overlay title " NO. OF CUSTOMERS ... " side-labels FRAME Qty.

/* default values */
ASSIGN
cbdate1 = 1/1/1995      bidate1 = cbdate1
cbdate2 = 12/31/2030    bidate2 = cbdate2
sz-s    = TRUE
sz-m    = TRUE
sz-l    = TRUE
sz-xl   = TRUE
co-d    = TRUE
co-i    = TRUE
llEMail = FALSE.

PAUSE 0.

FIND FIRST custgroup WHERE
           CustGroup.Brand     = gcBrand AND
           custgroup.custgroup = custgroup
EXCLUSIVE-LOCK NO-ERROR.           

updatemode = CustGroup.PrevCrit = ""  .

IF NOT updatemode THEN
ASSIGN
xCustGroup = ENTRY(1,Custgroup.PrevCrit,",")
Category   = ENTRY(2,Custgroup.PrevCrit,",")
InvGroup   = ENTRY(3,Custgroup.PrevCrit,",")
PriceList  = ENTRY(4,Custgroup.PrevCrit,",")
rd-cust-nr = INT(ENTRY(5,Custgroup.PrevCrit,","))
ProdPack   = ENTRY(6,Custgroup.PrevCrit,",")
Salesman   = ENTRY(7,Custgroup.PrevCrit,",")
Reseller   = ENTRY(8,Custgroup.PrevCrit,",")
abbrev     = ENTRY(9,Custgroup.PrevCrit,",")
sz-s       = IF ENTRY(10,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
sz-m       = IF ENTRY(11,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
sz-l       = IF ENTRY(12,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
sz-xl      = IF ENTRY(13,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
co-d       = IF ENTRY(14,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
co-i       = IF ENTRY(15,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
cbdate1    = DATE(INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),7,4)))
cbdate2    = DATE(INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),7,4)))
bidate1    = DATE(INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),7,4)))
bidate2    = DATE(INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),7,4)))


bicheck    = IF ENTRY(20,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
as-ponro   = ENTRY(21,Custgroup.PrevCrit,",") 
lleMail    = IF ENTRY(22,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
lcCLIType  = REPLACE(ENTRY(23,Custgroup.PrevCrit,","),CHR(1),",")
.

PAUSE 0.
disp 
xCustGroup 
Category   
InvGroup   
PriceList  
rd-cust-nr 
Salesman   
Reseller   
abbrev     
sz-s       
sz-m       
sz-l       
sz-xl      
co-d       
co-i       
cbdate1    
cbdate2
bidate1
bidate2
bicheck    
as-ponro    
lleMail
lcCLIType
with frame rajat.
pause 0.

FIND custgroup where  
     CustGroup.Brand     = gcBrand AND
     custgroup.custgroup = xcustgroup   NO-LOCK NO-ERROR.
FIND CustCat where
     CustCat.Brand    = gcBrand AND
     CustCat.Category = Category     NO-LOCK NO-ERROR.
FIND PriceList where 
     PriceList.Brand     = gcBrand AND
     PriceList.PriceList = Pricelist    NO-LOCK NO-ERROR.
FIND invgroup where 
     InvGroup.Brand     = gcBrand AND
     InvGroup.InvGroup  = invgroup     NO-LOCK NO-ERROR.
FIND salesman where 
     Salesman.Brand     = gcBrand AND
     Salesman.SalesMan  = salesman     NO-LOCK NO-ERROR.
FIND Reseller where
     Reseller.Brand    = gcBrand AND
     Reseller.reseller = reseller     NO-LOCK NO-ERROR.
FIND rcust where rcust.CustNum         = rd-cust-nr   NO-LOCK NO-ERROR.

DISP
custgroup.cgname  WHEN     AVAIL custgroup
"ALL CUSTOMERS"   WHEN NOT AVAIL custgroup   @ custgroup.cgname

CustCat.CatName   WHEN     AVAIL CustCat
"ALL"             WHEN NOT AVAIL CustCat     @ CustCat.CatName

PriceList.PLName  WHEN     AVAIL PriceList
"ALL"          WHEN NOT AVAIL PriceList   @ PriceList.PLName

InvGroup.Igname   WHEN     AVAIL invgroup
 "ALL"             WHEN NOT AVAIL invgroup    @ InvGroup.Igname
Salesman.SMName   WHEN     AVAIL salesman
"ALL"              WHEN NOT AVAIL salesman    @ Salesman.SmName
Reseller.rsName     WHEN     AVAIL Reseller
"ALL"              WHEN NOT AVAIL Reseller      @ Reseller.rsName
rcust.CustName      WHEN     AVAIL rcust
"ALL"              WHEN NOT AVAIL rcust       @ rcust.CustName 
with frame rajat.

rajat:
repeat WITH FRAME rajat:
   ehto = 9. RUN ufkey.

IF updatemode  THEN
   UPDATE
   xCustGroup Category InvGroup PriceList rd-cust-nr
   Salesman Reseller abbrev as-ponro
   bidate1 bidate2  bicheck
   lcCLIType
   sz-s sz-m sz-l sz-xl co-d co-i 
   cbdate1 cbdate2 
   lleMail 
   WITH FRAME rajat EDITING:
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME rajat:
         HIDE MESSAGE.


         if frame-field = "xCustGroup" THEN DO:
            if input xCustGroup = "" then disp 
              "ALL CUSTOMERS" @ CustGroup.CGName.
            ELSE DO:
               FIND CustGroup where 
                    CustGroup.Brand     = gcBrand AND
                    CustGroup.CustGroup = INPUT xCustGroup
               no-lock no-error.
               IF NOT AVAIL CustGroup THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP CustGroup.CGName.
               IF INPUT xCustGroup = CustGroup THEN DO:
                  BELL. MESSAGE
                  "You are not allowed to use SAME group code !".
                  NEXT.
               END.
            END.
         END.

         else if frame-field = "Category" THEN DO:
            if input Category = "" then disp "ALL" @ CustCat.CatName.
            ELSE DO:
               FIND CustCat where 
                    CustCat.Brand    = gcBrand AND
                    CustCat.Category = INPUT Category 
               no-lock no-error.
               IF NOT AVAIL CustCat THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP CustCat.CatName.
            END.
         END.

         else if frame-field = "InvGroup" THEN DO:
            if input InvGroup = "" then disp "ALL" @ InvGroup.Igname.
            ELSE DO:                        
               FIND InvGroup where 
                    InvGroup.Brand    = gcBrand AND
                    InvGroup.InvGroup = INPUT InvGroup
               no-lock no-error.
               IF NOT AVAIL InvGroup THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP InvGroup.Igname.
            END.
         END.

         else if frame-field = "PriceList" THEN DO:
            if input PriceList = "" then disp "ALL" @ PriceList.PLName.
            ELSE DO:
               FIND PriceList where 
                    PriceList.Brand     = gcBrand AND
                    PriceList.PriceList = INPUT PriceList no-lock no-error.
               IF NOT AVAIL PriceList THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP PriceList.PLName.
            END.
         END.

         else if frame-field = "rd-cust-nr" THEN DO:
            if input rd-cust-nr = 0 then disp "ALL" @ rcust.CustName.
            ELSE DO:
               FIND rcust where rcust.CustNum = INPUT rd-cust-nr
               no-lock no-error.
               IF NOT AVAIL rcust THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP rcust.CustName.
            END.
         END.

         else if frame-field = "Salesman" THEN DO:
            if input Salesman = "" then disp "ALL" @ Salesman.SmName.
            ELSE DO:
               FIND Salesman where 
                    Salesman.Brand     = gcBrand AND
                    Salesman.SalesMan = INPUT Salesman
               no-lock no-error.
               IF NOT AVAIL Salesman THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP Salesman.SmName.
            END.
         END.

         else if frame-field = "Reseller" THEN DO:
            if input Reseller = "" then disp "ALL" @ Reseller.rsName.
            ELSE DO:
               FIND Reseller where 
                    Reseller.Brand    = gcBrand AND
                    Reseller.reseller = INPUT Reseller
               no-lock no-error.
               IF NOT AVAIL Reseller THEN DO:
                  bell. message "UNKNOWN !". NEXT.
               END.
               DISP Reseller.rsName.
            END.
         END.

         else if frame-field = "cbdate2" THEN DO:
            IF INPUT cbdate2 < INPUT cbdate1 THEN DO:
               BELL. 
               message "Invalid order !".
               NEXT-PROMPT cbdate1.
               NEXT.
            END.
         END.      

         else if frame-field = "bidate2" THEN DO:
            IF INPUT bidate2 < INPUT bidate1 THEN DO:
               BELL. 
               message "Invalid order !".
               NEXT-PROMPT bidate1.
               NEXT.
            END.
         END.     

         ELSE IF FRAME-FIELD = "lcCLIType" AND INPUT lcCLIType > ""
         THEN DO:
            DO liCount = 1 TO NUM-ENTRIES(INPUT lcCLIType):
               lcCheck = ENTRY(liCount,INPUT lcCLIType).

               IF lcCheck NE "Fixed" AND
                  NOT CAN-FIND(CLIType WHERE 
                               CLIType.Brand   = gcBrand AND
                               CLIType.CLIType = lcCheck)
               THEN DO:
                  liCount = 99.
                  LEAVE.
               END.
            END.

            IF liCount = 99 THEN DO:
               BELL.
               MESSAGE "Unknown CLIType" lcCheck.
               NEXT-PROMPT lcCLIType.
               NEXT.
            END.

         END.

      END.
      APPLY LASTKEY.
   END.

toimi:
   repeat WITH FRAME toimi:
      PAUSE 0.
      ASSIGN
      ehto = 0 ufk = 0
      ufk[1] = 7 ufk[5] = 15 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN DO:
          updatemode = TRUE.
          NEXT rajat.
      END.    
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are You SURE You want to gather those customers (Y/N) ?"
         UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.

   sizes = "".
   if sz-s then sizes =         "S,".
   if sz-m then sizes = sizes + "M,".
   if sz-l then sizes = sizes + "L,".
   if sz-M then sizes = sizes + "XL".

   conns = "".
   if co-i then conns =         "I,".
   if co-d then conns = conns + "D".

   amt1 = 0. amt2 = 0.

   /* seek Customers AND accept those matching WITH given parameters */
   if xCustGroup ne "" THEN DO:
      FIND FIRST CGMember where 
                 CGMember.Brand     = gcBrand AND
                 CGMember.CustGroup = xCustGroup no-lock no-error.
      FIND Customer WHERE Customer.CustNum = CGMember.CustNum no-lock no-error.
   END.
   ELSE FIND FIRST Customer WHERE Customer.Brand = gcBrand no-lock no-error.



Customer:
   repeat:

      IF NOT AVAIL Customer THEN LEAVE Customer.

      amt1 = amt1 + 1.
      PAUSE 0.
      if amt1 mod  500 =  0  THEN DISP amt1 WITH FRAME Qty.


      c-cust:
      repeat:

        if Category     ne "" AND Customer.Category   NE Category    
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   

        if abbrev     ne "" AND Customer.SearchName  NE abbrev    
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   

        if Salesman    ne "" AND Customer.SalesMan  NE Salesman   
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   

        if Reseller    ne "" AND Customer.Reseller  NE Reseller   
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   

        if PriceList ne "" THEN DO:
           llFound = FALSE. 

           FOR EACH BillTarget OF Customer NO-LOCK,
              FIRST RatePlan NO-LOCK WHERE
                    RatePlan.Brand    = Customer.Brand AND
                    RatePlan.RatePlan = BillTarget.RatePlan,
              FIRST PListConf OF RatePlan NO-LOCK WHERE
                    PListConf.PriceList = PriceList:

              llFound = TRUE.
              LEAVE.
           END.

           IF NOT llFound THEN DO:
              RUN pRemoveCGMember.
              LEAVE C-CUST.
           END.

        END.   

        if InvGroup    ne "" AND Customer.InvGroup  NE InvGroup   
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   

        IF rd-cust-nr NE 0  AND Customer.InvCust NE rd-cust-nr 
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        IF lookup(Customer.Size,sizes) = 0                     
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        if lookup(string(Customer.conntype,"D/I"),conns) = 0       
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        IF Customer.ContrBeg < cbdate1                          
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.   
        IF Customer.ContrEnd > cbdate2                          
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        IF bicheck THEN DO:
           /* has this Customer any Invoices buring desired
              Period of time ? */
           FIND FIRST Invoice WHERE 
                      Invoice.CustNum = Customer.CustNum AND
                      Invoice.InvDate >= bidate1  AND
                      Invoice.InvDate <= bidate2
           no-lock no-error.
           IF NOT AVAIL Invoice                              
           THEN DO:
              RUN pRemoveCGMember.
              LEAVE C-CUST.          
           END.   
        END. 

        IF as-ponro ne "" AND 
          (Customer.ZipCode = ? OR
           NOT Customer.ZipCode BEGINS as-ponro)                 
        THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        IF lleMail AND Customer.eMail = "" THEN DO:
           RUN pRemoveCGMember.
           LEAVE C-CUST.
        END.

        IF lcCLIType NE "" THEN DO:

           ASSIGN ldStamp = fMake2DT(TODAY,TIME)
                  llFound = FALSE.

           IF LOOKUP("Fixed",lcCLIType) > 0 AND
              CAN-FIND(FIRST CLI OF Customer WHERE
                             CLI.CrStamp <= ldStamp AND 
                             CLI.ClStamp >= ldStamp)
           THEN llFound = TRUE.

           ELSE DO liCount = 1 TO NUM-ENTRIES(lcCLIType):            

              IF CAN-FIND(FIRST MobSub OF Customer WHERE
                                MobSub.CLIType = ENTRY(liCount,lcCLIType))
              THEN DO:
                 llFound = TRUE.
                 LEAVE.
              END.
           END.

           IF NOT llFound THEN DO:
              RUN pRemoveCGMember.
              LEAVE C-CUST.
           END.

        END.

        /***********************************************
        * This Customer matched WITH given parameters. *
        * NEXT we CREATE a NEW member record FOR it.   *
        ***********************************************/
        FIND CGMember where
             CGMember.CustNum   = Customer.CustNum AND
             CGMember.CustGroup = CustGroup NO-LOCK NO-ERROR.

        IF NOT AVAIL cgmember THEN DO:
           CREATE new-cgmember.          
           ASSIGN
           new-CGMember.Brand      = Customer.Brand
           new-CGMember.CustNum    = Customer.CustNum
           new-CGMember.CustName   = Customer.CustName
           new-CGMember.CustGroup  = CustGroup
           amt2                    = amt2 + 1.

           lcTask = fECGEnterTask(FALSE). 

           RUN StarEventMakeCreateEvent(lhnew-cgmember).

           PAUSE 0.
           DISP amt2 amt3 WITH FRAME Qty.
        END.
        LEAVE.
      END.


      /*********************************
      * Look AT the NEXT Customer ...  *
      *********************************/

      if xCustGroup ne "" THEN DO:
         FIND NEXT CGMember where 
                   CGMember.Brand     = gcBrand AND
                   CGMember.CustGroup = xCustGroup
         no-lock no-error.
         IF NOT AVAIL CGMember THEN LEAVE Customer.

         FIND Customer WHERE
              Customer.CustNum =  CGMember.CustNum no-lock no-error.
      END.
      ELSE FIND NEXT Customer WHERE 
         Customer.Brand = gcBrand no-lock no-error.

   END. /* Customer */
   PAUSE 0.

   message 
   "Totally" STRING(amt1,"zzzzzzz9")  "Customer checked       "  SKIP 
   "Totally" STRING(amt2,"zzzzzzz9")  "new member(s) created  "  SKIP
   "Totally" STRING(amt3,"zzzzzzz9")  "old member(s) removed  "  SKIP(1) 
   VIEW-AS ALERT-BOX.

   ok = FALSE.
   MESSAGE "Do You Want to Save Used Criteria?" update ok.    
   IF OK THEN DO:
      Find FIRST CustGroup WHERE
                 CustGroup.Brand     = gcBrand AND
                 CustGroup.CustGroup = CustGroup
      EXCLUSIVE-LOCK No-ERROR.

      IF AVAIL CustGroup THEN DO:
          CustGroup.PrevCrit = 
          xCustGroup                   +  Comma +
          Category                     +  Comma +
          InvGroup                     +  Comma +
          PriceList                    +  Comma +
          STRING(rd-cust-nr)           +  Comma +
          ProdPack                     +  Comma +
          Salesman                     +  Comma +
          Reseller                     +  Comma +
          abbrev                       +  Comma +
          STRING(sz-s)                 +  Comma +
          STRING(sz-m)                 +  Comma +
          STRING(sz-l)                 +  Comma +
          STRING(sz-xl)                +  Comma +
          STRING(co-d)                 +  Comma +
          STRING(co-i)                 +  Comma +
          STRING(cbdate1,"99-99-9999") +  Comma +
          STRING(cbdate2,"99-99-9999") +  Comma +
          STRING(bidate1,"99-99-9999") +  Comma +
          STRING(bidate2,"99-99-9999") +  Comma +
          STRING(bicheck)              +  Comma +
          STRING(as-ponro)             +  Comma +
          STRING(lleMail)              +  Comma +
          REPLACE(lcCLIType,",",CHR(1)).

          MESSAGE
          "Pre-defined criteria saved succesfully!"
          VIEW-aS ALERT-BOX TITLE "DONE".       
      END.           
   END.

   LEAVE.
END.
HIDE FRAME Qty.
HIDE FRAME rajat.
HIDE MESSAGE.


PROCEDURE pRemoveCGMember.
/* check FIRST whether this Customer is already a member */
        FIND        CGMember where
                    CGMember.CustNum   = Customer.CustNum AND
                    CGMember.CustGroup = CustGroup EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL CGMember THEN DO:
           amt3 = amt3 + 1.
           disp amt3 with frame qty.
           RUN StarEventMakeDeleteEvent(lhCGMember).

           lcTask = fECGLeaveTask(FALSE). 

           DELETE CGMember.
        END.

END PROCEDURE.



