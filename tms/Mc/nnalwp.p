/* -----------------------------------------------------------------
  MODULE .......: NNALWP.P
  TASK .........: creates an customer adress list FOR word processor
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 24.03.1998
  CHANGED ......: 21.01.1999 pt CustGroup
                  16.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------------------ */

{commali.i}                        

DEF VAR exdir     AS c  NO-UNDO.
DEF VAR exName    AS c  NO-UNDO.
DEF VAR exdate1   AS DA NO-UNDO.
DEF VAR exdate2   AS DA NO-UNDO.
DEF VAR exhdr     AS c  NO-UNDO.
DEF VAR i         AS i  NO-UNDO.
DEF VAR tab       AS c  NO-UNDO.
DEF VAR sml       AS c  NO-UNDO.
DEF VAR c         AS c  NO-UNDO.
DEF VAR szlist    AS c  NO-UNDO.
def var ok        as lo no-undo format "Yes/No".
DEF VAR Qty       AS i  NO-UNDO.

DEF VAR Salesman   LIKE Salesman.Salesman NO-UNDO.
DEF VAR Category    LIKE CustCat.Category     NO-UNDO.
DEF VAR InvGroup   LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR CustGroup   LIKE CustGroup.CustGroup NO-UNDO.

DEF VAR cdate1    AS DA NO-UNDO.
DEF VAR cdate2    AS DA NO-UNDO.
DEF VAR CustNum    LIKE Customer.CustNum   NO-UNDO.
DEF VAR ConnType      LIKE Customer.ConnType     NO-UNDO.
def var bills     as lo format "Found/Not found" NO-UNDO.
def var idate1    as da format "99-99-99" NO-UNDO.
def var idate2    as da format "99-99-99" NO-UNDO.

DEF NEW shared STREAM excel.

/* get default direcory Name FOR OUTPUT */
DO FOR TMSUser:
   FIND TMSUser where TMSUser.UserCode = katun no-lock.
   ASSIGN exdir = TMSUser.RepDir.
END.



/* initial values */
tab      = chr(9).
exName   = exdir + "/" + "address.txt".
exhdr    = "Name,Contact,Address,Post nr,Post address".
idate1   = date(month(TODAY),1,year(TODAY)).
idate2   = idate1 + 40.
idate2   = date(month(idate2),1,year(idate2)) - 1.



form
   skip(1)
"  Information:  This program writes an ADDRESS LIST of selected customers"
"                for a word processing program as a TAB-separated ASCII-file,"
"                one row / customer."
skip(1)
"                File's Name is .:" exName format "x(35)" no-label skip(1)
"                Ext Cust Group .:"  CustGroup
help "Code of an external Customer Group, (EMPTY: none)"
CustGroup.CGName no-label at 46 format "x(30)" SKIP
"                Salesman .......:"  Salesman 
help "Code of Salesman, empty = ALL" Salesman.SmName NO-LABEL AT 46 SKIP
"                Inv. group .....:" InvGroup  
help "Code if invoicing group: empty = ALL"  IGName NO-LABEL AT 46
                                             format "x(30)"         SKIP
"                Bills" bills no-label
help "Shall there (not) be bills Printed out between these dates (F/N)"       ":" idate1 NO-LABEL 
help "Earliest printout Date of an invoice" "-"
idate2 NO-LABEL
help "Latest printout Date of an invoice"

SKIP                                             
"                Size: ..........:" sml no-label format "X(4)"
help "Size code(s) of desired customers, e.g. 'SMLX' or 'ML', ..." SKIP
"                Contract began .:"           cdate1 no-label format "99-99-99"
help "Earliest starting Date of contract" "-" cdate2 no-label format "99-99-99"
help "Latest starting Date of contract"
"                Category .......:" Category
help "Customer category code"  CatName NO-LABEL AT 46 SKIP
"                Discount cust.no:" CustNum 
help "Discount/rating customer that must exist on customer records"
 Customer.CustName NO-LABEL AT 46 SKIP
"                ConnType. type .....:" ConnType no-label 
help "Type of subscriber: (D)irect / (I)ndirect connection, (?) = ALL"
skip(1)

WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " CUSTOMER MAILING LIST " + string(pvm,"99-99-99") + " " NO-LABELS FRAME start.

cdate2 = TODAY.
cdate1 = 1/1/1995.       
bills  = TRUE.                      

cfc = "sel". RUN ufcolor.


sml = "SMLX".

PAUSE 0.
DISP 
  "ALL" @ CustGroup.CGName
  "ALL" @ Salesman.SmName
  "ALL" @ IGName
  "ALL" @ CatName
  "ALL" @ Customer.CustName
WITH FRAME start.  

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN ufkey.
   UPDATE
      exName
      CustGroup
      Salesman InvGroup bills 
      idate1  
      idate2  validate(input idate2 >= input idate1,"Invalid order !")
      sml
      cdate1  validate(cdate1 ne ?,"Give first Date !")
      cdate2  validate(input cdate2 >= input cdate1,"Invalid order !")
      Category  CustNum ConnType
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
         if frame-field = "CustGroup" THEN DO:
            if input frame start CustGroup = "" then disp "ALL" @ CGName.
            ELSE DO:
               FIND CustGroup where 
                    CustGroup.Brand     = gcBrand AND
                    CustGroup.CustGroup = INPUT CustGroup
               no-lock no-error.
               IF NOT AVAIL CustGroup THEN DO:
                  BELL.
                  message "Unknown External Customer group !".
                  NEXT.
               END.
               DISP CGName.
            END.
         END.      

         else if frame-field = "InvGroup" THEN DO:
            if input frame start InvGroup = "" then disp "ALL" @ IGName.
            ELSE DO:
               FIND InvGroup where 
                    InvGroup.Brand    = gcBrand AND
                    InvGroup.InvGroup = INPUT InvGroup
               no-lock no-error.
               IF NOT AVAIL InvGroup THEN DO:
                  BELL.
                  message "Unknown invoicing group !".
                  NEXT.
               END.
               DISP IGName.
            END.
         END.      


         else if frame-field = "Salesman" THEN DO:
            if input frame start Salesman = "" then disp "ALL" @ IGName.
            ELSE DO:
               FIND Salesman where 
                    Salesman.Brand    = gcBrand AND
                    Salesman.Salesman = INPUT Salesman
               no-lock no-error.
               IF NOT AVAIL Salesman THEN DO:
                  BELL.
                  message "Unknown Salesman !".        
                  NEXT.
               END.
               DISP SmName.
            END.
         END.
         else if frame-field = "Category" THEN DO:

            if input frame start Category = "" then disp "ALL" @ CatName.
            ELSE DO:
               FIND CustCat where 
                    CustCat.Brand    = gcBrand AND
                    CustCat.Category = INPUT FRAME start Category
               no-lock no-error.
               IF NOT AVAIL CustCat THEN DO:
                  BELL.
                  message "Unknown customer category !".
                  NEXT.
               END.
               DISP CatName.
            END.
         END.

         else if frame-field = "CustNum" THEN DO:

            if input frame start CustNum = 0  then disp "ALL" @ Customer.CustName.
            ELSE DO:
               FIND Customer where Customer.CustNum = INPUT FRAME start CustNum
               no-lock no-error.
               IF NOT AVAIL Customer THEN DO:
                  BELL.
                  message "Unknown customer !".
                  NEXT.
               END.
               DISP CustName.
            END.
         END.

      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   message "Browsing and sorting data ...".

   OUTPUT STREAM excel TO value(exName).

   DO i = 1 TO num-entries(exhdr).
      PUT STREAM excel UNFORMATTED
      entry(i,exhdr).
      IF i < num-entries(exhdr) THEN PUT STREAM excel UNFORMATTED tab.
   END.   
   RUN uexskip(1).

   /* refine the Size STRING */
   szlist = "".
   DO i = 1 TO length(sml).
      c = substr(sml,i,1).
      if c = "x" then c = "XL".
      szlist = szlist + c + ",".
   END.

   FOR EACH Customer no-lock where 
            Customer.Brand = gcBrand AND 
            lookup(Customer.Size,szlist) 
            > 0   AND 
           (if Salesman ne "" THEN Customer.Salesman = Salesman ELSE TRUE)  AND
           (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE) AND
           (if Category  ne "" THEN Customer.Category  = Category  ELSE TRUE)
               AND
           (IF ConnType    NE ?  THEN Customer.ConnType    = ConnType   
            ELSE TRUE)    AND
           (IF CustNum  NE 0  THEN Customer.RateCust = CustNum  ELSE TRUE) AND
            (if CustGroup ne "" 
             THEN can-find(CGMember of Customer where
                              CGMember.CustGroup = CustGroup) 
             ELSE TRUE)       AND
            (IF bills         THEN can-find(FIRST Invoice of Customer where
                                                  Invoice.InvDate >= idate1  AND
                                                  Invoice.InvDate <= idate2)
                        ELSE NOT   can-find(FIRST Invoice of Customer where
                                                  Invoice.InvDate >= idate1  AND
                                                  Invoice.InvDate <= idate2)) 
   BY
   Customer.BankId:

      Qty = Qty + 1.

      PUT STREAM excel UNFORMATTED 
        Customer.CustName  tab 
        Customer.Contact tab 
        Customer.Address tab 
        Customer.ZipCode tab 
        Customer.PostOffice.
      RUN uexskip(1).

   END.
   OUTPUT STREAM excel CLOSE.

   PAUSE 0 no-message.
   MESSAGE
   "Totally" Qty "customers were Printed to address list - press ENTER !".
   PAUSE no-message.

   LEAVE.
END. /* crit */
PAUSE 0 no-message.
HIDE FRAME crit no-pause.

