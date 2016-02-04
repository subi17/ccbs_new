/* ----------------------------------------------------------
MODULE ......: nncusco.p
TASK ........: Count # of customers
APPLICATION .: TicketMaster
CREATED .....: 12.08.1998 pt
CHANGED .....: 08.12.1998 pt
               10.07.2002 tk validation for date2
               11.09.2002 jp Invgroup validation
               16.09.2003 aam brand
VERSION .....: M15
----------------------------------------------------------- */

{Syst/commali.i}

def var date1    as da format "99-99-99"  NO-UNDO.
def var date2    as da format "99-99-99"  NO-UNDO.
DEF VAR amt      AS i  EXTENT 12          NO-UNDO.
DEF VAR InvGroup  LIKE InvGroup.InvGroup    NO-UNDO.
def var cdate    as da format "99-99-99"  NO-UNDO.
DEF VAR Called   AS lo                    NO-UNDO.
form
   skip(1)
   " Contract done between ......:" date1
   help "Earliest Date of contract"
   "-"
   date2
   help "Latest Date of contract :" SKIP

   " Check if Calls after .......:" cdate
   help "Check how many customers have Called since this date" SKIP

   " Invoicing Group ............:" InvGroup help "Invoicing Group Code"

   skip(1)
   "                                      All  Called c." SKIP
   " Amount of contracts done....:" amt[1] amt[7]  SKIP
   "  - still active ............:" amt[2] amt[8]  SKIP
   "    - Size S ................:" amt[3] amt[9]  SKIP
   "    - Size M ................:" amt[4] amt[10] SKIP
   "    - Size L ................:" amt[5] amt[11] SKIP
   "    - Size XL ...............:" amt[6] amt[12] SKIP
WITH
   overlay centered title " COUNT CUSTOMERS " ROW 5 NO-LABELS FRAME count.


date1 = TODAY.
date2 = date1.
cdate = date(1,1,year(TODAY)).

DO WITH FRAME count:
   ehto = 9. RUN Syst/ufkey.
   UPDATE 
   date1 
   date2 VALIDATE ( input date2 >= input date1,"Invalid order !")
   cdate 
   InvGroup
   VALIDATE (CAN-FIND(invgroup WHERE 
                      InvGroup.Brand    = gcBrand AND
                      invgroup.invgroup = INPUT invgroup),
             "Unknown Invoice Group Code!").


   ufk = 0. ehto = 3. RUN Syst/ufkey.

   message "Counting ...".
   FOR EACH Customer no-lock where
            Customer.Brand     = gcBrand AND
            Customer.ContrBeg >= date1   AND
            Customer.ContrBeg <= date2   AND
            (if InvGroup ne "" THEN Customer.InvGroup = InvGroup ELSE TRUE):

       /* amount ALL */
       amt[1] = amt[1] + 1.

       FIND FIRST FixCDR where
                  FixCDR.InvCust  = Customer.CustNum AND
                  FixCDR.Date  >= cdate
       no-lock no-error.
       Called = AVAIL FixCDR.

       IF Called THEN amt[7] = amt[7] + 1.


       /* amount  active */
       IF Customer.ContrEnd = ? THEN DO:
          amt[2] = amt[2] + 1.
          IF Called THEN amt[8] = amt[8] + 1.

          if Customer.Size = "S"  THEN DO:
             amt[3] = amt[3] + 1.
             IF Called THEN amt[9] = amt[9] + 1.
          END.
          if Customer.Size = "M"  THEN DO:
             amt[4] = amt[4] + 1.
             IF Called THEN amt[10] = amt[10] + 1.
          END.
          if Customer.Size = "L"  THEN DO:
             amt[5] = amt[5] + 1.
             IF Called THEN amt[11] = amt[11] + 1.
          END.
          if Customer.Size = "XL" THEN DO:
             amt[6] = amt[6] + 1.
             IF Called THEN amt[12] = amt[12] + 1.
          END.
       END.
       PAUSE 0.
       DISP amt[1 FOR 12] WITH FRAME count.
    END.

    message "Press ENTER to continue !".
    PAUSE no-message.
END.
HIDE MESSAGE.
HIDE FRAME count no-pause.

