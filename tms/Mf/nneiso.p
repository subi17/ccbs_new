/* -----------------------------------------------
  MODULE .......: NNEISO.P
  FUNCTION .....: Customers who havo no Calls during ...
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 18-03-96 pt
  changePVM ....: 16-09-98 kl date1 - Date 2
                  28.05.02 aam fPutExcel removed 
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{excel.i}

def var cust-name as char no-undo format "x(30)".
def var date1     as Date no-undo format "99-99-9999" init TODAY.
def var date2     as Date no-undo format "99-99-9999" init TODAY.

def var excel     as lo   no-undo format "Yes/No".
def var exPaymFile    as c    no-undo format "x(30)".
DEF VAR exhdr     AS c    NO-UNDO.


form
   skip(1)
   " INSTRUCTION"                                               SKIP
   "   This list does not include customers whose contracts"    SKIP
   "   have been closed."                                       skip(2)

   "   Dates .........:" date1 " - " date2
help  "Dates You are intersted on"                              SKIP
   "   Customer Name .:" cust-name
help "Customer's Name or beginning of the name ..."             skip(1)
   "   Excel File ....:" excel
help "Do You want to create a tab separated ascii File (Y/N) ?" SKIP
   "   File Name .....:" exPaymFile
help "Name for the tab separated ascii file"                    skip(1)

with no-labels centered row 4 width 60 title " CRITERIAS FOR NO CALL CUSTOMERS " 
   side-labels OVERLAY FRAME haku.

form
  Customer.CustNum   column-label "Cust.nr"
  Customer.CustName  column-label "Cust. name"  format "x(30)"
  Salesman.SmName column-label "Salesman"    format "x(25)"
  Customer.ContrBeg  column-label "Contract"

with 15 down title " CUSTOMERS WITH NO Calls (" 
   + string(date1,"99-99-99") + " - " + string(date2,"99-99-99") + ") " 
   + string(today,"99-99-99") + " " 
   width 80 FRAME asi.

PAUSE 0 no-message.

DO FOR TMSUser:
   FIND TMSUser where 
        TMSUser.UserCode = katun
   no-lock no-error.
   exPaymFile = TMSUser.RepDir + "/" + "nocalls.txt".
END.

ehto = 9. RUN ufkey.

UPDATE date1 date2 cust-name excel WITH FRAME haku.
IF excel THEN UPDATE exPaymFile WITH FRAME haku.

ufk = 0. ehto = 3. RUN ufkey.
HIDE FRAME haku.
PAUSE 0 no-message.

IF excel THEN DO:
   OUTPUT STREAM excel TO value(exPaymFile).
   exhdr = "NO Calls REPORT FOR DAYS BETWEEN: " 
           + string(date1) + " - " + string(date2) + my-nl.
   PUT STREAM excel UNFORMATTED
      exhdr             
      my-nl
      my-nl
      "Salesman"         tab
      "Cust. nr"         tab
      "Cust. name"       tab
      "Contract started" tab
      my-nl.
END.

FOR EACH Customer no-lock where 
         Customer.ContrEnd <= pvm AND
   (if cust-name ne "" THEN index(CustName,cust-name) NE 0 ELSE TRUE),
   FIRST Salesman no-lock where
         Salesman.Salesman = Customer.Salesman
BY Customer.CustNum WITH FRAME asi.

   FIND FIRST FixCDR where 
              FixCDR.CustNum = Customer.InvCust AND 
              FixCDR.Date >= date1     AND 
              FixCDR.Date <= date2
   no-lock no-error.
   IF NOT AVAIL FixCDR THEN DO:

      DISP 
         Salesman.SmName
         Customer.CustNum 
         Customer.CustName 
         Customer.ContrBeg.

      IF excel THEN DO:
         PUT STREAM excel UNFORMATTED
            Salesman.SmName  tab
            Customer.CustNum    tab
            Customer.CustName   tab
            Customer.ContrBeg
            my-nl.
      END.

      IF FRAME-LINE = FRAME-DOWN THEN DO:
         message "Next page, hit SPACE - break, hit ENTER".
         READKEY. nap = keylabel(LASTKEY).
         if lookup(keylabel(lastkey),"enter,return") > 0 THEN LEAVE.
         CLEAR FRAME asi ALL.
         up FRAME-LINE - 1.
      END.
      ELSE DOWN.
   END.
END.

if nap = "" THEN DO:
   message "Press any key to return !".
   PAUSE no-message.
END.
HIDE FRAME asi no-pause.

