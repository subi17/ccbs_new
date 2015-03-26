/* -----------------------------------------------
  MODULE .......: NNASDA.P
  FUNCTION .....: Customer CREATE & UPDATE dates AND users
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 03-02-98
  MODIFIED .....: 03.03.03 tk tokens
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'customer'}

DEF INPUT PARAM  CustNum LIKE Customer.CustNum.

def var cr-name as c no-undo format "x(30)".
def var up-name as c no-undo format "x(30)".

FIND FIRST Customer where
           Customer.CustNum = CustNum
no-lock no-error.

form
   skip(1)
   "  Customer:"         Customer.CustName         skip(1)
   "     created ....:"  Customer.CreDate         SKIP
   "     created by .:"  Customer.CreUser cr-name skip(1)
   "     updated ....:"  Customer.UpdDate         SKIP
   "     updated by .:"  Customer.UpdUser up-name skip(1)
WITH ROW 5 /*width 40*/ centered OVERLAY
   NO-LABEL COLOR value(cfc) TITLE COLOR value(ctc)
   " Who/When Created/Changed This Customer " FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser   where
              TMSUser.UserCode = Customer.CreUser
   no-lock no-error.
   IF AVAIL TMSUser THEN ASSIGN cr-name = TMSUser.UserName.
END.

DO FOR TMSUser.
   FIND FIRST TMSUser   where
              TMSUser.UserCode = Customer.UpdUser
   no-lock no-error.
   IF AVAIL TMSUser THEN ASSIGN up-name = TMSUser.UserName.
END.

DISP Customer.CustName
     CreDate CreUser cr-name
     UpdDate UpdUser up-name
WITH FRAME frm.


message "Press ENTER !".
PAUSE no-message.

HIDE FRAME frm no-pause.

