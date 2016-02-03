/* --------------------------------------------------------------------
  MODULE .......: smpwpr.p
  FUNCTION .....: Print web-passwords for reseller's salesmen
  APPLICATION ..: TMS
  AUTHOR .......: tk  
  CREATED ......: 16.07.04 
  MODIFIED .....: 
  VERSION ......: TeleF
  ------------------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/email.i}

DEF INPUT PARAMETER icResell LIKE Reseller.Reseller NO-UNDO.

DEF VAR lcEmail  LIKE TMSUser.Email     NO-UNDO.
DEF VAR count    AS I NO-UNDO.
form
 lcEmail  
with row 10  NO-LABELS CENTERED OVERLAY
   TITLE " Send passwords to "
FRAME rajat.


FIND FIRST TMSUser NO-LOCK WHERE
           TMSUser.UserCode = katun NO-ERROR.
IF AVAIL TMSUser THEN lcEMail = TMSUser.Email.
ELSE lcEMail = "".

output stream excel to /tmp/smpasswords.txt.

FOR EACH Salesman NO-LOCK WHERE
         Salesman.Reseller = icResell,
   FIRST username EXCLUSIVE-LOCK WHERE
         username.usercode = SalesMan.Salesman:

   count = count + 1.
   
   PUT STREAM excel UNFORMATTED
      Username.Usercode tab
      username.password skip.

END.   
        
output stream excel close.

if count = 0 then do:
   MESSAGE "Nothing to print !" VIEW-AS ALERT-BOX.
   RETURN.
end.   

ehto = 9. RUN ufkey.
UPDATE 
   lcEMail with frame rajat.

if lcEmail = "" OR keylabel(lastkey) = "f4" then return.

SendAttMail("'WEB Passwords'",lcEMail,"/tmp/smpasswords.txt").

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

