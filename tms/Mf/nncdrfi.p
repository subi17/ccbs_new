/* ---------------------------------------------------------------------------
  MODULE .......: NNCDRFI.P
  FUNCTION .....: FOR starting the nncdr -module that reads in CDRs from File
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 28.07.1998
  MODIFIED .....: 23.06.1999 kl DO NOT RETURN, QUIT
                  27.06.1999 kl smaller compiler blocks
                  21.10.1999 kl tape Version
                  27.07.2000 kl search FOR the File
                  01.04.2003 kl run smenscdr
                  30.06.2003 kl QUIT removed

  Version ......: M15
  -------------------------------------------------------------------------- */

{commpaa.i}
{tmsparam2.i}

def var pvmlog  as log  no-undo format "Yes/No" init TRUE.
def var ticfile as char no-undo format "x(40)".

def var ok      as log  format "Yes/No" NO-UNDO init FALSE.

DEF VAR cust-unknown  AS i  NO-UNDO.

/* Unknown customer must be in database before reading in CDRs  */
cust-unknown = fCParamI("UnknownCustomer").

IF NOT can-find(Customer where 
                Customer.CustNum = cust-unknown) THEN DO:
   BELL.
   message "'Unknown customer'" cust-unknown
   " is not in customer File !".
   message "Press ENTER !".
   PAUSE no-message.
   QUIT.
END.


form skip(1)
" NOTICE:   This module reads CDRs from asciifiles created by the"
"           FIDEX server. The File is read completely, no Date   "
"           limitations are allowed."                            skip(1)
    " FileName of Calls ..:"                  ticfile     skip(1)
    " Separated log File for each day (Y/N)?" pvmlog 
       help  "create a new log File for each day (Y/N) " skip(1)
with centered overlay row 4 title " READ IN CALL RECORDS "
     NO-LABELS width 70 FRAME loki.

ASSIGN ok = TRUE.

IF ok THEN DO:
   ehto = 9. RUN ufkey.
   UPDATE 
      ticfile 
      pvmlog 
   WITH FRAME loki EDITING.
      READKEY. nap = keylabel(LASTKEY).
      IF lookup(nap,poisnap) > 0 THEN DO:
         if frame-field = "ticfile" AND search(INPUT ticfile) = ? THEN DO:
            message "File '" + input ticfile + "' can't be found !"
            VIEW-AS ALERT-BOX error.
            NEXT.
         END.
      END.
      APPLY LASTKEY.
   END.

   if ticfile = "" OR ticfile = "OnLine" THEN LEAVE.

   ufk = 0. ehto = 3. 
   RUN ufkey. PAUSE 0.

   message "Are You SURE You want to start reading CDRs into database ?"
   UPDATE ok.

END.

HIDE FRAME loki.

IF ok THEN RUN smenscdr(pvmlog,cust-unknown,ticfile,false).

