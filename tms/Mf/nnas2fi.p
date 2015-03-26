/* =============================================================
MODULE ......: nnas2fi.p
TASK ........: creates a white list UPDATE File FOR exchange
APPLICATION .: TicketMaster
CREATED .....: 31.03.1998 pt
CHANGED .....: 30.06.1999 pt get OUTPUT directory from TMSParam
               01.10.1999 pt starting Position of substr(a-sub) is a VAR (q)
VERSION .....: M15
=============================================================== */

{commali.i}  

DEF STREAM whitelist.

def var cust-nr1    like Customer.CustNum no-undo format "zzzzzzz".
def var cust-nr2    like Customer.CustNum no-undo format "zzzzzzz".
def var date1       as da format "99-99-99"  NO-UNDO.
def var date2       as da format "99-99-99"  NO-UNDO.
def var ok          as lo format "Yes/No" NO-UNDO.
DEF VAR i           AS i  NO-UNDO.
def var wlname      as c  no-undo format "x(40)".
DEF VAR exdir       AS c  NO-UNDO.
def var append      as lo no-undo format "Append/Overwrite".
def var cust-nr     as i  no-undo format "zzzzzzz" EXTENT 20. 
def var cust-name   as c  no-undo format "x(30)" EXTENT 20.
DEF VAR cnlist      AS c  NO-UNDO.
DEF VAR q           AS i  NO-UNDO init 2.

form
skip(1)
"  Instruction: Now we make a text File of customers' A-sub. numbers"     skip  
"               that have been stored between those 2 dates given below." skip
SKIP
"               You can also enter a list of separate customer numbers. (F4)"
skip(1)
"               File's Name will be:"  wlname  no-label skip(2)
"               Customer numbers ...: "
cust-nr1 help "First customer no." no-label "-"
cust-nr2 help "Last customer no." NO-LABEL 
validate(input cust-nr2 >= input cust-nr1,"Check order of customer numbers !") SKIP
"               Dates when stored ..:" date1 no-label
help "Earliest Date of storage" "-" date2 NO-LABEL
help "Latest Date of storage" 
validate(input date2 >= input date1,"Check order of dates !") skip(1)
"               Print each A-sub no beginning from" q FORMAT "9" 
help "No. of position from which each A-sub n. is Printed into file"
validate (INPUT q > 0,"Must be 1 ... 9 !")
space(0) ". digit"
skip(5)

WITH NO-LABEL
   width 80 title " A-SUBSCRIBER NUMBERS INTO A TEXT File " FRAME rajat.


form
  " " cust-nr[ 1] cust-name[ 1]  SKIP
  " " cust-nr[ 2] cust-name[ 2]  SKIP
  " " cust-nr[ 3] cust-name[ 3]  SKIP
  " " cust-nr[ 4] cust-name[ 4]  SKIP
  " " cust-nr[ 5] cust-name[ 5]  SKIP
  " " cust-nr[ 6] cust-name[ 6]  SKIP
  " " cust-nr[ 7] cust-name[ 7]  SKIP
  " " cust-nr[ 8] cust-name[ 8]  SKIP
  " " cust-nr[ 9] cust-name[ 9]  SKIP
  " " cust-nr[10] cust-name[10]  SKIP
  " " cust-nr[11] cust-name[11]  SKIP
  " " cust-nr[12] cust-name[12]  SKIP
  " " cust-nr[13] cust-name[13]  SKIP
  " " cust-nr[14] cust-name[14]  SKIP
WITH
  centered row 3 overlay no-labels title " ENTER CUST.NUMBERS " FRAME cust.


FIND FIRST Customer no-lock no-error.
cust-nr1 = Customer.CustNum.
FIND LAST Customer no-lock no-error.
ASSIGN cust-nr2 = Customer.CustNum date1 = TODAY date2 = TODAY.


{tmsparam.i WlFileDir return}.  

/*
DO FOR kayt:
   FIND kayt where kayt.ka-tun = katun no-lock.
   exdir = kayt.txtdir.
END.

*/

wlname = TMSParam.CharVal  + "/" + "asub.txt".

PAUSE 0.

DISP wlname WITH FRAME rajat.

rajat:
repeat WITH FRAME rajat.
   ehto = 9. RUN ufkey.
   UPDATE wlname cust-nr1 cust-nr2 date1 date2 q.

toimi:
   repeat:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 7  ufk[4] = 942 ufk[5] = 15 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT rajat.

      IF toimi = 4 THEN DO WITH FRAME cust.
         ehto = 9. RUN ufkey.
         PAUSE 0.
         UPDATE cust-nr[1 FOR 14] EDITING:
            READKEY.
            nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               PAUSE 0 no-message.

               i = frame-index.
               ASSIGN FRAME cust cust-nr[i].
               if cust-nr[i] = 0 then disp "" @ cust-name[i].
               ELSE DO:
                  FIND Customer where Customer.CustNum = cust-nr[i]
                  no-lock no-error.
                  IF NOT AVAIL Customer THEN DO:
                     BELL.
                     message "Unknown customer !".
                     NEXT.
                  END. 
                  cust-name[i] = Customer.CustName.  
                  DISP cust-name[i].
               END.
            END.
            APPLY LASTKEY.
         END. /* EDITING */
         HIDE FRAME cust no-pause.
         ASSIGN cust-nr1 = 0 cust-nr2 = 0 date1 = 1/1/1995 date2 = TODAY. 
         disp "LIST " @ cust-nr1 cust-nr2 date1 date2
         WITH FRAME rajat.
         NEXT toimi.
      END.

      IF toimi = 8 THEN UNDO rajat, LEAVE rajat.
      IF toimi = 5 THEN DO:
         BELL.
         message "Are you SURE You want to create this File (Y/N) ? " UPDATE ok.
         IF ok THEN LEAVE toimi.
      END.
   END.

   cnlist = "".
   DO i = 1 TO 20.
      IF cust-nr[i] NE 0 THEN 
      assign cnlist = cnlist + string(cust-nr[i]) + ",".
   END.

   /* does this File exist already ? */
   IF search(wlname) NE ? THEN DO:
      BELL.
      message "File exists: (A)ppend or (O)verwrite ???" UPDATE append.
   END.

   IF append THEN
   OUTPUT STREAM whitelist TO value(wlname) append.
   ELSE
   OUTPUT STREAM whitelist TO value(wlname).  /* overwriting mode */
   i = 0.  IF cust-nr2 = 0 THEN cust-nr2 = 9999999.
   FOR EACH CLI no-lock where
            CLI.CustNum >= cust-nr1  AND  
            CLI.CustNum <= cust-nr2  AND

            (if cnlist ne "" THEN 
            lookup(string(CLI.CustNum),cnlist) > 0
            ELSE TRUE)                  AND

            CLI.Date    >= date1  AND
            CLI.Date    <= date2:

       PUT STREAM whitelist UNFORMATTED
       substr(CLI.CLI,q) 
       chr(13) chr(10).
       i = i + 1.

   END.
   OUTPUT STREAM whitelist CLOSE.
   MESSAGE
   "Totally" i "new A-sub. numbers transferred into File - press ENTER !".
   PAUSE no-message.
   assign cust-nr = 0 cust-name = "".  
   CLEAR FRAME cust no-pause.
   NEXT rajat.
END.
HIDE FRAME rajat no-pause.
HIDE MESSAGE no-pause.

