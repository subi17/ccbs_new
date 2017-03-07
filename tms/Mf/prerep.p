/* ----------------------------------------------------------------------
  MODULE .......: prerep.p
  TASK .........: EXPORTING THE PRESELECT DATA TO TAB-SEPARATED PaymFile
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 10-03-00
  CHANGED ......: 29-06-00 pt loop structure, dates 
                  18-10-00 ht rsopers-option added
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/excel.i}


DEF VAR pecode    AS C  NO-UNDO.
DEF VAR ok        AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR prerep    AS C  NO-UNDO FORMAT "x(40)".
DEF VAR ft        AS LO NO-UNDO.
DEF VAR coun      AS I  NO-UNDO.
DEF VAR coun2     AS I  NO-UNDO.
DEF VAR sdate1    AS DA NO-UNDO FORMAT "99-99-99".
DEF VAR sdate2    AS DA NO-UNDO FORMAT "99-99-99".
DEF VAR cdate1    AS DA NO-UNDO FORMAT "99-99-99".
DEF VAR cdate2    AS DA NO-UNDO FORMAT "99-99-99".
DEF VAR newrecs   AS LO NO-UNDO FORMAT "Yes/No".
DEF VAR rsopers   AS LO NO-UNDO FORMAT "Yes/No".

{Func/tmsparam.i PreselRep RETURN}. prerep = TMSParam.CharVal.

form 
 skip(1)
 "    NOTE:  This program creates a tab-separated PaymFile with     " SKIP
 "           all customers having any preselection!             " skip(1)
 "           Request Sent to Telia " sdate1
help "Earliest subscription day" "-" sdate2 help "Latest subscription day"
SKIP
 "           Confirmation dates ..." cdate1 help "Earliest confirmation day"
 "-" cdate2 help "Latest confirmation day" 
 "           Unsent (new) requests " newrecs
 help "Shall NEW, unsent requests be Printed (Y/N)"
 SKIP
 "           All forwarded requests" rsopers
 help "Shall all forward sended requests be Printed (Y/N)"

 skip(2) 


 "           FileName ..: " prerep 
help "Name of output file"
skip(6)

 WITH  OVERLAY ROW 1 WIDTH 80
    COLOR VALUE(cfc)

    TITLE COLOR VALUE(ctc) 
    " " + ynimi + "   " + 
    " REPORT OF PRESELECT CUSTOMERS    "
    + STRING(pvm,"99-99-99") + " "

    NO-LABELS 
    FRAME main.


sdate1 = 1/1/2000. sdate2 = TODAY.
cdate1 = 1/1/2000. cdate2 = TODAY.


PAUSE 0.

MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN Syst/ufkey.p.

UPDATE
  sdate1 sdate2 validate(input sdate2 >= input sdate1,"Invalid order !")
  cdate1 cdate2 validate(input cdate2 >= input cdate1,"Invalid order !")
  newrecs
  rsopers
  prerep

WITH FRAME main. 

Action:
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 795
      ufk[8] = 8.
      RUN Syst/ufkey.p.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:

         ok = FALSE.
         MESSAGE "Do You REALLY want to CREATE REPORT (Y/N) ?" UPDATE ok.
         IF NOT ok THEN NEXT Action.

         LEAVE Action.
      END.
   END. /* Action */      

   OUTPUT STREAM excel TO value(prerep). 

   PUT STREAM excel UNFORMATTED
   ynimi         skip(1)
   "CARRIER PRESELECTION RECORDS" SKIP
   "Sent:"    tab 
      sdate1 format "99.99.9999" " - " sdate2 format "99.99.9999" my-nl
   "Confirmed:" tab
      cdate1 format "99.99.9999" " - " cdate2 format "99.99.9999" my-nl 
   my-nl   
   "CustNo"      tab
   "CustName"    tab
   "ResellCode"  tab
   "ResellName"  tab
   "SubsNo"      tab
   "PreSType"    tab
   "ErrorCode"   tab
   "SentDate"    tab 
   "ConfDate"    tab
   "OutFileSeq"  tab
   "InFileSeq"   tab
   my-nl.

   coun = 0.


   FOR
   EACH   Presel NO-LOCK WHERE

          (IF NOT newrecs THEN 
          Presel.SentDate NE ? ELSE TRUE) AND

          (IF Presel.SentDate NE ? THEN 
          Presel.SentDate >= sdate1 AND
          Presel.SentDate <= sdate2 ELSE TRUE)  AND

          (IF Presel.ConfDate NE ? THEN 
          Presel.ConfDate >= cdate1 AND
          Presel.ConfDate <= cdate2 ELSE TRUE),



          Customer OF Presel NO-LOCK

   BREAK
   BY Presel.CustNum
   BY Presel.CLI:       

      IF NOT rsopers THEN DO:
         FIND FIRST rsoper WHERE rsoper.CustNum  = Presel.CustNum
                           AND   rsoper.Reseller = 1 /* sharing, NOT host */
         NO-LOCK NO-ERROR.

         IF AVAILABLE rsoper THEN NEXT.  /* belongs TO any reseller */
      END.

      IF FIRST-OF(Presel.CustNum) THEN DO:
         FIND Reseller WHERE Reseller.Reseller = Customer.Reseller NO-LOCK NO-ERROR.
         PUT STREAM excel UNFORMATTED
            Customer.CustNum    tab
            Customer.CustName   tab
            Customer.Reseller   tab.
         IF AVAIL Reseller THEN PUT STREAM excel UNFORMATTED   
            Reseller.RsName.
         PUT STREAM excel UNFORMATTED tab.
         ft = TRUE.
         coun = coun + 1.
      END.
      ELSE PUT STREAM excel UNFORMATTED tab tab tab tab.

      pecode = STRING(Presel.ReturnCode).
      IF Presel.FileSeq2 = 0 THEN pecode = "".

      PUT STREAM excel UNFORMATTED
         Presel.CLI      tab
         Presel.PsType     tab
         pecode            tab
         Presel.SentDate FORMAT "99.99.9999"  tab
         Presel.ConfDate FORMAT "99.99.9999"  tab
         Presel.FileSeq1   tab
         Presel.FileSeq2
         my-nl.

      coun2 = coun2 + 1.
   END. 

   OUTPUT STREAM excel CLOSE.

   unix silent VALUE("chmod 777 " + prerep).

   MESSAGE 
   SKIP
   "TOTALLY" coun "CUSTOMERS  with" SKIP 
   coun2 "CLIs were Printed."
   VIEW-AS ALERT-BOX INFORMATION.

   LEAVE main.
END. /* MAIN */
HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.

