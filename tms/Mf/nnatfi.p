/* -----------------------------------------------
  MODULE .......: NNATFI.P
  FUNCTION .....: A-tilaajanumeroiden vienti tiedostoon
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 12.09.96
  MODIFIED  ....: 07.11.97 pt OUTPUT directory now home2
                  04.09.00 kl Pref check added
                  11.09.02 jp header for report
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Func/tmsparam2.i}

DEF VAR ufkey  AS LOG NO-UNDO.

DEF VAR i AS dec.

DEF VAR order    AS lo NO-UNDO.
def var ok       as lo no-undo format "Yes/No".
def var custnr   as i  no-undo format "zzzzz9".
def var area     as c  no-undo format "x(16)".
DEF VAR criteria AS lo NO-UNDO.
def var fname    as c  no-undo format "x(30)".
DEF VAR t1epref  AS c  NO-UNDO.
DEF VAR xcust    AS i  NO-UNDO.

form
   skip(1)
"   INSTRUCTION: This module creates a tab separated ASCII file"    skip
"                of all customers a-sub numbers using the criteria" skip
"                determined below."                                 skip(1)
"                Also the PREFIX HOSTING CLIs will be reported."
   skip(11)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + " A-SUBCRIBER NUMBERS TO File " +
        string(pvm,"99-99-99") + " "
        FRAME valinta.

view FRAME valinta.
PAUSE 0 no-message.

DO FOR TMSUser.
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   fname = TMSUser.RepDir + "/asublist.txt".
END.

ASSIGN 
   criteria = TRUE
   t1epref  = fCParamC("Tele1Pref").

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, RETURN:
      IF criteria THEN DO:
         ehto = 9. RUN ufkey.p.
         UPDATE
           order label "Order by ......." format "Customer/A-Sub"
             help   "Choose listing order:  (C/A)" SKIP
           custnr  label "Customer number "
             validate(custnr = 0 OR can-find(Customer where 
                                             Customer.CustNum = custnr),
                                             "Unknown customer !")
             help "Choose customer number, empty for all" SKIP
           area label "Areacode ......."
             help  "Areacode, starting of it OR empty"    SKIP
           fname label  "File Name ......" fname
         with centered overlay side-labels title " Criteria for File "
         ROW 9 FRAME rajat.

         ASSIGN
         criteria = FALSE
         ufkey      = TRUE.
      END.
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      READKEY. nap = keylabel(LASTKEY).

      if lookup(nap,"1,f1") > 0 THEN DO:
         criteria = TRUE.
         NEXT toimi.
      END.
      else if lookup(nap,"5,f5") > 0 THEN DO:
         ok = FALSE.
         message "Are You sure You want to start processing ?" UPDATE ok.
         IF ok THEN LEAVE toimi.
         ELSE       NEXT  toimi. 
      END.
      else if lookup(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
   END. /* toimi */

message "Processing data ...".

/* Kirjoitetaan AINA tahan samaan paikkaa, samalle nimelle, paalle */
OUTPUT STREAM excel TO value(fname).
PUT STREAM excel UNFORMATTED
          "Prefix"                                           tab
          "CLISER from" tab
          "Customer Number"                                  tab
          "Customer Name"                                      my-nl.

FOR EACH CLISer no-lock where
    (IF custnr NE 0 THEN CustNum = custnr ELSE TRUE) AND
    (if area ne "" THEN CLIFrom BEGINS area ELSE TRUE),

  FIRST Customer where 
        Customer.CustNum = CLISer.CustNum no-lock 

  BREAK BY
    (if order then string(CLISer.CustNum,"9999999") ELSE CLISer.CLIFrom).

    DO i = decimal(CLIFrom) TO decimal(CLITo):
       PUT STREAM excel UNFORMATTED
          t1epref                                            tab
          string(i,fill("9",length(CLIFrom))) format "x(12)" tab
          CLISer.CustNum                                     tab
          Customer.CustName                                    my-nl.
    END.

    if last-of(if order then string(CLISer.CustNum,"9999999") 
             ELSE CLISer.CLIFrom) THEN DO:

      FIND FIRST rsoper where
                 rsoper.CustNum = Customer.CustNum
      no-lock no-error.

      IF AVAIL rsoper THEN DO:

         FIND FIRST OperIndir where
                    OperIndir.Operator = rsoper.Operator
         no-lock no-error.

         FOR EACH CLIPref where
                  CLIPref.Pref = OperIndir.Prefix:

            PUT STREAM excel UNFORMATTED
               CLIPref.Pref tab
               CLIPref.CLI  tab
               Customer.CustNum   tab
               Customer.CustName  my-nl.

         END.

      END.

   END.

END.

OUTPUT STREAM excel CLOSE.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

