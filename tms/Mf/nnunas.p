/* --------------------------------------------------------------------------
   MODULE ........: nnunas.p 
   FUNCTION ......: Listing of unknown a-subscribers
   APPLICATION ...: TicketMaster
   CREATED .......: 05.01.1998 pt
   MODIFIED ......: 06.05.1999 kl ExCode & incoming CGR
                    05.11.1999 kl only UNINVOICED Calls
                    02.01.2001 kl .i's
                    23.04.02 lp - added DECIMAL TO amount
   VERSION .......: M15
-------------------------------------------------------------------------- */

{commali.i}                      
{excel.i}
{tmsparam2.i}

DEF VAR i      AS i  NO-UNDO.
DEF VAR hdr    AS c  NO-UNDO.
DEF VAR exdir  AS c  NO-UNDO.
def var exFile as c  no-undo  init "chkasub.txt".
DEF VAR exName AS c  NO-UNDO.
DEF VAR date1  AS DA NO-UNDO.
DEF VAR date2  AS DA NO-UNDO.
DEF VAR net    AS DE NO-UNDO.
def var day1   as da no-undo format "99-99-99".
def var day2   as da no-undo format "99-99-99".
def var ukcust as i  no-undo format ">>>>>>9".

ASSIGN
    day2 = date(month(TODAY),1,year(TODAY)) - 1
   day1 = date(month(day2),1,year(day2))
   ukcust = fCParamI("UnknownCustomer").

DO FOR TMSUser:
   FIND FIRST TMSUser where TMSUser.UserCode = katun no-lock.
   exdir = TMSUser.RepDir.
END.   

form
skip(1)
"  Instruction:   This program creates a tab-separated ASCII File of all" 
skip
"                 UNINVOICED unidentified A-subscriber numbers that belong"
skip
"                 to 'unknown' customer." 
skip(2)
"                 Calls under Period .." day1 no-label "-" day2 no-label
skip(2)
"                 File's Name is" exName format "x(45)"      skip(7)
WITH 
   overlay width 80 no-labels title " LISTING OF UNKNOWN A-SUB NUMBERS "
   FRAME rajat.

ASSIGN
   exName = exdir + "/" + exFile
   hdr = "A-sub. no.,# of Calls,Value,First call,Last call,SW,CGR".

rajat:
repeat WITH FRAME rajat:
   PAUSE 0.
   DISP  exName.
   ehto = 9. RUN ufkey.

   UPDATE 
      day1 
      day2 validate(input day2 >= input day1, "Invalid order !")
      exName.

toimi:
   repeat WITH FRAME rajat:
      ASSIGN ehto = 0 ufk = 0 ufk[1] = 7 ufk[5] = 15 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  rajat.
      IF toimi = 8 THEN LEAVE rajat.
      IF toimi = 5 THEN LEAVE toimi.
   END.

   PAUSE 0.
   message "Processing ...".

   OUTPUT STREAM excel TO value(exName).           

   PUT STREAM excel UNFORMATTED  ynimi my-nl my-nl.

   PUT STREAM excel UNFORMATTED 
      "Calls from Unidentified A-subscriber numbers " +
       string(day1,"99.99.9999") + " - " + string(day2,"99.99.9999")
       my-nl my-nl.

   DO i = 1 TO num-entries(hdr).
      PUT STREAM excel UNFORMATTED entry(i,hdr) tab.
   END.
   PUT STREAM excel UNFORMATTED my-nl.

   FOR EACH FixCDR no-lock where 
            FixCDR.Date >= day1   AND
            FixCDR.Date <= day2   AND
            FixCDR.InvCust = ukcust
   BREAK
   BY FixCDR.CLI.

      net = FixCDR.GrossPrice - FixCDR.DiscValue.

      accumulate
       FixCDR.Date(sub-count BY FixCDR.CLI)

       net   (sub-total BY FixCDR.CLI).

      IF first-of(FixCDR.CLI) THEN ASSIGN
      date1 = FixCDR.Date date2 = date1.

      IF FixCDR.Date < date1 THEN date1 = FixCDR.Date.
      IF FixCDR.Date > date2 THEN date2 = FixCDR.Date.

      IF last-of(FixCDR.CLI) THEN DO:

         net = (accum sub-total BY FixCDR.CLI net).

         PUT STREAM excel UNFORMATTED
         /*
         "Subscriber " + FixCDR.CLI tab
         */
         FixCDR.CLI                             tab
         (accum sub-count BY FixCDR.CLI Date) tab
         string(net,">>>>>>>>9.99")                tab   
         string(date1,"99.99.9999")                tab
         string(date2,"99.99.9999")                tab
         FixCDR.ExCode                           tab
         FixCDR.TrunkIn                        my-nl.

      END.
   END.
   OUTPUT STREAM excel CLOSE.

   message "File is ready - press ENTER !".
   PAUSE no-message.
   LEAVE rajat.
END.
HIDE FRAME rajat no-pause.
HIDE MESSAGE no-pause.

