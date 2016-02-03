/* ------------------------------------------------------
  MODULI .......: INVSTAT_F.P
  TEHTAVA ......: Invoice statistics
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 22.04.2002
  MUUTOSPVM ....: 13.06.2002 JP/ E-mail Option
                  13.12.2002/aam invoice type added 
                  04.02.2003/aam account limit,
                                 interval
                  18.05.2004/aam longer format for account               
                  29.06.2004/aam "credited" by default yes
                  19.01.2007/mvi "credit invoice" term -> "credit note"
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/email.i}

def var xDate1     as date no-undo.
def var xDate2     as date no-undo. 
def var xState1    as int  no-undo.
def var xState2    as int  no-undo. 
def var xCredit    as log  no-undo.
def var xFile      as char no-undo. 
def var xCount     as int  no-undo. 
def var xemail     AS CHAR No-UNDO.

DEF VAR liAcct1      AS INT   NO-UNDO FORMAT ">>>>>9".
DEF VAR liAcct2      LIKE liAcct1.
DEF VAR liInvType1   AS INT   NO-UNDO FORMAT "9".
DEF VAR liInvType2   LIKE liInvType1.
DEF VAR llInterval   AS LOG   NO-UNDO. 
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR lcAccGrp     AS CHAR  NO-UNDO. 



FORM
   SKIP(3)
   "Create a tab separated statistic file from invoices." 
         AT 10 
         SKIP(1)

   xDate1  
         AT 10 
         LABEL  "Invoice dates .."              
         FORMAT "99-99-9999"
      "-"
      xDate2 
         NO-LABEL 
         FORMAT "99-99-9999"
         SKIP

   liInvType1 at 10 
         label  "Invoice Types .."
         help "Invoice types from"
   "-"
   liInvType2     
         no-label 
         help "Invoice types to" skip

   xState1 
         AT 10 
         LABEL "Invoice status ."
         FORMAT "9"
         HELP "0=not printed"
      "-"
      xState2
         NO-LABEL
         FORMAT "9"
         SKIP
   liAcct1 
         AT 10
         LABEL "Accounts ......."
         HELP "Accounts"
      "-"
      liAcct2 
         NO-LABEL
         HELP "Accounts" 
         SKIP
   xCredit
         AT 10
         LABEL "Credit notes    "
         FORMAT "Yes/No"
         HELP "Take credited and credit notes"
         SKIP
   llInterval 
         AT 10
         LABEL "Show interval .."
         FORMAT "Yes/No"
         HELP "Show interval for contract and single fees"
         SKIP(1)
   xFile AT 10 
         LABEL "Output file ...."
         FORMAT "x(40)"
   xEmail AT 10 LABEL "Email to ......." FORMAT "x(50)"  
         HELP "Email Address, EMPTY = Only file "
   SKIP(3)   
   WITH ROW 1 SIDE-LABELS WIDTH 79
        TITLE " " + ynimi + " INVOICE STATISTICS " +
        STRING(TODAY,"99-99-99") + " "                   
        FRAME valinta.

ASSIGN xDate1  = today
       xDate2  = today
       liInvType2 = 9
       liAcct2 = 999999
       xState1 = 0
       xState2 = 3
       xCredit = true
       xFile   = "/tmp/inv_stat_" + 
                 string(year(today)) +
                 string(month(today),"99") +
                 ".txt".

pause 0.
display xDate1  xDate2
        liInvType1 liInvType2
        xState1 xState2
        liAcct1 liAcct2
        xCredit
        llInterval
        xFile
with frame valinta.

limits:
REPEAT with frame valinta:


    ehto = 9. RUN ufkey.

    repeat with frame valinta on endkey undo, leave:
        UPDATE 
        xDate1
        xDate2 VALIDATE(INPUT xDate2 >= INPUT xDate1, "Check the values")
        liInvType1
        liInvType2
               validate(input liInvType2 >= input liInvType1,
                            "Check the values")                
        xState1
        xState2 VALIDATE(INPUT xState2 >= INPUT xState1, "Check the values")
        liAcct1
        liAcct2 VALIDATE(INPUT liAcct2 >= INPUT liAcct1, 
                            "Check the values")
        xCredit
        llInterval
        xFile
        xemail
        with frame valinta editing:

            readkey. nap = keylabel(lastkey).

            IF nap = "F9" AND 
               (INDEX(FRAME-FIELD,"liInvType") > 0 OR
                INDEX(FRAME-FIELD,"xState") > 0)
            THEN DO:

               lcFrameField = FRAME-FIELD.

               IF LOOKUP(FRAME-FIELD,"liInvType1,liInvType2") > 0
               THEN DO:

                  RUN h-tmscodes(INPUT "Invoice",  /* TableName*/
                                       "InvType", /* FieldName */
                                       "Report", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DO WITH FRAME valinta:
                     IF lcFrameField  = "liInvType1" 
                     THEN DISPLAY INTEGER(lcCode) ;& liInvType1.
                     ELSE DISPLAY INTEGER(lcCode) ;& liInvType2.
                  END.
               END.

               ELSE IF LOOKUP(FRAME-FIELD,"xState1,xState2") > 0
               THEN DO:

                  RUN h-tmscodes(INPUT "Invoice",  /* TableName*/
                                       "PrintState", /* FieldName */
                                       "Report", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" THEN DO:
                     IF lcFrameField = "xState1" 
                     THEN DISPLAY INTEGER(lcCode) ;& xState1.
                     ELSE
                     IF lcFrameField = "xState2" 
                     THEN DISPLAY INTEGER(lcCode) ;& xState2.
                  END.
               END.

               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.


            ELSE IF lookup(keylabel(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME valinta:
               HIDE MESSAGE.

            END.

            APPLY LASTKEY.
        END. 

        leave.
    end.

    display xDate1  xDate2
            liInvType1 liInvType2
            xState1 xState2
            xCredit
            xFile xemail.

    task:
    repeat with frame valinta:

      assign ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      run ufkey.

      if toimi = 1 then next  limits.

      if toimi = 8 then leave limits.

      if toimi = 5 then leave task.
    end.


    run invstat (xDate1,
                 xDate2,
                 liInvType1,
                 liInvType2,
                 xState1,
                 xState2,
                 liAcct1,
                 liAcct2,
                 xCredit,
                 llInterval,
                 xFile,
                 OUTPUT xCount). 

    IF xEmail ne "" THEN DO:
        ASSIGN
           xMailAddr   = xEmail
           xMailSubj   = "INVOICE_STATISTICS".
       SendMail(xfile,xfile).
    END.

    MESSAGE "File is done for" xCount "invoices."
    VIEW-AS ALERT-BOX
    TITLE " Statistics done ".

    LEAVE limits.

END.   /* limits */

HIDE MESSAGE       NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.
