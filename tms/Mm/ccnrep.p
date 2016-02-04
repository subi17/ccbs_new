/* -----------------------------------------------------------------
  MODULE .......: ccnrep.p
  TASK .........: Printing criteria for ccnrep (invasub)
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.09.03
  CHANGED ......: 
  Version ......:
  ------------------------------------------------------------------ */

{Syst/commali.i}  
{Func/cparam2.i}

DEF VAR ldtDate  AS DATE NO-UNDO.
DEF VAR ldtdate2 AS DATE NO-UNDO.
DEF VAR lcFile   AS CHAR NO-UNDO.
DEF VAR lcTrans  AS CHAR NO-UNDO.
DEF VAR liInvCnt AS INT  NO-UNDO.

form
   skip(6)
   ldtdate COLON 20
      FORMAT "99-99-99"
      LABEL "Invoicing dates" 
   "-" 
   ldtdate2 
      FORMAT "99-99-99"  
      NO-LABEL
      SKIP(1)
   lcFile COLON 20 
      LABEL "File Name"
      FORMAT "X(50)"
      SKIP
   lcTrans COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(50)" 
   SKIP(7)
WITH
   WIDTH 80 ROW 1 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " CCN REPORT " + string(pvm,"99-99-99") + " "
   SIDE-LABELS FRAME start.

ASSIGN 
   ldtdate  = TODAY
   ldtdate2 = ldtDate
   lcFile   = fCParamC("CCNReportFile")
   lcTrans  = fCParamC("CCNReportTrans").

IF lcFile = ? OR lcTrans = ? THEN DO:
   MESSAGE "File configuration missing"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor.

CRIT:
repeat WITH FRAME start:

   PAUSE 0.
   DISP ldtDate ldtDate2
        lcFile 
        lcTrans WITH FRAME start.
   
   ehto = 9. RUN Syst/ufkey.

   UPDATE
      ldtdate    validate(ldtdate ne ?,"First Date missing")
      ldtdate2   validate(input ldtdate2 >= input ldtdate,"Invalid order !")
      lcFile
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

   task:
   repeat WITH FRAME start:

      PAUSE 0.
      DISP ldtDate ldtDate2
           lcFile 
           lcTrans WITH FRAME start.
      
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.

      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         IF lcFile = "" THEN DO:
            MESSAGE "File name is mandatory"
            VIEW-AS ALERT-BOX ERROR.
            NEXT.
         END.
         
         RUN Mm/ccreport.p(ldtdate,
                        ldtdate2,
                        1,
                        lcFile,
                        0,
                        0,
                        "",
                        OUTPUT liInvCnt).

         MESSAGE "Printing complete" VIEW-AS ALERT-BOX TITLE " DONE ". 
            
         LEAVE CRIT.
      END.
   END.
   
END.

HIDE FRAME start no-pause.

