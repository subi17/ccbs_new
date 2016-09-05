/* ---------------------------------------------------------------------------
  MODULE .......: NNKOSU.P
  KUTSUVAMODULI : 
  FUNCTION .....: Book payments to invoices            
  SYSTEM .......: TMS
  AUTHOR .......: PT
  CREATED ......: 26.06.1997
  CHANGED ......:
                  09.04.01/aam use PaymCfg-table instead of ocrpaym.i00
                  21.05.01/aam filename can contain wild-cards
                  09.08.02/aam don't delete files and update PaymLog
                               in conversion modules (nnocko*); do that here,
                               use temp-table parameter instead of ascii-file,
                               use external procedure choosefile.p,
                               show payment file in the main screen etc.
                  11.09.03/aam brand 
                  03.05.05/aam logic to readpaym.p              
                  03.05.07/aam posting date to file name
                  10.05.07/aam info from readpaym
  Version ......: M15
  ------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i "new"}
/* temp-table */
{Ar/paymfile.i}
{Ar/paymtrans.i}
{Func/farplog.i}

ASSIGN tuni1 = "nnkosu"
       tuni2 = "".

def workfile worigin NO-UNDO
   field w-seq   AS I format "9"
   field worname AS C format "x(30)"
   field wlogpr  AS C format "x(8)"
   field wfile   AS C format "x(30)"
   field wacct   AS I format "zzzz9"
   field wconmod AS C format "x(12)".


DEF VAR rc          AS INT  NO-UNDO.
DEF VAR amt-o       AS INT  NO-UNDO.
DEF VAR op          AS INT  NO-UNDO.
DEF VAR llOk        AS LOG  NO-UNDO.
DEF VAR xPaymFile   AS CHAR NO-UNDO.
DEF VAR liFileType  AS INT  NO-UNDO. 
DEF VAR liRead      AS INT  NO-UNDO. 
DEF VAR ldtFileDate AS DATE NO-UNDO.
DEF VAR lcInfo      AS CHAR NO-UNDO.

form
   skip(1)
   "   This program reads in payments and books them onto invoices in A/R." 
       SKIP
   "   A summary (log) of payments is being printed accordingly."   
   skip(1)
   "   Payment file .....:" xPaymFile NO-LABEL FORMAT "X(55)"  SKIP
   "   Amount of payments:" rc       NO-LABEL FORMAT "zzzzzz" SKIP(1)
   skip(10)
with
   row 1 width 80 overlay color value(cfc) title color value(ctc)
   " " + ynimi + " " + qtitle + " " + string(pvm,"99-99-9999") +
   " " frame main.


cfc = "sel".  RUN Syst/ufcolor.p.
view frame main.

/* payment configuration file */
IF NOT CAN-FIND(FIRST PaymCfg WHERE PaymCfg.Brand = gcBrand) THEN DO:
   MESSAGE 
   "SYSTEM ERROR:"                   SKIP
   "Payment file configuration"  SKIP
   "has not been done"  SKIP(1)
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

amt-o = 0.
FOR EACH PaymCfg WHERE
         PaymCfg.Brand = gcBrand NO-LOCK:

   /* read all possible origin records */
   CREATE worigin.
   ASSIGN 
      amt-o           = amt-o + 1
      worigin.w-seq   = amt-o
      worigin.worname = PaymCfg.Origin
      worigin.wlogpr  = PaymCfg.PaymCfg
      worigin.wfile   = PaymCfg.PaymFile
      worigin.wconmod = PaymCfg.ConvMod
      worigin.wacct   = PaymCfg.PaymAccNum.
END.

MainLoop:
REPEAT:

   lChoosePaymFile:
   REPEAT:

      REPEAT:

         ASSIGN xPaymFile = ""
                rc        = 0.
         DISPLAY xPaymFile rc WITH FRAME main.

         /* select origin */
         RUN local-set-origin (OUTPUT op).
         IF op = 0 THEN DO:
            /* no origin was chosen */
            HIDE FRAME main.
            RETURN.
         END.

         FIND FIRST worigin WHERE worigin.w-seq = op.

         IF SEARCH(worigin.wconmod + ".r") = ? THEN DO:
            MESSAGE
            "SYSTEM ERROR:"                                  SKIP
            "Payment file conversion module"  worigin.wconmod SKIP
            "does not exist"
            VIEW-AS ALERT-BOX ERROR.
            NEXT. 
         END.

         /* are wild-cards used */
         IF INDEX(worigin.wFile,"*") GT 0 OR
            INDEX(worigin.wFile,"?") GT 0
         THEN RUN Mc/choosefile.p (wOrigin.wFile,
                              OUTPUT xPaymFile).
         ELSE ASSIGN xPaymFile = worigin.wfile.

         /* nothing chosen */
         IF xPaymFile = "" THEN DO:
            MESSAGE "Nothing was chosen."
            VIEW-AS ALERT-BOX WARNING.
            NEXT.
         END.

         IF SEARCH(xPaymFile) = ? THEN DO:
            MESSAGE 
            "The payment file"   SKIP
            xPaymFile    SKIP
            "does not exist"
            VIEW-AS ALERT-BOX ERROR.
         END.
         ELSE IF INDEX(xPaymFile,"*") GT 0 OR
                 INDEX(xPaymFile,"?") GT 0 OR
                 INDEX(xPaymFile,"<") GT 0
         THEN NEXT.
         ELSE LEAVE.   
      END.   

      /* delete possible old ones */
      EMPTY TEMP-TABLE ttPayment.

      DISPLAY xPaymFile WITH FRAME main. 

      ehto = 5.
      RUN Syst/ufkey.p.
      
      MESSAGE "Searching for new payments ...".

      /* type of payment file */
      CASE worigin.wconmod:
      WHEN "nnocko"     THEN liFileType = 1.
      WHEN "nnockott"   THEN liFileType = 2.
      WHEN "nnockointr" OR
      WHEN "nnockoakf"  THEN liFileType = 3.
      OTHERWISE liFileType = 0.
      END CASE. 
   
      RUN VALUE(worigin.wconmod) (INPUT  xPaymFile, 
                                  INPUT  worigin.wacct, 
                                  INPUT  wOrigin.WLogPr,
                                  OUTPUT TABLE ttPayment,
                                  output rc).

      HIDE MESSAGE NO-PAUSE. 

      IF rc = 0 then do:
         bell.
         MESSAGE
         "No payments were read from file" SKIP
         xPaymFile SKIP
         "Can file be handled as processed ?"
         VIEW-AS ALERT-BOX QUESTION
         BUTTONS YES-NO
         TITLE " No Payments Found "
         SET llOk.
         IF llOk THEN RUN pDeleteFile(xPaymFile). 
      END.
      ELSE LEAVE.
   END.

   IF rc >= 0 THEN disp rc with FRAME main.

   FIND FIRST ttPayment NO-ERROR.
   IF AVAILABLE ttPayment 
   THEN ldtFileDate = ttPayment.AccDate.
   ELSE ldtFileDate = TODAY.
   
   ASSIGN tila = true
          /* if "file" is chosen for printer then direct the output
             to a file and transfer it into a determined directory 
          */
          oso  = "-"  + worigin.wlogpr + "_"     +
                 STRING(YEAR(ldtFileDate),"9999") +
                 STRING(MONTH(ldtFileDate),"99")  +
                 STRING(DAY(ldtFileDate),"99")    +
                 "_" + STRING(TIME) + ".txt".
     
   {Syst/utuloste.i "return"}

   RUN Ar/readpaym.p (INPUT TABLE ttPayment,
                 xPaymFile,
                 liFileType,
                 TRUE,   /* show messages */
                 TRUE,   /* send mail     */
                 OUTPUT liRead,
                 OUTPUT lcInfo).

   /* update log */
   fCreateArplog(xPaymFile,
                 wOrigin.WLogPr,
                 katun). 

   /* delete or move the payment file to archive */
   IF liRead > 0 THEN RUN pDeleteFile(xPaymFile). 

END.

HIDE FRAME main no-pause.
HIDE MESSAGE.


PROCEDURE local-set-origin:

    DEF OUTPUT PARAMETER oiChosen AS INT NO-UNDO.

    FORM
       worname LABEL "Origin"
               HELP "Origin of payments" 
       wacct   LABEL "AccNo" FORMAT ">>>>>>>9"
    WITH 
    CENTERED OVERLAY amt-o DOWN ROW 9 TITLE " CHOOSE ORIGIN "
    FRAME origin.

    PAUSE 0.
    VIEW FRAME origin.

    FOR EACH worigin WITH FRAME origin:
        DISP worigin.worname worigin.wacct.
        IF worigin.w-seq = amt-o THEN UP amt-o - 1.
        ELSE DOWN.
    END.
    ASSIGN ufk = 0 ufk[5] = 11 ufk[8] = 8 ehto = 3.
    RUN Syst/ufkey.p.
    CHOOSE ROW worigin.worname no-error {Syst/uchoose.i}
    WITH FRAME origin.   

    IF keylabel(lastkey) = "f8" THEN oiChosen = 0.
    ELSE                             oiChosen = FRAME-LINE(origin).

    HIDE FRAME origin.
END.


