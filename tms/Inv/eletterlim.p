/* ------------------------------------------------------
  MODULI .......: ELETTERLIM.P
  TEHTAVA ......: CREATE INVOICES WITH ELETTER LIMITS
  SOVELLUTUS ...: NN
  TEKIJA .......: JR
  LUONTIPVM ....: 27.08.01
  MUUTOSPVM ....: 09.10.2001/aam don't start immediately after updating
                                 limits (show "start" -button),
                                 corrections TO the language used
                  25.10.2001/jr  CHOOSE all/not printed invoices
                  14.01.2002/aam old/new -invoices option removed,
                                 updation of cparam TO an TRANSACTION
                                 HELP FOR invoice nbrs etc. 
                  26.02.2002/aam credit invoices optionally 
                  04.12.2002/aam message about test flag
                  12.02.2003/aam LetterClass
                  09.10.2003/aam fEPLStart 
                  01.04.2004/aam printhouse selection
                  27.05.2004/aam invoice type, error from eletterinv
                  05.06.2006/aam use EPLLargeFile 
                  19.01.2007/mvi "Credit invoice" term -> "Credit note"
  VERSIO .......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Func/feplstart.i}

DEF VAR asno         AS INTEGER   FORMAT "zzzzzzz9"  INIT "0"        NO-UNDO.
DEF VAR lano1        AS INTEGER   FORMAT "zzzzzzz9"  INIT "1"        NO-UNDO.
DEF VAR lano2        AS INTEGER   FORMAT "zzzzzzz9"  INIT "99999999" NO-UNDO.
DEF VAR Lpvm         AS DATE      FORMAT "99-99-99"  INIT TODAY      NO-UNDO.
DEF VAR igroup       AS CHAR      FORMAT "x(8)"                      NO-UNDO.

DEF VAR dkk          AS INTEGER                                      NO-UNDO.
DEF VAR dvv          AS INTEGER                                      NO-UNDO.
DEF VAR xOk          AS LOGICAL                                      NO-UNDO.
DEF VAR LCnewfile    AS CHARACTER FORMAT "x(60)"                     NO-UNDO.
DEF VAR LCold        AS CHARACTER FORMAT "x(60)"                     NO-UNDO.
DEF VAR LLtila       AS LOG                                          NO-UNDO.

DEF VAR xCount         AS INT  NO-UNDO.
DEF VAR xCredit        AS LOG  NO-UNDO. 
DEF VAR lcTestFlag     AS CHAR NO-UNDO. 
DEF VAR liLetterClass  AS INT  NO-UNDO. 
DEF VAR llPrintService AS LOG  NO-UNDO.
DEF VAR llInvType      AS LOG  NO-UNDO. 
DEF VAR lcError        AS CHAR NO-UNDO.

ASSIGN
lcNewFile     = fCParamC("EPLLargeFile")
lcTestFlag    = fCParamC("EPLTest") 
liLetterClass = fCParamI("EPLInvLClass"). 

FORM
   SKIP(1)
"  This program writes an EPL file from invoices."
   SKIP
   " - From specified invoice numbers and customer numbers." SKIP
   " - From specified invoicing date and from specified invoicing group." SKIP
   SKIP (13)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " ELETTERS FROM INVOICES " +
        STRING(TODAY,"99-99-99") + " "
        FRAME valinta.

FORM
   
   lano1 
        LABEL "Invoice Numbers" 
        HELP "From invoice number"
   " - " 
   lano2 
        NO-LABEL 
        HELP "To invoice number" SKIP
   Lpvm  
        LABEL "Invoice Date .." 
        HELP "Invoice date"
   SKIP
   igroup 
        LABEL "Invoice Group ." 
        HELP "Customer's invoice group (EMPTY = all invoice groups)" SKIP 
   asno   
        LABEL "Customer Number" 
        HELP "Customer number, 0: ALL" SKIP
   LLtila 
        LABEL "Printing Status" 
        FORMAT "All/New"
        HELP "Print (A)ll or only (N)ew unprinted invoices" SKIP
   xCredit 
        LABEL "Credit Notes   "
        HELP "Print also credit notes" 
        FORMAT "Yes/No"
        SKIP
   llInvType 
        LABEL "Invoice Type .."
        HELP "(N)ormal invoices or (A)dvance payment invoices"
        FORMAT "Normal/Advance Payment"
        SKIP(1)
        
   LCnewfile 
        LABEL "File Name ....." 
        HELP "EPL file" SKIP
   liLetterClass 
        LABEL "Letter Class .."
        HELP "Letter class to be used for this batch"
        FORMAT "9"
        VALIDATE(INPUT liLetterClass >= 1 AND INPUT liLetterClass <= 4,
                 "Valid choices are 1-4") SKIP
   llPrintService 
        LABEL "Print House ..."
        HELP "Are invoice printed using (E)letter or (P)rintservice"
        FORMAT "PrintService/ELetter"
   SKIP
   WITH TITLE " INVOICES " SIDE-LABELS
   ROW 7 WIDTH 80 OVERLAY FRAME rajat.

VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

ASSIGN Lpvm           = DATE(MONTH(TODAY),1,YEAR(TODAY))
       llPrintService = FALSE
       llInvType      = FALSE.

limits:
REPEAT:

    DISPLAY lano1 lano2 Lpvm igroup asno LCnewfile liLetterClass 
            LLtila xCredit llPrintService llInvType
            WITH FRAME rajat.

    ehto = 9. RUN Syst/ufkey.

    UPDATE 
       lano1
       lano2 VALIDATE(INPUT lano2 >= INPUT lano1, "Not possible !")
       Lpvm
       igroup
       asno 
       LLtila
       xCredit 
       llInvType
       LCnewfile
       liLetterClass
       llPrintService
    WITH FRAME rajat EDITING:

        READKEY.
        IF LOOKUP(KEYLABEL(LASTKEY),"enter,return,tab,back-tab,f1") > 0
        THEN DO:
            PAUSE 0.
            IF FRAME-FIELD = "lano1" THEN 
            DO:
                IF INPUT lano1 > 1 THEN DISP INPUT lano1 @ lano2
                WITH FRAME rajat.
            END.
            ELSE IF FRAME-FIELD = "lano2" THEN DO:
                IF INPUT lano2 = INPUT lano1 THEN DO:
                    FIND FIRST Invoice NO-LOCK WHERE
                         Invoice.Brand  = gcBrand AND 
                         Invoice.InvNum = INPUT lano2 NO-ERROR.
                    IF AVAILABLE Invoice THEN DISPLAY
                        Invoice.InvDate  ;& Lpvm 
                        Invoice.CustNum ;& asno
                        WITH FRAME rajat.
                END.
            END.
        END.

        APPLY LASTKEY.
    END.

    task:
    repeat WITH FRAME rajat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN NEXT  limits.
      IF toimi = 8 THEN LEAVE limits.

      IF toimi = 5 THEN DO:
         IF fEPLStart(lcTestFlag) THEN LEAVE task.
      END.
      
    END.

    IF lano2 = 0 THEN lano2 = 999999.

    IF asno NE 0 THEN DO:
        FIND FIRST Customer WHERE 
                   Customer.Brand = gcBrand AND
                   Customer.CustNum = asno
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Customer THEN DO:
            MESSAGE "Invalid customer number !".
            BELL.
            RETURN.
        END.
    END.

    IF igroup NE "" THEN DO:
        FIND FIRST invgroup WHERE 
                   InvGroup.Brand = gcBrand AND
                   invgroup.InvGroup = igroup 
        NO-LOCK NO-ERROR.
        IF NOT AVAILABLE invgroup THEN DO:
            MESSAGE "Invalid invoice group !".
            BELL.
            RETURN.
        END.
     
    END.

    RUN Inv/eletterinv(INPUT lano1,
                   INPUT lano2,
                   INPUT Lpvm,
                   INPUT igroup,
                   INPUT asno,
                   INPUT llPrintService,
                   INPUT LLtila, 
                   INPUT xCredit,
                   INPUT (IF llInvType THEN 1 ELSE 4),
                   INPUT liLetterClass,
                   INPUT lcNewFile,
                   OUTPUT xCount,
                   OUTPUT lcError).

    MESSAGE "EPL file is done for" xCount "invoices !"
    VIEW-AS ALERT-BOX.

    LEAVE limits.

END.   /* limits */

HIDE MESSAGE       NO-PAUSE.
HIDE FRAME rajat   NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.

