/* ----------------------------------------------------------------------
  MODULE .......: VATCode.P
  TASK .........: UPDATE VAT Codes
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 08-06-99
  CHANGED ......: 08.10.99 jp urights added
                  08.12.99 pt order 2 = VATPercent
                  29.04.02 tk Eventlogging added
                  27.02.03 tk tokens
                  04.05.04/aam account with 6 digits
                  13.11.06/aam account with 8 digits,
                               TaxZone and TaxClass
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'vatcode'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhVATCode AS HANDLE NO-UNDO.
   lhVATCode = BUFFER VATCode:HANDLE.
   RUN StarEventInitialize(lhVATCode).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhVATCode).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liVATCode  LIKE VATCode.VATCode  NO-UNDO.
DEF VAR VATPercent LIKE VATCode.VATPerc NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 4.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR crundo       AS LOG                    NO-UNDO INIT FALSE.
DEF VAR lcZoneName   AS CHAR                   NO-UNDO.
DEF VAR lcClassName  AS CHAR                   NO-UNDO.
DEF VAR lcTaxZone    AS CHAR                   NO-UNDO.
DEF VAR lcTaxClass   AS CHAR                   NO-UNDO.

DEF BUFFER bVatCode FOR VatCode.

form
    VATCode.VATCode      /* COLUMN-LABEL FORMAT */
    VATCode.VCName     FORMAT "X(25)"
    VATCode.TaxZone    FORMAT "x(5)"
    VATCode.TaxClass   FORMAT "x(5)"
    VATCode.VATPerc
    VATCode.AccNum     FORMAT ">>>>>>>9"
    VATCode.ToDate
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
         "  VAT CODES  " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    VATCode.VATCode   COLON 20
    VATCode.VCName    COLON 20
       VALIDATE(VatCode.VCName ne "","Missing VatCode name!")

    VATCode.FromDate COLON 20
    VATCode.ToDate   COLON 20
    
    VATCode.TaxZone   COLON 20
       VALIDATE(CAN-FIND(TaxZone WHERE TaxZone.TaxZone = INPUT VatCode.TaxZone),
                "Unknown tax zone")
    lcZoneName      
       FORMAT "X(30)" 
       NO-LABEL

    VATCode.TaxClass  COLON 20
       VALIDATE(CAN-FIND(TaxClass WHERE 
                         TaxClass.TaxClass = INPUT VatCode.TaxClass),
                "Unknown tax zone")
    lcClassName      
       FORMAT "X(30)" 
       NO-LABEL
    
    VATCode.VATPerc   COLON 20
    VATCode.AccNum    COLON 20  LABEL "Account" FORMAT ">>>>>>>9"
    Account.AccName   COLON 20
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr side-labels FRAME lis.

form /* seek VATAmt Code  BY  VATCode */
    liVATCode
    help "Enter VAT Code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek VATAmt Code  BY VCName */
    VATPercent
    help "Enter VAT Percent"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND %%% "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek VAT Code  BY taxzone */
    lcTaxZone
    help "Enter Tax zone"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND ZONE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek VAT Code  BY taxzone */
    lcTaxClass
    help "Enter Tax class"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CLASS "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.


FUNCTION fZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   lcZoneName = "".
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN lcZoneName = TaxZone.TZName. 
   
END FUNCTION.

FUNCTION fClassName RETURNS LOGIC
   (icTaxClass AS CHAR):
   
   lcClassName = "".
   
   FIND TaxClass WHERE TaxClass.TaxClass = icTaxClass NO-LOCK NO-ERROR.
   IF AVAILABLE TaxClass THEN lcClassName = TaxClass.TCName. 
   
END FUNCTION.



cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.


RUN local-find-first.

IF AVAILABLE VATCode THEN ASSIGN
   memory       = recid(VATCode)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No VAT Codes available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a VATCode  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      DO WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSAction:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR VATCode.VATCode
           VALIDATE
              (VATCode.VATCode = 0 OR                        
              NOT CAN-FIND(VATCode using  VATCode.VATCode),
              "VAT Code " + string(INPUT VATCode.VATCode) +
              " already exists !").
           IF INPUT VATCode.VATCode = 0 THEN LEAVE ADD-ROW.
           CREATE VATCode.
           ASSIGN
           VATCode.VATCode = INPUT FRAME lis VATCode.VATCode.

           RUN local-update-record.

           IF crundo = TRUE THEN DO:
              crundo = FALSE.
              UNDO add-row, LEAVE add-row.
           END.

           ASSIGN
           memory = recid(VATCode)
           xrecid = memory.
        END.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhVATCode).

      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST VATCode
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VATCode THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND VATCode WHERE recid(VATCode) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE VATCode THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(VATCode).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35 ufk[2]= 789 ufk[3]= 1093 ufk[4]= 1094
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW VATCode.VATCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VATCode.VATCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW VATCode.VATPerc {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VATCode.VATPerc WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW VATCode.TaxZone {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VATCode.TaxZone WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW VATCode.TaxClass  {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VATCode.TaxClass WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND VATCode WHERE recid(VATCode) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE VATCode THEN
              ASSIGN FIRSTrow = i memory = recid(VATCode).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE VATCode THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(VATCode)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE VATCode THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(VATCode).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND VATCode WHERE recid(VATCode) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE VATCode THEN DO:
           memory = recid(VATCode).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE VATCode THEN memory = recid(VATCode).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND VATCode WHERE recid(VATCode) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       VATCode = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE liVATCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF liVATCode <> 0 THEN DO:
          FIND FIRST VATCode WHERE VATCode.VATCode >= liVATCode
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VATCode THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some vatcode/vc-code was found */
          ASSIGN order = 1 memory = recid(VATCode) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       VATPercent = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE VATPercent WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF VATPercent <> 0 THEN DO:
          FIND FIRST VATCode WHERE 
                     VATCode.VATPerc >= VATPercent
          USE-INDEX VATPer /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VATCode THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some vatcode/vc-name was found */
          ASSIGN order = 2 memory = recid(VATCode) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       VATPercent = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE lcTaxZone WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF lcTaxZone > "" THEN DO:
          FIND FIRST VATCode WHERE 
                     VATCode.TaxZone >= lcTaxZone
          USE-INDEX TaxZone NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VATCode THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some vatcode/vc-name was found */
          ASSIGN order = 3 memory = recid(VATCode) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* Search BY col 4 */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       VATPercent = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE lcTaxClass WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.

       IF lcTaxClass > "" THEN DO:
          FIND FIRST VATCode WHERE 
                     VATCode.TaxClass >= lcTaxClass
          USE-INDEX TaxZone NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VATCode THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some vatcode/vc-name was found */
          ASSIGN order = 4 memory = recid(VATCode) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-4 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          VATCode.VATCode 
          VATCode.VCName 
          VATCode.VATPerc
          VATCode.AccNum.

       RUN local-find-NEXT.
       IF AVAILABLE VATCode THEN memory = recid(VATCode).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE VATCode THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(VATCode).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          VATCode.VATCode 
          VATCode.VCName 
          VATCode.VATPerc     
          VATCode.AccNum.
          
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhVATCode).

           DELETE VATCode.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST VATCode
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY VATCode.VATCode.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhVATCode).

       RUN local-update-record.

       IF crundo = TRUE THEN DO:
          crundo = FALSE.
          HIDE FRAME lis NO-PAUSE.
          UNDO, Leave.

       END.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhVATCode).

       HIDE FRAME lis NO-PAUSE.
       RUN local-disp-row.
       xrecid = recid(VATCode).
     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(VATCode) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(VATCode) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND VATCode WHERE recid(VATCode) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND VATCode WHERE recid(VATCode) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST VATCode
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST VATCode USE-INDEX VATPerc
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST VATCode USE-INDEX TaxZone 
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST VATCode USE-INDEX TaxClass
       /* srule */ NO-LOCK NO-ERROR.  
       
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST VATCode
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST VATCode USE-INDEX VATPerc
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST VATCode USE-INDEX TaxZone
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST VATCode USE-INDEX TaxClass
       /* srule */ NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT VATCode
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT VATCode USE-INDEX VATPerc
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT VATCode USE-INDEX TaxZone
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT VATCode USE-INDEX TaxClass
       /* srule */ NO-LOCK NO-ERROR.   
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev VATCode
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev VATCode USE-INDEX VATPerc
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND prev VATCode USE-INDEX TaxZone
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND prev VATCode USE-INDEX TaxClass
       /* srule */ NO-LOCK NO-ERROR.  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       DISPLAY
       VATCode.VATCode
       VATCode.VCName
       VATCode.TaxZone
       VATCode.TaxClass
       VATCode.VATPerc
       VATCode.AccNum
       VATCode.ToDate
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND FIRST Account WHERE 
              Account.Brand = gcBrand AND 
              Account.AccNum = VATCode.AccNum 
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-update-record:
   RUN local-find-others.
   
   fZoneName(VATCode.TaxZone).
   fClassName(VATCode.TaxClass).
   
   DISP
   Account.AccName when  AVAIL Account
   "!! Unknown !!" when  NOT AVAIL Account @ Account.AccName
   VATCode.VCName
   VATCode.TaxZone lcZoneName
   VATCode.TaxClass lcClassName
   VATCode.VATPerc
   VATCode.AccNum
   VATCode.FromDate
   VATCode.ToDate
   WITH FRAME lis.

   IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      UPDATE
          VATCode.VCName
          VATCode.TaxZone
          VATCode.TaxClass
          VATCode.VATPerc
          VATCode.AccNum
          VATCode.FromDate
          VATCode.ToDate

      WITH FRAME lis EDITING  :
          READKEY.

          IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
             crundo = TRUE.
             leave.
          END.

          IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             PAUSE 0.
             IF FRAME-FIELD = "AccNum" THEN DO:
                FIND FIRST Account WHERE Account.AccNum =
                INPUT FRAME lis VATCode.AccNum NO-LOCK NO-ERROR.
                IF NOT AVAIL Account THEN DO:
                   BELL.
                   MESSAGE "Unknown AccNum !".
                   NEXT.
                END.
                DISP Account.AccName WITH FRAME lis.
             END.

             ELSE IF FRAME-FIELD = "TaxZone" THEN DO:
                fZoneName(INPUT INPUT FRAME lis VatCode.TaxZone).
                DISPLAY lcZoneName WITH FRAME lis.
             END.

             ELSE IF FRAME-FIELD = "TaxClass" THEN DO:
                fClassName(INPUT INPUT FRAME lis VatCode.TaxClass).
                DISPLAY lcClassName WITH FRAME lis.
             END.
           
          END.
          APPLY LASTKEY.
       END. /* EDITING */
       
       IF CAN-FIND(FIRST bVatCode WHERE
                         bVatCode.TaxZone  = VatCode.TaxZone  AND
                         bVatCode.TaxClass = VatCode.TaxClass AND
                         (
      /* 1. begin time cannot be inside the timeinterval of an
            existing VATCode */
                           (bVatCode.ToDate >= VatCode.FromDate AND
                            bVatCode.FromDate <= VatCode.FromDate)
                           OR 
      /* 2. end time cannot be inside the the timeinterval of an
            existing VATCode */
                           (bVatCode.ToDate >= VatCode.ToDate AND
                            bVatCode.FromDate <= VatCode.ToDate)
                           OR
      /* 3. timeinterval may not contain an existing VATCode 
            timeinterval */
                           (bVatCode.ToDate <= VatCode.ToDate AND
                            bVatCode.FromDate >= VatCode.FromDate)
                         ) AND
                         RECID(bVatCode) NE RECID(VatCode))
       THEN DO:
          MESSAGE "Active VAT code already exists with tax zone" VatCode.TaxZone
                  "and tax class" VatCode.TaxClass
          VIEW-AS ALERT-BOX ERROR.
          UNDO.
       END.
   
       IF VatCode.FromDate > VatCode.ToDate THEN DO:
          MESSAGE "Cannot set begin date after end date"
          VIEW-AS ALERT-BOX ERROR.
          UNDO.
       END.
       
       LEAVE.
   END.

   ELSE PAUSE MESSAGE "Press ENTER to continue".

END PROCEDURE.

