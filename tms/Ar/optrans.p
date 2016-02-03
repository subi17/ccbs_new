/* ----------------------------------------------------------------------
  MODULE .......: OPLog.p
  TASK .........: Browser of Overpayment transactions
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 25-01-00
  CHANGED ......: 27.03.02/aam longer FORMAT FOR EventType
                  03.03.03 tk  tokens
                  06.08.04/aam use new indexes
                  02.01.06/aam values from TMSCodes
                  24.01.06/jt DYNAMIC-FUNCTION("fDispCustName"
                  27.08.07/aam Info
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}  /*qupd = TRUE.*/
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'oplog'}

DEF INPUT PARAMETER iiCustNum AS I NO-UNDO.
DEF /* NEW */ shared VAR siirto AS CHAR.


DEF VAR InvNum  LIKE OPLog.InvNum  NO-UNDO.
DEF VAR EventDate  LIKE OPLog.EventDate  NO-UNDO.
DEF VAR Ttype-name   AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 2.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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
DEF VAR lcTypes      AS CHAR                   NO-UNDO.
DEF VAR lcValue      AS CHAR                   NO-UNDO. 
DEF VAR lcCustName   AS CHAR                   NO-UNDO.

DEF TEMP-TABLE ttOpType NO-UNDO
   FIELD OpType AS CHAR
   FIELD OpName AS CHAR
   INDEX OpType OpType.
   
form
    OPLog.InvNum      /* COLUMN-LABEL FORMAT */
    OPLog.EventDate      /* COLUMN-LABEL FORMAT */
    OPLog.Amt              /* COLUMN-LABEL */ FORMAT "->,>>>,>>9.99"
    OPLog.EventType    column-label "T"        format ">9"
    Ttype-name         column-label "TypeName" format "x(42)"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " OVERPAYMENT TRANSACTIONS OF "
    + STRING(iiCustNum) + " " 
    + lcCustName
    FRAME sel.

form
" Customer's name .......: " lcCustName                            skip(1)
" number of Invoice .....: " OPLog.InvNum                          skip
" Voucher of Inv. .......: " OPLog.Voucher                         skip
" Date of Transaction ...: " OPLog.EventDate    /* label format */ skip(1)
" Transactions TypeCode .: " OPLog.EventType format ">9" 
  Ttype-name format "x(42)"                                        skip        
" Sum of Transaction ....: " OPLog.Amt    
                             format "->,>>>,>>9.99" skip(1)
" TimeStamp of creation .: " OPLog.CreStamp                        skip 
" ID of the TMS user ....: " OPLog.UserCode                        skip
" Info ..................: " OPLog.Info FORMAT "X(40)"             SKIP

WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    /*1 columns*/
    FRAME lis.

form /* seek BROWSE  BY  InvNum */
    InvNum
    HELP "Enter number of Invoice"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND number "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek BROWSE  BY EventDate */
    EventDate
    HELP "Enter Date of transaction"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.

lcCustName =  DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                BUFFER Customer).
cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Invoice  ,   By Date    ,   By 3    , By 4".


RUN local-find-first.
IF AVAILABLE OPLog THEN ASSIGN
   memory       = recid(OPLog)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE 
   "   There are no Overpayment Transactions "    SKIP
   "   for customer '" iiCustNum "' !"
   VIEW-AS ALERT-BOX TITLE "NO TRANSACTIONS".
   RETURN.
END.

lcTypes = DYNAMIC-FUNCTION("fTMSCodeList" IN ghFunc1,
                           "OpLog",
                           "EventType").
                              
DO i = 1 TO NUM-ENTRIES(lcTypes,CHR(1)):

    lcValue = ENTRY(i,lcTypes,CHR(1)).
    
    CREATE ttOpType.
    ASSIGN ttOpType.OpType = ENTRY(1,lcValue,CHR(9))
           ttOpType.OpName = ENTRY(2,lcValue,CHR(9)).
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 32
       " " + ENTRY(order,orders) + " ".
    END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND OPLog WHERE recid(OPLog) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OPLog THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OPLog).
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
        PAUSE 0 NO-MESSAGE.

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
        ufk[1]= 36  ufk[2]= 28 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OPLog.InvNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OPLog.InvNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW OPLog.EventDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OPLog.EventDate WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND OPLog WHERE recid(OPLog) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OPLog THEN
              ASSIGN FIRSTrow = i memory = recid(OPLog).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE OPLog THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(OPLog)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE OPLog THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(OPLog).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND OPLog WHERE recid(OPLog) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OPLog THEN DO:
           memory = recid(OPLog).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OPLog THEN memory = recid(OPLog).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND OPLog WHERE recid(OPLog) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */                          
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET InvNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF InvNum ENTERED THEN DO:
          FIND FIRST OPLog WHERE 
                    OPLog.CustNum = iiCustNum AND
                    OPLog.InvNum >= InvNum
              USE-INDEX CustInv NO-LOCK NO-ERROR.
          IF NOT AVAILABLE OPLog THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some optrans/InvNo was found */
          ASSIGN order = 1 memory = recid(OPLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET EventDate WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF EventDate ENTERED THEN DO:
          FIND FIRST OPLog WHERE 
                     OPLog.CustNum   = iiCustNum AND
                     OPLog.EventDate = EventDate
          USE-INDEX CustDate NO-LOCK NO-ERROR.
          IF NOT AVAILABLE OPLog THEN 
          FIND LAST OPLog WHERE 
                    OPLog.CustNum   = iiCustNum AND
                    OPLog.EventDate > EventDate
          USE-INDEX CustDate NO-LOCK NO-ERROR.

          IF NOT AVAILABLE OPLog THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some optrans/TDate was found */
          ASSIGN order = 2 memory = recid(OPLog) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY OPLog.InvNum.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(OPLog).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(OPLog) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(OPLog) must-print = TRUE.
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
      FIND OPLog WHERE recid(OPLog) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OPLog WHERE recid(OPLog) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST OPLog
       where OPLog.CustNum = iiCustNum USE-INDEX CustInv NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST OPLog USE-INDEX CustDate
       where OPLog.CustNum = iiCustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST OPLog
       where OPLog.CustNum = iiCustNum USE-INDEX CustInv NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST OPLog USE-INDEX CustDate
       where OPLog.CustNum = iiCustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT OPLog
       where OPLog.CustNum = iiCustNum USE-INDEX CustInv NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT OPLog USE-INDEX CustDate
       where OPLog.CustNum = iiCustNum NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND prev OPLog
       where OPLog.CustNum = iiCustNum USE-INDEX CustInv NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev OPLog USE-INDEX CustDate
       where OPLog.CustNum = iiCustNum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       /* FIND additional information from other tables FOR DISPLAY */
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       OPLog.InvNum
       OPLog.EventDate
       Amt
       OPLog.EventType
       Ttype-name
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Customer WHERE Customer.CustNum = OPLog.CustNum NO-LOCK NO-ERROR.

   Ttype-name = "".
   IF OPLog.EventType > 0 THEN DO:
      FIND FIRST ttOpType WHERE ttOpType.OpType = STRING(OPLog.EventType)
         NO-ERROR.
      IF AVAILABLE ttOpType THEN  
         Ttype-name = ttOpType.OpName.
   END.  


END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      
      lcCustName =  DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                      BUFFER Customer).
      DISPLAY
        lcCustName
        OPLog.EventDate
        OPLog.CreStamp
        OPLog.UserCode
        OPLog.EventType
        Ttype-name
        OPLog.InvNum
        OPLog.Voucher
        OPLog.Amt
        OPLog.Info

      WITH FRAME lis.

      ufk = 0. ehto = 3. RUN ufkey.
      message "Press ENTER !".
      PAUSE no-message.



      UPDATE
      WITH FRAME lis.
      LEAVE.
   END.
END PROCEDURE.

