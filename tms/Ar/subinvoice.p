/* ----------------------------------------------------------------------
  MODULE .......: SubInvoice
  TASK .........: browse table SubInvoice
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 01.02.07
  CHANGED ......: 09.11.07/aam Sergel
  Version ......: TF
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/func.p}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SubInvoice'}
{Func/finvbal.i}
{Ar/subinvdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSubInvoice AS HANDLE NO-UNDO.
   lhSubInvoice = BUFFER SubInvoice:HANDLE.
   RUN StarEventInitialize(lhSubInvoice).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSubInvoice).
   END.

END.

DEF INPUT PARAMETER iiInvNum     AS INT  NO-UNDO.

DEF VAR liSubInvNum  AS INT                    NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 5.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR ldtDate      AS DATE                   NO-UNDO.
DEF VAR ldInterest   AS DEC                    NO-UNDO.
DEF VAR ldDebt       AS DEC                    NO-UNDO.
DEF VAR liIntMethod  AS INT                    NO-UNDO.
DEF VAR liDays       AS INT                    NO-UNDO.
DEF VAR ldIntPerc    AS DEC                    NO-UNDO. 
DEF VAR llHelp       AS LOG                    NO-UNDO.
DEF VAR lcDue        AS CHAR                   NO-UNDO.
DEF VAR ldtDueDate   AS DATE                   NO-UNDO.
DEF VAR lcExtInvID   AS CHAR                   NO-UNDO.

DEF BUFFER bSubInv FOR SubInvoice.

form
    SubInvoice.SubInvNum   
    SubInvoice.CLI       FORMAT "X(12)"
    SubInvoice.InvAmt 
    SubInvoice.PaidAmt
    ldDebt               FORMAT "->>>>>>9.99" COLUMN-LABEL "Debt"
    lcDue                FORMAT "X(3)"        COLUMN-LABEL "DUE"        
    SubInvoice.PaymState                      COLUMN-LABEL "PState"
    SubInvoice.ClaimState
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc)
       " SUBINVOICES OF INVOICE "  + STRING(lcExtInvID) + " "
    FRAME sel.

form /* seek  SubInvoice */
    "SubInvoice:" liSubInvNum FORMAT ">>>>9"
    HELP "Enter SubInvoice"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SubInvoice "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM
   "Invoice .........:" SubInvoice.InvNum    SKIP
   "Subinvoice ......:" SubInvoice.SubInvNum SKIP
   "Date ............:" Invoice.InvDate      SKIP
   "Dueday ..........:" Invoice.DueDate      SKIP
   "Amount ..........:" SubInvoice.InvAmt    
      FORMAT "->>>>>>9.99" SKIP
   "Paid ............:" SubInvoice.PaidAmt   
      FORMAT "->>>>>>9.99" SKIP
   "Date for Interest:" ldtDate 
      FORMAT "99-99-99"    SKIP
   "Interest ........:" ldInterest  
      FORMAT "->>>>>>9.99" SKIP
WITH CENTERED NO-LABELS OVERLAY ROW 10 TITLE " ESTIMATED INTEREST "
     FRAME fInterest.


FIND Invoice WHERE Invoice.InvNum = iiInvNum NO-LOCK.
ASSIGN
   lcExtInvID = Invoice.ExtInvID
   ldtDueDate = Invoice.DueDate.

                                          
/* used as help browser */
IF gcHelpParam > "" THEN DO:
   IF iiInvNum = 0 THEN iiInvNum = INTEGER(gcHelpParam) NO-ERROR.
   ASSIGN llHelp      = TRUE
          gcHelpParam = "".
END.
ELSE llHelp = FALSE.          

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE SubInvoice THEN ASSIGN
   Memory       = recid(SubInvoice)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

liIntMethod = fCParamI("IntCalcMet").


LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SubInvoice WHERE recid(SubInvoice) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SubInvoice THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SubInvoice).
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
        ufk    = 0
        ufk[1] = 36
        ufk[2] = 927
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        IF llHelp THEN ASSIGN 
           ufk[5] = 11
           ufk[7] = 790.
        ELSE ASSIGN 
           ufk[5] = 790 
           ufk[7] = 1152.
           
        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SubInvoice.SubInvNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SubInvoice.SubInvNum WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND SubInvoice WHERE recid(SubInvoice) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE SubInvoice THEN
              ASSIGN FIRSTrow = i Memory = recid(SubInvoice).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE SubInvoice THEN DO:
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
                rtab[1] = recid(SubInvoice)
                Memory  = rtab[1].
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
           IF NOT AVAILABLE SubInvoice THEN DO:
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
              rtab[FRAME-DOWN] = recid(SubInvoice).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SubInvoice WHERE recid(SubInvoice) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE SubInvoice THEN DO:
           Memory = recid(SubInvoice).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE SubInvoice THEN Memory = recid(SubInvoice).
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
           Memory = rtab[FRAME-DOWN].
           FIND SubInvoice WHERE recid(SubInvoice) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE liSubInvNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liSubInvNum > 0 THEN DO:
       
          FIND FIRST SubInvoice WHERE 
                     SubInvoice.InvNum     = iiInvNum AND
                     SubInvoice.SubInvNum >= liSubInvNum
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE SubInvoice THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* memo */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN DO:  
        RUN local-find-this(FALSE).
 
        RUN Mc/memo.p(INPUT SubInvoice.CustNum,
                 INPUT "SubInvoice",
                 INPUT STRING(SubInvoice.InvNum) + "/" + 
                       STRING(SubInvoice.SubInvNum),
                 INPUT "Invoice number").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* payments */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
        RUN local-find-this(FALSE).

        RUN Ar/payments.p(0,
                     SubInvoice.InvNum,
                     SubInvoice.SubInvNum).
        ufkey = TRUE.
        NEXT LOOP.
     END.

     /* choose from help */
     ELSE IF (llHelp AND LOOKUP(nap,"enter,return,5,f5") > 0 AND ufk[5] > 0) 
     THEN DO:  

        RUN local-find-this(FALSE).
        xrecid = RECID(SubInvoice).
        LEAVE LOOP.
     END.
      
     /* subinvoice rows */
     ELSE IF (NOT llHelp AND LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0) OR
             (llHelp AND LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0)
     THEN DO:  

        RUN local-find-this(FALSE).
       
        RUN Ar/nnlryp.p(SubInvoice.InvNum,
                     SubInvoice.SubInvNum).

        ufkey = TRUE.
        NEXT LOOP.
     END.
      
     /* other actions */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  

        RUN local-find-this(FALSE).
        
        COLOR DISPLAY MESSAGES
           SubInvoice.SubInvNum 
           SubInvoice.CLI
           SubInvoice.InvAmt WITH FRAME sel.

        otheractions:
        REPEAT ON ENDKEY UNDO, NEXT:

           assign ufk    = 0
                  ufk[8] = 8
                  ehto   = 0
                  ufkey  = true.
           RUN Syst/ufkey.p.
               
           /* credit invoice */
           IF toimi = 3 THEN DO:

              FIND Invoice OF SubInvoice NO-LOCK. 
              ASSIGN si-recid2 = RECID(Invoice)
                     si-recid  = RECID(SubInvoice)
                     memory    = RECID(SubInvoice).
              
              RUN Ar/nncimu.p.

              ASSIGN si-recid2  = ?   
                     si-recid   = ?
                     ufkey      = TRUE
                     must-print = TRUE.
              LEAVE otheractions.       
           END. 
    
           /* calculate estimated Interest for unpaid invoices */
           ELSE IF toimi = 5 THEN DO:       

              ASSIGN ldDebt  = SubInvoice.InvAmt - SubInvoice.PaidAmt
                     ldtDate = TODAY.

              FIND Invoice OF SubInvoice NO-LOCK.
                
              RUN Ar/calcint.p(Invoice.DueDate,
                          ldtDate,
                          liIntMethod,
                          ldDebt,
                          Invoice.CustNum,
                          OUTPUT liDays,
                          OUTPUT ldIntPerc,
                          OUTPUT ldInterest).

              IF ldDebt = 0 THEN DO:
                 MESSAGE "This invoice is totally paid"
                 VIEW-AS ALERT-BOX INFORMATION.
                 NEXT.
              end.   
              
              ehto = 9. 
              RUN Syst/ufkey.p.

              PAUSE 0.
              DISP 
              SubInvoice.InvNum 
              SubInvoice.SubInvNum
              Invoice.InvDate    
              Invoice.DueDate
              ldtDate
              SubInvoice.InvAmt  
              SubInvoice.PaidAmt 
              ldInterest
              WITH FRAME fInterest.
        
              REPEAT WITH FRAME fInterest ON ENDKEY UNDO, LEAVE:  
                 UPDATE ldtDate WITH FRAME fInterest.
              
                 IF ldtdate ENTERED THEN DO:
                    RUN Ar/calcint.p(Invoice.DueDate,
                                ldtDate,
                                liIntMethod,
                                ldDebt,
                                SubInvoice.CustNum,
                                OUTPUT liDays,
                                OUTPUT ldIntPerc,
                                OUTPUT ldInterest).
          
                    DISP ldInterest WITH FRAME fInterest.
                    MESSAGE "Press ENTER".
                    PAUSE NO-MESSAGE.
                 END.
                    
                 LEAVE.
              END.
   
              hide frame fInterest no-pause.
           END.
 
            
           ELSE IF toimi = 8 THEN LEAVE otheractions.
        
        end.
        
        COLOR DISPLAY NORMAL
           SubInvoice.SubInvNum 
           SubInvoice.CLI
           SubInvoice.InvAmt WITH FRAME sel.

        ufkey = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF NOT llHelp AND LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSubInvoice).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSubInvoice).

       RUN local-disp-row.
       xrecid = recid(SubInvoice).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SubInvoice) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SubInvoice) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND SubInvoice WHERE recid(SubInvoice) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND SubInvoice WHERE recid(SubInvoice) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
          FIND FIRST SubInvoice WHERE SubInvoice.InvNum = iiInvNum
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
          FIND LAST SubInvoice WHERE SubInvoice.InvNum = iiInvNum
          NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN 
          FIND NEXT SubInvoice WHERE SubInvoice.InvNum = iiInvNum 
          NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN 
          FIND PREV SubInvoice WHERE SubInvoice.InvNum = iiInvNum 
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       SubInvoice.SubInvNum
       SubInvoice.CLI
       SubInvoice.InvAmt
       SubInvoice.PaidAmt
       ldDebt 
       lcDue
       SubInvoice.PaymState
       SubInvoice.ClaimState
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   ldDebt = SubInvoice.InvAmt - SubInvoice.PaidAmt.
 
   IF SubInvoice.PaymState = 3 THEN lcDue = "CRL".
   ELSE IF ldDebt NE 0 AND ldtDueDate < pvm THEN lcDue = "!!!".
    
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME fInvDet ON ENDKEY UNDO, LEAVE:
   
      /* show details */
      RUN pSubInvoiceDetails(SubInvoice.InvNum,
                             SubInvoice.SubInvNum,
                             FALSE).

      ASSIGN
         ufkey = true
         ufk = 0
         ufk[8] = 8
         ehto = 0.
      RUN Syst/ufkey.p.

      IF toimi = 8 THEN DO:
         HIDE FRAME fInvDet NO-PAUSE.
         LEAVE.   
      END.
   END.
   
END PROCEDURE.

 
