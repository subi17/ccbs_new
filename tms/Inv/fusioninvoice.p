/* ----------------------------------------------------------------------
  MODULE .......: fusioninvoice.p
  TASK .........: FusionInvoice browser
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 12.12.13
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FusionInvoice'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR liCustnum    LIKE FusionInvoice.Custnum  NO-UNDO.
DEF VAR liFuInvNum   LIKE FusionInvoice.FuInvNum  NO-UNDO.
DEF VAR ldaInvdate   LIKE FusionInvoice.InvDate  NO-UNDO.
DEF VAR lcCustomerID LIKE FusionInvoice.CustomerID  NO-UNDO.
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
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcExtInvId   LIKE Invoice.ExtInvId NO-UNDO.

form
    FusionInvoice.FuInvNum      /* COLUMN-LABEL FORMAT */
    FusionInvoice.InvDate
    FusionInvoice.CustomerID
    FusionInvoice.Custnum
    FusionInvoice.InvoiceNum
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Fusion Invoices "
    + string(pvm,"99-99-9999") + " "
    FRAME sel.

form
    FusionInvoice.FuInvNum        
    FusionInvoice.InvDate            
    FusionInvoice.InvoiceNum       
    lcExtInvId LABEL "Ext.InvID"
    FusionInvoice.CustomerId      
    FusionInvoice.Custnum
    FusionInvoice.Language        
    FusionInvoice.Email           
    SKIP(1)

    FusionInvoice.ProductCode     
    FusionInvoice.ProductText   
    FusionInvoice.FixedNumber
    SKIP(1)

    FusionInvoice.AddItems       FORMAT "-z,zz9.99"
    FusionInvoice.BaseServiceMF  FORMAT "-z,zz9.99" LABEL "BaseServiceMF"
    FusionInvoice.OtherItems     FORMAT "-z,zz9.99"    
    FusionInvoice.OtherMF        FORMAT "-z,zz9.99"
    FusionInvoice.TrafficAmt     FORMAT "-z,zz9.99"
    FusionInvoice.TaxIncome      FORMAT "-z,zz9.99"
    FusionInvoice.TaxAmt         FORMAT "-z,zz9.99"  
    FusionInvoice.TotalAmt       FORMAT "-z,zz9.99"   
    FusionInvoice.TotalToPay     FORMAT "-z,zz9.99"
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    2 columns
    FRAME lis.

form
    lcExtInvId LABEL "Ext.InvID"
    FusionInvoice.CustNum            
    FusionInvoice.MsSeq           
    FusionInvoice.MSubsType     
    FusionInvoice.MOtherMF         
    FusionInvoice.MTariffMF       
    FusionInvoice.MTermFinancing  
    FusionInvoice.MTraffic        
    FusionInvoice.MTaxableIncome     
    FusionInvoice.MVatAmt             
    FusionInvoice.MTotalInvoice  
    FusionInvoice.MInvAmt         
    
WITH  OVERLAY ROW 8 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) "MOBILE INVOICE"
    SIDE-LABELS 
    2 columns
    FRAME lis2.

form /* seek FusionInvoice  BY  FusionInvoice */
    liCustnum
    HELP "Enter Fusion InvNum"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND FuInvNo "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek FusionInvoice  BY CoName */
    liCustnum
    HELP "Enter InvDate"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND InvDate "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek FusionInvoice  BY CoName */
    liCustnum
    HELP "Enter CustomerId"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CustomerID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form /* seek FusionInvoice  BY CoName */
    liCustnum
    HELP "Enter Custnum"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Custnum "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f4.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By FuInvNum  ,By InvDate   ,By CustomerID,By CustNum   ".

FIND FIRST FusionInvoice
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE FusionInvoice THEN ASSIGN
   Memory       = recid(FusionInvoice)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No fusion invoices available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FusionInvoice WHERE recid(FusionInvoice) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FusionInvoice THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FusionInvoice).
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
        ufk[1]= 92  ufk[2]= 28 ufk[3]= 812 ufk[4]= 707
        ufk[5]= 0 /* (IF lcRight = "RW" THEN 5 ELSE 0) */
        ufk[6]= 0 /* (IF lcRight = "RW" THEN 4 ELSE 0) */
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FusionInvoice.FuInvNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FusionInvoice.FuInvNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW FusionInvoice.Invdate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FusionInvoice.Invdate WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW FusionInvoice.CustomerID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FusionInvoice.CustomerID WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
        CHOOSE ROW FusionInvoice.Custnum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FusionInvoice.Custnum WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND FusionInvoice WHERE recid(FusionInvoice) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FusionInvoice THEN
              ASSIGN FIRSTrow = i Memory = recid(FusionInvoice).
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
           IF NOT AVAILABLE FusionInvoice THEN DO:
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
                rtab[1] = recid(FusionInvoice)
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
           IF NOT AVAILABLE FusionInvoice THEN DO:
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
              rtab[FRAME-DOWN] = recid(FusionInvoice).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FusionInvoice WHERE recid(FusionInvoice) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FusionInvoice THEN DO:
           Memory = recid(FusionInvoice).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FusionInvoice THEN Memory = recid(FusionInvoice).
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
           FIND FusionInvoice WHERE recid(FusionInvoice) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET liFuInvNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF liFuInvNum ENTERED THEN DO:
          FIND FIRST FusionInvoice WHERE FusionInvoice.FuInvnum >= liFuInvNum
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FusionInvoice THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FusionInvoice/FusionInvoice was found */
          ASSIGN order = 1 Memory = recid(FusionInvoice) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       ldaInvdate = DATE(MONTH(TODAY),1,YEAR(TODAY)).
       DISP ldaInvdate WITH FRAME f2.
       SET ldaInvDate WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ldaInvDate ENTERED THEN DO:
          FIND FIRST FusionInvoice WHERE FusionInvoice.InvDate >= ldaInvDate
          USE-INDEX InvDate /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FusionInvoice THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FusionInvoice/CoName was found */
          ASSIGN order = 2 Memory = recid(FusionInvoice) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */
     
     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f3.
       SET lcCustomerID WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF lcCustomerID ENTERED THEN DO:
          FIND FIRST FusionInvoice WHERE FusionInvoice.CustomerID EQ lcCustomerID
          USE-INDEX CustomerID /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FusionInvoice THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FusionInvoice/CoName was found */
          ASSIGN order = 3 Memory = recid(FusionInvoice) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */
     
     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f4.
       SET liCustnum WITH FRAME f4.
       HIDE FRAME f4 NO-PAUSE.
       IF liCustnum ENTERED THEN DO:
          FIND FIRST FusionInvoice WHERE FusionInvoice.Custnum EQ liCustnum
          USE-INDEX Custnum /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE FusionInvoice THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some FusionInvoice/CoName was found */
          ASSIGN order = 4 Memory = recid(FusionInvoice) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-4 */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
/*       {Syst/uright2.i} */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY FusionInvoice.FuInvNum.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       ufkey = true.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(FusionInvoice).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FusionInvoice) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FusionInvoice) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
HIDE FRAME f1 NO-PAUSE.
HIDE FRAME f2 NO-PAUSE.
HIDE FRAME f3 NO-PAUSE.
HIDE FRAME f4 NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FusionInvoice WHERE recid(FusionInvoice) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FusionInvoice WHERE recid(FusionInvoice) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST FusionInvoice
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FusionInvoice USE-INDEX InvDate
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST FusionInvoice USE-INDEX CustomerId
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND FIRST FusionInvoice USE-INDEX Custnum
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST FusionInvoice
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FusionInvoice USE-INDEX InvDate
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST FusionInvoice USE-INDEX CustomerId
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND LAST FusionInvoice USE-INDEX Custnum
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT FusionInvoice
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FusionInvoice USE-INDEX InvDate
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT FusionInvoice USE-INDEX CustomerId
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND NEXT FusionInvoice USE-INDEX Custnum
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV FusionInvoice
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FusionInvoice USE-INDEX InvDate
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV FusionInvoice USE-INDEX CustomerId
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 4 THEN FIND PREV FusionInvoice USE-INDEX Custnum
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       FusionInvoice.FuInvNum
       FusionInvoice.InvDate
       FusionInvoice.Custnum
       FusionInvoice.CustomerId
       FusionInvoice.InvoiceNum

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   IF FusionInvoice.InvNum > 0 THEN DO:
      FIND FIRST Invoice NO-LOCK WHERE
                 Invoice.Invnum = FusionInvoice.InvNum NO-ERROR.
      IF AVAIL Invoice THEN lcExtInvId = Invoice.ExtInvID.
      ELSE lcExtInvId = "".
   END.
   ELSE lcExtInvId = "".
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   RUN local-find-others.
      
   ufkey = true.
   LOOP:
   REPEAT ON ENDKEY UNDO, LEAVE:
   DISP 
    FusionInvoice.AddItems            
    FusionInvoice.BaseServiceMF   
    FusionInvoice.CustomerId      
    FusionInvoice.Custnum      
    FusionInvoice.Email           
    lcExtInvId
    FusionInvoice.FixedNumber
    FusionInvoice.FuInvNum        
    FusionInvoice.InvDate            
    FusionInvoice.InvoiceNum       
    FusionInvoice.Language        
    FusionInvoice.OtherItems      
    FusionInvoice.OtherMF      
    FusionInvoice.ProductCode     
    FusionInvoice.ProductText   
    FusionInvoice.TaxAmt          
    FusionInvoice.TaxIncome   
    FusionInvoice.TotalAmt        
    FusionInvoice.TotalToPay    
    FusionInvoice.TrafficAmt     
      WITH FRAME lis.
   
   IF ufkey THEN DO:
     ASSIGN
     ufk = 0
     ufk[1]= 216 WHEN fusioninvoice.MsSeq > 0 
     ufk[8]= 8 ufk[9]= 1
     ehto = 3 ufkey = FALSE.
     RUN Syst/ufkey.p.
   END.

   nap = keylabel(LASTKEY).
  
  IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN DO:
     PAUSE 0.
     DISP
     FusionInvoice.CustNum            
     lcExtInvId
     FusionInvoice.MInvAmt         
     FusionInvoice.MOtherMF         
     FusionInvoice.MsSeq           
     FusionInvoice.MSubsType     
     FusionInvoice.MTariffMF       
     FusionInvoice.MTaxableIncome     
     FusionInvoice.MTermFinancing  
     FusionInvoice.MTotalInvoice  
     FusionInvoice.MTraffic        
     FusionInvoice.MVatAmt WITH FRAME lis2.
     MESSAGE "Press ENTER !".
     PAUSE No-message.

     HIDE FRAME lis2 NO-PAUSE.
  END.
  ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.
  
  PAUSE.

  END.
END PROCEDURE.
 
