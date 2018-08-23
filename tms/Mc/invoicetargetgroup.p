/* ----------------------------------------------------------------------
  MODULE .......: invoicetargetgroup
  TASK .........: UPDATEs table invoicetargetgroup
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 05/2010 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable invoicetargetgroup

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoicetargetgroup'}
{Mc/invoicetarget.i}

DEF INPUT PARAMETER iiCustnum  AS INT NO-UNDO.
DEF INPUT PARAMETER iiITGroupID AS INT NO-UNDO.

DEF BUFFER bInvoiceTargetGroup FOR InvoiceTargetGroup.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR liinvoicetargetgroup  LIKE invoicetargetgroup.itgroupid NO-UNDO.
DEF VAR liCustnum    LIKE InvoiceTargetGroup.AgrCust NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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
DEF VAR lcError      AS CHARACTER NO-UNDO. 
DEF VAR liInvoiceTargetGroupID AS INTEGER NO-UNDO. 

DEF VAR lcDelType    AS CHAR                   NO-UNDO FORMAT "X(22)".


FORM
    invoicetargetgroup.itgroupid
    invoicetargetgroup.Custnum 
    invoicetargetgroup.FromDate 
    invoicetargetgroup.ToDate 
    invoicetargetgroup.DefaultGroup
    lcDelType COLUMN-LABEL "Del.Type"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
       "  InvoiceTargetGroups  " + "  " +
       string(TODAY,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
  InvoiceTargetGroup.ITGroupID
     LABEL "IT GroupID ...."
     help "Invoice Target Group ID"
  InvoiceTargetGroup.AccountID AT 38
     LABEL "Account ID ...."
     SKIP

  InvoiceTargetGroup.CustAccName
     FORMAT "X(12)"
     LABEL "Account Name .."
  InvoiceTargetGroup.BankAccount AT 38
     FORMAT "X(12)"
     LABEL "BankAccount ..."
     SKIP

  InvoiceTargetGroup.Currency
     LABEL "Currency ......"
  InvoiceTargetGroup.PaymentMethod AT 38
     LABEL "Pay Mode ......"
     SKIP

  InvoiceTargetGroup.BankName
     LABEL "BankName ......"
     FORMAT "X(12)"
     HELP  "Bank Name"
  InvoiceTargetGroup.BillCycle AT 38
     LABEL "BillCycle ....."
     SKIP

  InvoiceTargetGroup.MandateID
     LABEL "MandateID ....."
     FORMAT "X(12)"
  InvoiceTargetGroup.MandateDate AT 38
     LABEL "Mandate Date .."
     SKIP

  InvoiceTargetGroup.CustNum
     LABEL "CustNum ......."
  InvoiceTargetGroup.AgrCust AT 38
     LABEL "Agr. Customer ."
     SKIP

  InvoiceTargetGroup.DefaultGroup
     LABEL "DefaultGroup .."
  InvoiceTargetGroup.StatusCode AT 38
     LABEL "Status Code ..."
     SKIP

  InvoiceTargetGroup.InvInterval
     LABEL "InvInterval ..."
  lcDelType AT 38
     FORMAT "X(10)"
     LABEL "DelType ......."
     SKIP

  InvoiceTargetGroup.InvGroup
     FORMAT "X(8)"
     LABEL "PayType ......."
  InvoiceTargetGroup.DueDateOffSet AT 38
     LABEL "Due Date ......"
     SKIP

  InvoiceTargetGroup.FromDate
     LABEL "Valid From ...."
  InvoiceTargetGroup.ToDate AT 38
     LABEL "Valid To ......"
     SKIP

  WITH  CENTERED OVERLAY ROW 3 WIDTH 80
  SIDE-LABELS TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr
  FRAME lis.

FORM 
    "ITGroupID:" liinvoicetargetgroup
    HELP "Enter invoicetargetgroup ID"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND InvoiceTargetGroup "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "Custnum:" liCustnum FORMAT ">>>>>>>>9" 
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND InvoiceTargetGroup "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f2.

IF iiCustnum > 0 THEN ASSIGN
   FrmRow = 3
   FrmDown = 8.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE invoicetargetgroup THEN ASSIGN
   Memory       = recid(invoicetargetgroup)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No invoicetargetgroups available" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder Ne 1 THEN Do:
       pr-order = order.
    END.
   
   IF must-add THEN DO:  /* Add a InvoiceTarget  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        UpdateField:
        REPEAT TRANS WITH FRAME lis:

           IF iiCustnum > 0 THEN liCustnum = iiCustnum.
           
           DISPLAY FALSE @ InvoiceTargetGroup.DefaultGroup.
           DISPLAY TODAY @ InvoiceTargetGroup.FromDate.
           DISPLAY 12/31/2049 @ InvoiceTargetGroup.ToDate.

           Syst.Var:ehto = 9.
           RUN Syst/ufkey.p.
           
           UPDATE
              liCustnum
           WITH FRAME lis EDITING:
    
              READKEY.
        
              IF LOOKUP(KEYLABEL(LASTKEY),"f2") > 0 THEN NEXT.
/*
              IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 
                 IF FRAME-FIELD = "liCustnum" THEN DO:

                    MESSAGE (input liCustnum) licustnum VIEW-AS ALERT-BOX.
                    IF NOT CAN-FIND(
                       FIRST Customer WHERE
                             Customer.Custnum = liCustnum)
                          THEN DO:
                       MESSAGE "Customer not found" VIEW-AS ALERT-BOX.
                       NEXT.
                    END.
                 END.
              
              END.
*/
              APPLY LASTKEY.
           END.

           IF iiCustnum > 0 AND liCustnum NE iiCustnum THEN DO:
              MESSAGE "Customer must be" iiCustnum VIEW-AS ALERT-BOX ERROR.
              NEXT UpdateField.
           END.
 
           liInvoiceTargetGroupID = fAddInvoiceTargetGroup( 
                              liCustnum,
                              ?,
                              OUTPUT lcError).

           IF lcError NE "" THEN DO:
              MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
              NEXT UpdateField.
           END.
           ELSE DO:
              MESSAGE "Invoice target group" liInvoiceTargetGroupID "created"
              VIEW-AS ALERT-BOX.
           END.

           FIND InvoiceTargetGroup WHERE
                InvoiceTargetGroup.ITGroupID = liInvoiceTargetGroupID NO-LOCK.

           LEAVE.

        END.

        IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN 
        UNDO add-row, LEAVE add-row.

        ASSIGN
        Memory = recid(InvoiceTargetGroup)
        xrecid = Memory.  
        LEAVE.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE InvoiceTargetGroup THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.
   
   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE invoicetargetgroup THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(invoicetargetgroup).
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
        Syst.Var:ufk   = 0
        Syst.Var:ufk[1]= 135 WHEN iiCustnum = 0
        Syst.Var:ufk[2]= 702 WHEN iiCustnum = 0
        Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.Var:ufk[7]= 0  
        Syst.Var:ufk[8]= 8 
        Syst.Var:ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF Syst.Var:gcHelpParam > "" THEN ASSIGN
           Syst.Var:ufk[5] = 11
           Syst.Var:ufk[6] = 0.

        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW invoicetargetgroup.itgroupid {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) invoicetargetgroup.itgroupid WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW invoicetargetgroup.custnum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) invoicetargetgroup.custnum WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE invoicetargetgroup THEN
              ASSIGN FIRSTrow = i Memory = recid(invoicetargetgroup).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE invoicetargetgroup THEN DO:
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
                rtab[1] = recid(invoicetargetgroup)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE invoicetargetgroup THEN DO:
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
              rtab[FRAME-DOWN] = recid(invoicetargetgroup).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE invoicetargetgroup THEN DO:
           Memory = recid(invoicetargetgroup).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE invoicetargetgroup THEN Memory = recid(invoicetargetgroup).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET liinvoicetargetgroup WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF liinvoicetargetgroup > 0 THEN DO:
          FIND FIRST invoicetargetgroup WHERE 
                     invoicetargetgroup.itgroupid >= liinvoicetargetgroup
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */
     
     /* Search BY column 2 */
     ELSE IF LOOKUP(Syst.Var:nap,"2,f2") > 0 AND Syst.Var:ufk[2] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f2.
       SET liCustnum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       
       IF liCustnum > 0 THEN DO:
          FIND FIRST invoicetargetgroup WHERE 
                     invoicetargetgroup.Custnum >= liCustnum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY invoicetargetgroup.itgroupid.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(invoicetargetgroup).
       IF must-print EQ TRUE THEN NEXT LOOP.
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(invoicetargetgroup) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(invoicetargetgroup) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

Syst.Var:ehto = 4.
RUN Syst/ufkey.p.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND invoicetargetgroup WHERE recid(invoicetargetgroup) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiCustnum > 0 THEN 
      FIND FIRST invoicetargetgroup USE-INDEX Custnum WHERE 
                 invoicetargetgroup.Custnum = iiCustnum NO-LOCK NO-ERROR.
   
   ELSE IF iiITGroupID > 0 THEN
      FIND FIRST invoicetargetgroup USE-INDEX ITGroupID WHERE 
                 invoicetargetgroup.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND FIRST invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX itgroupid NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX Custnum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiCustnum > 0 THEN 
      FIND LAST invoicetargetgroup USE-INDEX Custnum WHERE 
                 invoicetargetgroup.Custnum = iiCustnum NO-LOCK NO-ERROR.
   
   ELSE IF iiITGroupID > 0 THEN
      FIND LAST invoicetargetgroup USE-INDEX ITGroupID WHERE 
                 invoicetargetgroup.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND LAST invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX itgroupid NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX Custnum NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiCustnum > 0 THEN 
      FIND NEXT invoicetargetgroup USE-INDEX Custnum WHERE 
                 invoicetargetgroup.Custnum = iiCustnum NO-LOCK NO-ERROR.
   
   ELSE IF iiITGroupID > 0 THEN
      FIND NEXT invoicetargetgroup USE-INDEX ITGroupID WHERE 
                 invoicetargetgroup.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND NEXT invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX itgroupid NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX Custnum NO-LOCK NO-ERROR.

      
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF iiCustnum > 0 THEN 
      FIND PREV invoicetargetgroup USE-INDEX Custnum WHERE 
                 invoicetargetgroup.Custnum = iiCustnum NO-LOCK NO-ERROR.
   
   ELSE IF iiITGroupID > 0 THEN
      FIND PREV invoicetargetgroup USE-INDEX ITGroupID WHERE 
                 invoicetargetgroup.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND PREV invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX itgroupid NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV invoicetargetgroup WHERE (IF iiCustnum   > 0 THEN invoicetargetgroup.Custnum = iiCustnum   ELSE TRUE) 
                                         AND (IF iiITGroupID > 0 THEN invoicetargetgroup.ITGroupID = iiITGroupID ELSE TRUE) USE-INDEX Custnum NO-LOCK NO-ERROR.

      
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
          invoicetargetgroup.itgroupid
          invoicetargetgroup.Custnum 
          invoicetargetgroup.FromDate 
          invoicetargetgroup.ToDate 
          invoicetargetgroup.DefaultGroup
          lcDelType
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   IF invoicetargetgroup.DelType <> ? THEN
      lcDelType = Func.Common:mTMSCodeName("Invoice",
                                   "DelType",
                                   STRING(invoicetargetgroup.DelType)).
   ELSE lcDelType = "".
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llDispMenu   AS LOG  NO-UNDO.
   DEF VAR lcNewValue   AS CHAR NO-UNDO.
   DEF VAR ldtNewDate   AS DATE NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

   RUN local-find-others.
   
   DISP 
      InvoiceTargetGroup.ITGroupID     
      InvoiceTargetGroup.AccountID     
      InvoiceTargetGroup.CustAccName   
      InvoiceTargetGroup.Currency      
      InvoiceTargetGroup.BankAccount   
      InvoiceTargetGroup.BankName      
      InvoiceTargetGroup.PaymentMethod 
      InvoiceTargetGroup.MandateID     
      InvoiceTargetGroup.MandateDate   
      InvoiceTargetGroup.CustNum       
      InvoiceTargetGroup.AgrCust       
      InvoiceTargetGroup.DefaultGroup  
      InvoiceTargetGroup.BillCycle     
      InvoiceTargetGroup.InvInterval   
      lcDelType
      InvoiceTargetGroup.InvGroup      
      InvoiceTargetGroup.DueDateOffSet 
      InvoiceTargetGroup.StatusCode    
      InvoiceTargetGroup.FromDate      
      InvoiceTargetGroup.ToDate 
   WITH FRAME lis.
   
      ASSIGN 
         Syst.Var:ufk    = 0
         Syst.Var:ufk[3] = 9023
         Syst.Var:ufk[4] = 0
         Syst.Var:ufk[6] = 1752 
         Syst.Var:ufk[7] = 1522 WHEN lcRight EQ "RW"
         Syst.Var:ufk[8] = 8
         Syst.Var:ehto   = 0.
      
      RUN Syst/ufkey.p.
            
      IF Syst.Var:toimi = 3 THEN RUN Mc/invoicetarget.p (
         invoicetargetgroup.itgroupid,
         0).
      
      IF Syst.Var:toimi = 6 THEN DO: 
         RUN Mc/eventsel.p("invoicetargetgroup",
                        STRING(InvoiceTargetGroup.ITGroupID)).
      END.   
      
      /* functions */
      ELSE IF Syst.Var:toimi = 7 THEN do:

         DEFINE VARIABLE lcMenuOptions AS CHARACTER NO-UNDO. 
         DEFINE VARIABLE lcSelected AS CHARACTER NO-UNDO. 

         lcMenuOptions = "SET AS DEFAULT GROUP|DEACTIVATE GROUP".
         
         RUN Syst/selectbox.p(
            "INVOICE TARGET GROUP",
            lcMenuOptions,
            OUTPUT lcSelected).
               
         CASE lcSelected:
                              
            WHEN "SET AS DEFAULT GROUP" THEN DO:
               fSetDefaultInvoiceTargetGroup(
                  InvoiceTargetGroup.ITGroupID,
                  OUTPUT lcError).
               IF lcError NE "" THEN MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
               ELSE DO:
                  MESSAGE "Group is set as default group" VIEW-AS ALERT-BOX.
                  ASSIGN must-print = TRUE.
               END.
            END.

            WHEN "DEACTIVATE GROUP" THEN DO:
               fDeactivateInvoiceTargetGroup(
                  InvoiceTargetGroup.ITGroupID,
                  OUTPUT lcError).
               IF lcError NE "" THEN MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
               ELSE DO:
                  MESSAGE "Group is inactivated" VIEW-AS ALERT-BOX.
               END.
            END.  

         END.      

      END.   
      
      ELSE IF Syst.Var:toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.
