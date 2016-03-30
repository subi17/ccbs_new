/* ----------------------------------------------------------------------
  MODULE .......: InvoiceTarget.p
  TASK .........: UPDATEs table InvoiceTarget
  APPLICATION ..: tms
  AUTHOR .......: anttis
  CREATED ......: 05/2010
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'InvoiceTarget'}
{Func/timestamp.i}
{Mc/invoicetarget.i}

DEF INPUT PARAM iiITGroupID AS INT NO-UNDO.
DEF INPUT PARAM iiMsSeq AS INTEGER NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEF BUFFER bInvoiceTarget FOR InvoiceTarget.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcType       AS CHAR NO-UNDO.
DEF VAR lcBegin      AS CHAR NO-UNDO.
DEF VAR lcEnd        AS CHAR NO-UNDO.
DEF VAR ldtFromDate  AS DATE NO-UNDO.
DEF VAR ldtToDate    AS DATE NO-UNDO. 
DEF VAR lcFrameField AS CHAR NO-UNDO. 
DEF VAR lcCodeTable  AS CHAR NO-UNDO.
DEF VAR ldaDefFrom    AS DATE  NO-UNDO.
DEF VAR liNewITGroup AS INTEGER NO-UNDO. 
DEF VAR lcError AS CHARACTER NO-UNDO. 
DEF VAR liMsSeq LIKE MobSub.Msseq NO-UNDO.
DEF VAR liInvoiceTargetID AS INTEGER NO-UNDO. 
DEF VAR lcMSISDN LIKE MobSub.CLI NO-UNDO.

FORM
   InvoiceTarget.ITGroupID
   InvoiceTarget.InvoiceTargetId
   InvoiceTarget.MsSeq
   lcMSISDN
   InvoiceTarget.FromDate
   InvoiceTarget.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
   COLOR VALUE(cfc)   
   TITLE COLOR VALUE(ctc) 
      " InvoiceTargets " + " "
   FRAME sel.

FORM
   InvoiceTarget.ITGroupId COLON 20 SKIP
   InvoiceTarget.InvoiceTargetID COLON 20 SKIP(1)
   liMsSeq COLON 20 LABEL "Subscr.ID" HELP "Help F9"
   lcMSISDN COLON 20
   InvoiceTarget.FromDate COLON 20
   InvoiceTarget.ToDate COLON 20
WITH OVERLAY ROW 3 centered
   COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) ac-hdr 
   SIDE-LABELS 
   FRAME lis.

form liNewITGroup
    HELP "Enter new invoice target group ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " MOVE TO NEW GROUP "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

IF iiMsSeq > 0 THEN ASSIGN
   FrmRow = 3
   FrmDown = 8.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

IF iiITGroupID > 0  THEN DO:
   FIND InvoiceTargetGroup WHERE 
        InvoiceTargetGroup.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE InvoiceTargetGroup THEN DO:
      MESSAGE "InvoiceTargetGroup not available"
      VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
END.

RUN local-Find-First.

IF AVAILABLE InvoiceTarget THEN ASSIGN
   Memory       = recid(InvoiceTarget)
   must-print   = TRUE
   must-add     = FALSE
   ldaDefFrom = TODAY.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No InvoiceTarget available!" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE
      ldaDefFrom    = TODAY.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.
   
   IF must-add THEN DO:  /* Add a InvoiceTarget  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        UpdateField:
        REPEAT TRANS WITH FRAME lis:
           
           DISPLAY iiITGroupID @ InvoiceTarget.ITGroupID.
           DISPLAY ldaDefFrom @ InvoiceTarget.FromDate.
           DISPLAY 12/31/2049 @ InvoiceTarget.ToDate.

           ehto = 9.
           RUN Syst/ufkey.
           
           UPDATE
              liMsSeq
           WITH FRAME lis EDITING:
    
              READKEY.
        
              IF LOOKUP(KEYLABEL(LASTKEY),"f2") > 0 THEN NEXT.

              IF KEYLABEL(LASTKEY) = "F9" AND 
                 LOOKUP(FRAME-FIELD,
                        "liMsSeq") > 0 
              THEN DO:

                 IF FRAME-FIELD = "liMsSeq" THEN DO:
                    RUN Help/h-msseq.p(InvoiceTargetGroup.Custnum).
                    
                    IF siirto NE ? THEN 
                       DISP siirto @ liMsSeq WITH FRAME lis.
                 END.
               
                 ehto = 9.
                 RUN Syst/ufkey.
                 NEXT. 
              END.

              ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
              DO WITH FRAME lis:
                 PAUSE 0.
    
                 IF FRAME-FIELD = "liMsSeq" THEN DO:

                    FIND MobSub WHERE
                         MobSub.MsSeq = INPUT liMsSeq
                    NO-LOCK NO-ERROR.
    
                    IF NOT AVAIL MobSub THEN DO:
                       MESSAGE "Unknown subscription ID"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.

                 END.
               END.
              
              APPLY LASTKEY.
           END.
           
           liInvoiceTargetID =_fAddInvoiceTarget(InvoiceTargetGroup.ITGroupId, 
                              liMsseq,
                              OUTPUT lcError).

           IF lcError NE "" THEN DO:
              MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
              NEXT UpdateField.
           END.

           FIND InvoiceTarget WHERE
                InvoiceTarget.InvoiceTargetID = liInvoiceTargetID NO-LOCK.

           LEAVE.

        END.

        IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN 
        UNDO add-row, LEAVE add-row.

        ASSIGN
        Memory = recid(InvoiceTarget)
        xrecid = Memory.  
        LEAVE.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE InvoiceTarget THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND InvoiceTarget WHERE recid(InvoiceTarget) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE InvoiceTarget THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(InvoiceTarget).
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
        ufk   = 0
        ufk[5]= (IF lcRight = "RW" AND iiITGroupID > 0 THEN 5 ELSE 0)  
/*        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) */
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0.
 
          
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvoiceTarget.InvoiceTargetID {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvoiceTarget.InvoiceTargetID WITH FRAME sel.
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
        FIND InvoiceTarget WHERE recid(InvoiceTarget) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE InvoiceTarget THEN
              ASSIGN FIRSTrow = i Memory = recid(InvoiceTarget).
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
           IF NOT AVAILABLE InvoiceTarget THEN DO:
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
                rtab[1] = recid(InvoiceTarget)
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
           IF NOT AVAILABLE InvoiceTarget THEN DO:
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
              rtab[FRAME-DOWN] = recid(InvoiceTarget).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND InvoiceTarget WHERE recid(InvoiceTarget) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE InvoiceTarget THEN DO:
           Memory = recid(InvoiceTarget).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE InvoiceTarget THEN Memory = recid(InvoiceTarget).
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
           FIND InvoiceTarget WHERE recid(InvoiceTarget) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       ASSIGN ac-hdr = " VIEW " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-VIEW-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       
       IF must-print EQ TRUE THEN NEXT LOOP.

       RUN local-disp-row.
       xrecid = recid(InvoiceTarget).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(InvoiceTarget) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(InvoiceTarget) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND InvoiceTarget WHERE recid(InvoiceTarget) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND InvoiceTarget WHERE recid(InvoiceTarget) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF iiMsSeq > 0 THEN 
      FIND FIRST invoicetarget USE-INDEX MsSeq WHERE 
                invoicetarget.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN FIND FIRST InvoiceTarget WHERE 
      InvoiceTarget.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiMsSeq > 0 THEN 
      FIND LAST invoicetarget USE-INDEX MsSeq WHERE 
                invoicetarget.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN FIND LAST InvoiceTarget WHERE 
      InvoiceTarget.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiMsSeq > 0 THEN 
      FIND NEXT invoicetarget USE-INDEX MsSeq WHERE 
                invoicetarget.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN FIND NEXT InvoiceTarget WHERE 
      InvoiceTarget.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiMsSeq > 0 THEN 
      FIND PREV invoicetarget USE-INDEX MsSeq WHERE 
                invoicetarget.MsSeq = iiMsSeq NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN FIND PREV InvoiceTarget WHERE 
      InvoiceTarget.ITGroupID = iiITGroupID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      InvoiceTarget.ITGroupID
      InvoiceTarget.InvoiceTargetID
      InvoiceTarget.MsSeq
      lcMSISDN
      InvoiceTarget.FromDate
      InvoiceTarget.ToDate
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcMSISDN = "".
   FIND MobSub WHERE
        MobSub.MsSeq = InvoiceTarget.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL MobSub THEN lcMSISDN = MobSub.CLI.
   ELSE DO:
      FIND TermMobSub WHERE
           TermMobSub.MsSeq = InvoiceTarget.MsSeq NO-LOCK NO-ERROR.
      IF AVAIL TermMobSub THEN lcMSISDN = TermMobSub.CLI.
   END.

END PROCEDURE.

PROCEDURE local-VIEW-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         InvoiceTarget.ITGroupID
         InvoiceTarget.InvoiceTargetID
         InvoiceTarget.MsSeq @ liMsSeq
         lcMSISDN
         InvoiceTarget.FromDate
         InvoiceTarget.ToDate
      WITH FRAME lis.

      ASSIGN 
         ufk    = 0
         ufk[6] = 1752
         ufk[7] = 1522 WHEN lcRight EQ "RW" 
         ufk[8] = 8
         ehto   = 0.
      
      RUN Syst/ufkey.
      
      IF toimi = 6 THEN DO: 
         RUN Mc/eventsel.p("invoicetarget", 
                        STRING(InvoiceTarget.InvoiceTargetID)).
      END.   
   
      IF toimi = 7 THEN do:

         DEFINE VARIABLE lcMenuOptions AS CHARACTER NO-UNDO. 
         DEFINE VARIABLE lcSelected AS CHARACTER NO-UNDO. 

         lcMenuOptions = "MOVE TO ANOTHER GROUP".
         
         RUN Syst/selectbox.p(
            "INVOICE TARGET",
            lcMenuOptions,
            OUTPUT lcSelected).
               
         CASE lcSelected:
                              
            WHEN "MOVE TO ANOTHER GROUP" THEN DO:
                
               ehto = 9. RUN Syst/ufkey. ufkey = true.
               CLEAR FRAME f1.
               REPEAT WITH FRAM f1 ON ENDKEY UNDO, LEAVE.

                  SET liNewITGroup WITH FRAME f1.

                  IF LOOKUP(KEYLABEL(LASTKEY),"F1,ENTER,RETURN") > 0 THEN DO:
                     
                     fMoveInvoiceTarget(
                        InvoiceTarget.InvoiceTargetID,
                        liNewITGroup,
                        "",
                        OUTPUT lcError).

                     IF lcError NE "" THEN DO:
                        MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
                        NEXT.
                     END.
                     ELSE DO:
                        
                        MESSAGE "Invoice target is moved to group " liNewITGroup
                        VIEW-AS ALERT-BOX.
                        
                        IF NOT AVAIL InvoiceTarget THEN DO:
                           RUN local-find-first.
                           must-print = TRUE.
                           LEAVE.
                        END.
                     END.
                     LEAVE.
                  END.
               END. 
               HIDE FRAME f1 NO-PAUSE.
               
            END.  

         END.      

      END.   
      
      ELSE IF toimi = 8 THEN LEAVE.
      
   END.

END PROCEDURE.

