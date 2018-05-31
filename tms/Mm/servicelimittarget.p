/* ----------------------------------------------------------------------
  MODULE .......: ServiceLimitTarget
  TASK .........: UPDATEs table ServiceLimitTarget
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 09.09.2002 ccn/prod help menu
                  05.11.2002 jr Eventlog
                  10.03.2003 tk tokens
  Version ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Mobsub'}

DEF INPUT PARAMETER iiSLSeq AS INT NO-UNDO.

DEF  NEW  shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
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
DEF VAR mtypename    AS C                      NO-UNDO.
DEF VAR mtypenames   AS C                      NO-UNDO.
DEF VAR membername   AS C  FORMAT "X(15)"                    NO-UNDO.

DEF BUFFER xxmember for ServiceLimitTarget.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhServiceLimitTarget AS HANDLE NO-UNDO.
   lhServiceLimitTarget = BUFFER ServiceLimitTarget:HANDLE.
   RUN StarEventInitialize(lhServiceLimitTarget).
               
   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhServiceLimitTarget).
   END.
END.


form
   ServiceLimitTarget.ServiceLMember
      membername    COLUMN-LABEL "Name" FORMAT "X(20)" 
   ServiceLimitTarget.InsideRate   COLUMN-LABEL "InSide"
   ServiceLimitTarget.outsideRate  COLUMN-LABEL "OutSide"
WITH ROW FrmRow  OVERLAY CENTERED FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) "Servicelimit  members " + 
       (if iislseq = 0 THEN "ALL" ELSE STRING(iislseq))
    FRAME sel.

form
   ServiceLimitTarget.ServiceLMember  
      COLON 30 LABEL "Billing Item" 
      membername NO-LABEL format "X(25)" SKIP
   ServiceLimitTarget.Insiderate  
      COLON 30 LABEL  "Inside B-Destination"  
      FORMAT "X(41)" SKIP
   ServiceLimitTarget.OutSiderate     
      COLON 30 LABEL "Outside B-Destination" 
      FORMAT "X(41)" SKIP(1)
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr
    SIDE-LABELS
    FRAME lis.

form /* seek  MemberType */
    Membertype
    HELP "Enter Member Of Fatime Group "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND MEMBER "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By Type  ,  By Member  ,By 3, By 4".


FIND FIRST ServiceLimitTarget 
WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
IF AVAILABLE ServiceLimitTarget THEN ASSIGN
   Memory       = recid(ServiceLimitTarget)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No ServiceLimit Target  members available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.      

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ServiceLimitTarget  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           PROMPT-FOR ServiceLimitTarget.ServiceLMember.
           
           IF INPUT FRAME lis ServiceLimitTarget.ServiceLMember = "" THEN 
           LEAVE add-row. 

           IF NOT CAN-FIND(FIRST BillItem WHERE
                  BillItem.Brand = Syst.Var:gcBrand AND 
                  BillItem.BillCode = INPUT ServiceLimitTarget.ServiceLMember) 
           THEN DO:
              BELL.
              MESSAGE 
              "Unknown Billing Item!".
              NEXT-PROMPT ServiceLimitTarget.ServiceLMember. next.
           ENd.             
                    
           CREATE ServiceLimitTarget.
           ASSIGN
           ServiceLimitTarget.Slseq          = iiSlSeq
           ServiceLimitTarget.ServiceLMember = 
               INPUT ServiceLimitTarget.ServiceLMember.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServiceLimitTarget).
           ASSIGN
           Memory = recid(ServiceLimitTarget)
           xrecid = Memory.
           LEAVE add-row.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ServiceLimitTarget

      WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServiceLimitTarget THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServiceLimitTarget THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServiceLimitTarget).
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
        Syst.Var:ufk[1]= 0  Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
        Syst.Var:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        Syst.Var:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServiceLimitTarget.ServiceLMember {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) ServiceLimitTarget.ServiceLMember WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAIL  ServiceLimitTarget THEN
              ASSIGN FIRSTrow = i Memory = recid(ServiceLimitTarget).
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

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ServiceLimitTarget THEN DO:
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
                rtab[1] = recid(ServiceLimitTarget)
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
           IF NOT AVAILABLE ServiceLimitTarget THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServiceLimitTarget).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServiceLimitTarget THEN DO:
           Memory = recid(ServiceLimitTarget).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServiceLimitTarget THEN Memory = recid(ServiceLimitTarget).
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
           FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
        ServiceLimitTarget.ServiceLMember 
        ServiceLimitTarget.InsideRate
        ServiceLimitTarget.OutsideRate
        with frame sel.

       RUN local-find-NEXT.
       IF AVAILABLE ServiceLimitTarget THEN Memory = recid(ServiceLimitTarget).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServiceLimitTarget THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServiceLimitTarget).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       ServiceLimitTarget.ServiceLMember with frame sel.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServiceLimitTarget).
           DELETE ServiceLimitTarget.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServiceLimitTarget
           WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq)) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY 
       ServiceLimitTarget.ServiceLMemBer.
         
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServiceLimitTarget).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServiceLimitTarget).
       RUN local-disp-row.
       xrecid = recid(ServiceLimitTarget).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServiceLimitTarget) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServiceLimitTarget) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServiceLimitTarget WHERE recid(ServiceLimitTarget) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServiceLimitTarget 

       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ServiceLimitTarget 
       USE-INDEX SLSeq
       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServiceLimitTarget 


       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ServiceLimitTarget USE-INDEX SLSeq
       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServiceLimitTarget 


       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ServiceLimitTarget USE-INDEX SLSeq
       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServiceLimitTarget 

       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ServiceLimitTarget USE-INDEX SLSeq
       WHERE (IF iiSLSeq = 0 THEN TRUE ELSE ServiceLimitTarget.SLSeq = iiSLSeq) NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
         ServiceLimitTarget.ServiceLMember
          ServiceLimitTarget.InsideRate
          ServiceLimitTarget.OutsideRate
         membername
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND first BillItem WHERE 
              BillItem.Brand = Syst.Var:gcBrand AND
              BillItem.BillCode       = ServiceLimitTarget.serviceLMember 
   No-LOCK NO-ERROR.
               
   IF AVAIL BillItem THEN membername = BillItem.BIName.
   ELSE                   membername = "".            
              
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DISP
         membername 
         ServiceLimitTarget.ServiceLMember
      WITH FRAME lis. 


      IF lcRight = "RW" THEN DO:

         UPDATE
            ServiceLimitTarget.serviceLMember WHEN NOT NEW ServiceLimitTarget
            ServiceLimitTarget.insiderate
            ServiceLimitTarget.Outsiderate
            
            WITH FRAME lis

         EDITING:
             
             READKEY.

             IF FRAME-FIELD = "ServiceLMember" AND keylabel(lastkey) = "F9"
             THEN DO:

                RUN Help/nntuse.p.
                
                IF CAN-FIND (FIRST xxmember WHERE 
                xxmember.ServiceLMember = ServiceLimitTarget.ServiceLMember AND
                xxmember.ServiceLMember  = siirto               AND
                recid(xxmember) NE recid(ServiceLimitTarget)) THEN DO:
                 
                 MESSAGE
                    "ServiceLimitTarget Member already exists!".         
                    NEXT-PROMPT ServiceLimitTarget.ServiceLMember. NEXT.         
                END.             
                
                IF siirto ne ? THEN 
                ASSIGN 
                ServiceLimitTarget.ServiceLMember = siirto.
                DISP ServiceLimitTarget.ServiceLMember with frame lis.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),Syst.Var:poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "ServiceLMember" THEN DO:
                   IF CAN-FIND (FIRST xxmember WHERE
                    xxMember.slseq            = Servicelimit.SLSeq AND 
                    xxmember.servicelmember   = ServiceLimitTarget.ServiceLMember      AND
                    xxmember.ServiceLMember  = INPUT FRAME lis 
                      ServiceLimitTarget.ServiceLMember                        AND
                       recid(xxmember) NE recid(ServiceLimitTarget)) THEN DO:
                       MESSAGE 
                       "Service limit already exists!".
                       NEXT-PROMPT ServiceLimitTarget.ServiceLMember. NEXT.
                   END.
                   
                   ASSIGN input frame lis ServiceLimitTarget.ServiceLMember.
               
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      END.
      ELSE PAUSE.
      
      LEAVE.
   END.
END PROCEDURE.

