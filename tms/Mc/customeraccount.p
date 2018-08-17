/* ----------------------------------------------------------------------
  MODULE .......: customeraccount.P
  TASK .........: Customer Account for the Order
  APPLICATION ..: TMS
  AUTHOR .......: srvuddan
  CREATED ......: 11-08-2018
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Syst/eventval.i}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   
   {Func/lib/eventlog.i}
      
   DEFINE VARIABLE lhCustomerAccount AS HANDLE NO-UNDO.
   lhCustomerAccount = BUFFER CustomerAccount:HANDLE.
   RUN StarEventInitialize(lhCustomerAccount).
              
ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCustomerAccount).
   END.
   
END.

DEF INPUT PARAMETER iiAccountID AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto       AS CHAR.

DEF VAR CustomerAccount LIKE CustomerAccount.AccountID NO-UNDO.
DEF VAR xrecid       AS RECID init ?.
DEF VAR FIRSTrow     AS INT   NO-UNDO init 0.
DEF VAR FrmRow       AS INT   NO-UNDO init 1.
DEF VAR FrmDown      AS INT   NO-UNDO init 15.
DEF VAR order        AS INT   NO-UNDO init 1.
DEF VAR orders       AS CHAR  NO-UNDO.
DEF VAR maxOrder     AS INT   NO-UNDO init 2.
DEF VAR ufkey        AS LOG   NO-UNDO init TRUE.
DEF VAR delrow       AS INT   NO-UNDO init 0.
DEF VAR pr-order     AS INT   NO-UNDO.
DEF VAR Memory       AS RECID NO-UNDO.
DEF VAR RowNo        AS INT   NO-UNDO.
DEF VAR must-print   AS LOG   NO-UNDO.
DEF VAR must-add     AS LOG   NO-UNDO.
DEF VAR ac-hdr       AS CHAR  NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24 NO-UNDO.
DEF VAR i            AS INT   NO-UNDO.
DEF VAR ok           AS log   format "Yes/No" NO-UNDO.

DEF VAR lcHeader     AS CHAR  NO-UNDO.

form
    CustomerAccount.AccountID   FORMAT ">>>>>>>9" COLUMN-LABEL "AccountID"
    CustomerAccount.CustNum     FORMAT ">>>>>>>>9"
    CustomerAccount.DefaultAcc  FORMAT "Yes/No" 
    CustomerAccount.AccountName FORMAT "X(15)" 
    CustomerAccount.statuscode  FORMAT ">>>>>>>9"
    WITH OVERLAY ROW FrmRow WIDTH 80 CENTERED SCROLL 1 FrmDown DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " CUSTOMER ACCOUNT OF ORDER:" + STRING(iiAccountID) + " " FRAME sel.
    
form
    CustomerAccount.AccountID   COLON 20  
    CustomerAccount.CustNum     COLON 20 
    CustomerAccount.DefaultAcc  COLON 20 
    CustomerAccount.AccountName COLON 20 
    CustomerAccount.statuscode  COLON 20 
    CustomerAccount.FromDate    COLON 20
    CustomerAccount.ToDate      COLON 20
    WITH  OVERLAY ROW 4 CENTERED 
    COLOR VALUE(Syst.Var:cfc)
    TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.
    
IF iiAccountID > 0 THEN ASSIGN 
   order = 1
   maxorder = 1
   FrmRow = 3
   FrmDown = 10.
       
Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. 
ASSIGN 
   Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE CustomerAccount THEN ASSIGN
        Memory     = recid(CustomerAccount)
        must-print = TRUE
        must-add   = FALSE.
ELSE DO: 
MESSAGE "No Customer Account available !" VIEW-AS ALERT-BOX.
ASSIGN
        Memory     = ?
        must-print = FALSE
        must-add   = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN 
   DO:
      pr-order = order.
   END.

   IF must-add THEN 
   DO:  /* Add a CustomerAccount  */
      ASSIGN 
         Syst.Var:cfc = "lis" 
         ufkey        = true 
         ac-hdr       = " ADD " 
         must-add     = FALSE.
     RUN Syst/ufcolor.p.

     ADD-ROW:
        REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
           PAUSE 0 NO-MESSAGE.
           Syst.Var:ehto = 9. RUN Syst/ufkey.p.
           
           REPEAT TRANSACTION WITH FRAME lis
           ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW:
           
              CLEAR FRAME lis NO-PAUSE.
              
              PROMPT-FOR CustomerAccount.AccountID
                 VALIDATE(CustomerAccount.AccountID = "" OR
                 NOT can-find(CustomerAccount WHERE 
                 CustomerAccount.AccountID = INPUT FRAME lis CustomerAccount.AccountID),
                 "CustomerAccount " + string(INPUT CustomerAccount.AccountID) +
                 " already exists !").
              IF INPUT CustomerAccount.AccountID NOT ENTERED THEN 
                 LEAVE add-row.
               
              CREATE CustomerAccount.
              ASSIGN
                 CustomerAccount.AccountID = INPUT FRAME lis 
                                         CustomerAccount.AccountID.

              RUN local-UPDATE-record.

              IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
                 UNDO add-row, LEAVE add-row.

              IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustomerAccount).
              
              ASSIGN
                 Memory = recid(CustomerAccount)
                 xrecid = Memory.
              LEAVE.
           END.
        END.  /* ADD-ROW */
        
        HIDE FRAME lis NO-PAUSE.
        ASSIGN 
           must-print = TRUE.

        /* is there ANY record ? */
        FIND FIRST CustomerAccount WHERE 
                   CustomerAccount.AccountID = iiAccountID
       /* srule */ NO-LOCK NO-ERROR.
        IF NOT AVAILABLE CustomerAccount THEN LEAVE LOOP.
        NEXT LOOP.
    END.
    
    PrintPage:
    DO:
       IF must-print THEN 
       DO:
          UP FRAME-LINE - 1.
          FIND CustomerAccount WHERE recid(CustomerAccount) = Memory NO-LOCK NO-ERROR.

          /* DISPLAY one page beginning the record 
          whose RECID is saved into 'Memory'.
          starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
          IF delrow > 0 THEN DOWN delrow - 1.

          REPEAT WITH FRAME sel:
             IF AVAILABLE CustomerAccount THEN 
                DO:
                   RUN local-disp-row.
                   rtab[FRAME-LINE] = recid(CustomerAccount).
                   RUN local-find-NEXT.
                END.
             ELSE 
             DO:
                CLEAR NO-PAUSE.
                rtab[FRAME-LINE] = ?.
             END.
             IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
             DOWN.
          END.
          up FRAME-LINE - 1.
          DOWN FIRSTrow.
          ASSIGN 
             FIRSTrow   = 0
             must-print = FALSE.
          PAUSE 0 NO-MESSAGE.

       /* Now there is one page DISPLAYed AND the cursor is on the
       upermost ROW, waiting FOR a 'choose' */
       END. /* must-print = TRUE */
    END. /* PrintPage */
    
    IF delrow > 0 THEN DOWN delrow - 1.
    ASSIGN 
       delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
            Syst.Var:ufk[1]= 0 Syst.Var:ufk[2]= 0 Syst.Var:ufk[3]= 0  Syst.Var:ufk[4]= 0
            Syst.Var:ufk[5]= 0  Syst.Var:ufk[6]= 0 Syst.Var:ufk[7]= 0 Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
            Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.

      END.
      
      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW CustomerAccount.AccountID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) CustomerAccount.AccountID WITH FRAME sel.
      END.
  /*    ELSE IF order = 2 THEN DO:
         CHOOSE ROW CustomerAccount.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(Syst.Var:ccc) CustomerAccount.CustNum WITH FRAME sel.
      END.  
*/
      Syst.Var:nap = keylabel(LASTKEY).
      
      IF rtab[FRAME-LINE] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.   
      END. 
        
      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 AND maxorder > 1 THEN DO:
         order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 AND maxorder > 1 THEN DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.
      
      IF order <> pr-order AND MaxOrder > 1 THEN DO:
         ASSIGN 
            FIRSTrow = 0 
            Memory   = rtab[FRAME-LINE].
         FIND CustomerAccount WHERE recid(CustomerAccount) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-PREV.
            IF AVAILABLE CustomerAccount THEN
               ASSIGN FIRSTrow = i Memory   = recid(CustomerAccount).
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
            IF NOT AVAILABLE CustomerAccount THEN DO:
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
                  rtab[1] = recid(CustomerAccount)
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
            IF NOT AVAILABLE CustomerAccount THEN DO:
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
               rtab[FRAME-DOWN] = recid(CustomerAccount).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
           END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT ROW */
      
      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
         Memory = rtab[1].
         FIND CustomerAccount WHERE recid(CustomerAccount) = Memory NO-LOCK NO-ERROR.
         RUN local-find-PREV.
         IF AVAILABLE CustomerAccount THEN DO:
            Memory = recid(CustomerAccount).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-PREV.
               IF AVAILABLE CustomerAccount THEN Memory = recid(CustomerAccount).
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
            FIND CustomerAccount WHERE recid(CustomerAccount) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */
     
      ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
         REPEAT WITH FRAME lis TRANSACTION
         ON ENDKEY UNDO, LEAVE:
         
         /* change */
         {Syst/uright2.i}
         RUN local-find-this(FALSE).
         ASSIGN ac-hdr = " VIEW " ufkey = TRUE Syst.Var:ehto = 9.
         Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
         RUN local-disp-lis.
        
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
        
         RUN local-disp-row.
          
         xrecid = recid(CustomerAccount).
         LEAVE.
         
      END.
      
      ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(CustomerAccount) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(CustomerAccount) must-print = TRUE.
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
      FIND CustomerAccount WHERE recid(CustomerAccount) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
   ELSE
      FIND CustomerAccount WHERE recid(CustomerAccount) = rtab[frame-line(sel)] 
      NO-LOCK.
END PROCEDURE.
        
PROCEDURE local-find-FIRST:
   FIND FIRST CustomerAccount WHERE
              CustomerAccount.AccountID = iiAccountID
              NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST CustomerAccount WHERE
      CustomerAccount.AccountID = iiAccountID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT CustomerAccount WHERE
      CustomerAccount.AccountID = iiAccountID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV CustomerAccount WHERE
      CustomerAccount.AccountID = iiAccountID
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      CustomerAccount.AccountID
      CustomerAccount.CustNum
      CustomerAccount.DefaultAcc
      CustomerAccount.AccountName
      CustomerAccount.StatusCode
      WITH FRAME sel.       
            
END PROCEDURE.

PROCEDURE local-find-others:
END PROCEDURE.

PROCEDURE local-disp-lis:
   
   RUN local-find-others.
   CLEAR FRAME lis NO-PAUSE.
   DISPLAY
      CustomerAccount.AccountID
      CustomerAccount.CustNum
      CustomerAccount.DefaultAcc
      CustomerAccount.AccountName
      CustomerAccount.statuscode
      CustomerAccount.FromDate
      CustomerAccount.ToDate
      with frame lis. 
    
END PROCEDURE.    

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISPLAY
         CustomerAccount.AccountID
         CustomerAccount.CustNum
         CustomerAccount.DefaultAcc
         CustomerAccount.AccountName
         CustomerAccount.statuscode
         CustomerAccount.FromDate
         CustomerAccount.ToDate
         WITH FRAME lis.
      
      UPDATE
         CustomerAccount.DefaultAcc
         CustomerAccount.AccountName
         CustomerAccount.statuscode
         CustomerAccount.FromDate
         CustomerAccount.ToDate
         WITH FRAME lis. 
      LEAVE.
   END.
END PROCEDURE.
