/* -----------------------------------------------------------------------------
  MODULE .......: usersman.p
  TASK .........: Browse table UserSman
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 19.03.04
  CHANGED ......: 
  Version ......: M15
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE TMSCodeDef NO
&GLOBAL-DEFINE BrTable UserSman

{Syst/commali.i}
{Func/finvbal.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'UserSman'}
{Func/fduedate.i}
{Ar/invdet.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhUserSman AS HANDLE NO-UNDO.
   lhUserSman = BUFFER UserSman:HANDLE.
   RUN StarEventInitialize(lhUserSman).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhUserSman).
   END.

END.


DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO. 

DEF VAR cu-text  AS c no-undo.

DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 5.
DEF VAR FrmDown      AS int                    NO-UNDO  init 8.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR unpaid       AS LOG format "*/"        NO-UNDO.

form   
    UserSman.UserCode   
    UserSman.Brand      
    UserSman.Salesman
    Salesman.SMName 
WITH ROW FrmRow centered overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " USER'S SALESMAN DEFINITIONS  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    UserSman.UserCode   COLON 20
        TMSUser.UserName NO-LABEL SKIP
    UserSman.Brand      COLON 20
        Brand.BrName  NO-LABEL SKIP
    UserSman.Salesman   COLON 20 
        Salesman.SMName NO-LABEL SKIP
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FIND TMSUser WHERE TMSUser.UserCode = icUserCode NO-LOCK NO-ERROR.
IF NOT AVAILABLE TMSUser THEN DO:
   MESSAGE "Unknown user" icUserCode
   VIEW-AS ALERT-BOX.
   RETURN.
END.

RUN local-find-first.

IF AVAILABLE UserSman THEN ASSIGN
   memory       = recid(UserSman)
   must-print   = true
   must-add     = false.
ELSE DO:
   memory = ?.
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Brand  ,".


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a UserSman  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:

           DISPLAY TMSUser.UserCode @ UserSman.UserCode
                   TMSUser.UserName WITH FRAME lis.
                   
           PROMPT-FOR UserSman.Brand.

           IF INPUT FRAME lis UserSman.Brand = ""               
           THEN LEAVE add-row.

           IF NOT CAN-FIND(Brand WHERE Brand.Brand = INPUT UserSman.Brand)
           THEN DO:
              MESSAGE "Unknown brand"
              VIEW-AS ALERT-BOX.
              NEXT.
           END.
           
           IF CAN-FIND(FIRST UserSman WHERE
                             UserSman.UserCode = icUserCode AND
                             UserSman.Brand    = INPUT FRAME lis
                                                 UserSman.Brand)
           THEN DO:
              MESSAGE 
              "Salesman definition already exists for brand"
              INPUT FRAME lis UserSman.Brand
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE UserSman.
           ASSIGN
           UserSman.UserCode = icUserCode
           UserSman.Brand    = INPUT FRAME lis UserSman.Brand.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhUserSman).

           ASSIGN
           Memory = recid(UserSman)
           xrecid = Memory.  
           LEAVE.
        END.
     
      END.  /* ADD-ROW */
   
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE UserSman THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND UserSman WHERE recid(UserSman) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE UserSman THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(UserSman).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.
   
   BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, retuRN:

      IF ufkey THEN DO:
         ASSIGN
         ufk = 0
         ufk[5]= 5 
         ufk[6]= 4    
         ufk[8]= 8 
         ehto = 3 ufkey = false.
         
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row UserSman.Brand ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) UserSman.Brand WITH FRAME sel.
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
        order = order + 1. 
        IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND UserSman WHERE recid(UserSman) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE UserSman THEN
              ASSIGN FIRSTrow = i memory = recid(UserSman).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE UserSman THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(UserSman)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE UserSman THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(UserSman).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND UserSman WHERE recid(UserSman) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE UserSman THEN DO:
           memory = recid(UserSman).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE UserSman THEN memory = recid(UserSman).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND UserSman WHERE recid(UserSman) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
     
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " VIEW " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY UserSman.Brand.

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(UserSman).
       LEAVE.
     END.


     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
     
        delrow = FRAME-LINE.
        RUN local-find-this (FALSE).
                          
        RUN local-find-NEXT.
        
        IF AVAILABLE UserSman THEN Memory = recid(UserSman).
        ELSE DO:
           /* read back the record that is TO be  removed */
           RUN local-find-this (FALSE).
                      
           RUN local-find-PREV.
           IF AVAILABLE UserSman THEN DO:
              ASSIGN
              delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
              Memory = recid(UserSman).
           END.
        END.
     
        /* FIND back the ROW that is TO be removed */
        RUN local-find-this(TRUE).
             
        /* Highlight */
        COLOR DISPLAY VALUE(ctc)
        UserSman.UserCode
        UserSman.Brand
        UserSman.Salesman.
                              
        ASSIGN ok = FALSE.
        MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY VALUE(ccc)
        UserSman.UserCode
        UserSman.Brand
        UserSman.Salesman.
        
        IF ok THEN DO:
          
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUserSman).
           DELETE UserSman.
           
           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE UserSman THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
    
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE delrow = 0. /* UNDO DELETE */
        
     END. /* DELETE */
        
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(UserSman) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(UserSman) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
HIDE MESSAGE NO-PAUSE. 
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT parameter exlock AS LO NO-undo.

    IF exlock THEN
      FIND UserSman WHERE recid(UserSman) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND UserSman WHERE recid(UserSman) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   FIND FIRST UserSman WHERE 
              UserSman.UserCode = icUserCode
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
   FIND LAST UserSman WHERE 
             UserSman.UserCode = icUserCode
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   FIND NEXT UserSman WHERE 
             UserSman.UserCode = icUserCode
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   FIND PREV UserSman WHERE 
             UserSman.UserCode = icUserCode
   NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       UserSman.UserCode
       UserSman.Brand 
       UserSman.Salesman
       Salesman.SMName WHEN AVAILABLE Salesman
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
    FIND Salesman WHERE
         Salesman.Brand    = UserSman.Brand AND
         Salesman.Salesman = UserSman.Salesman NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      FIND Brand OF UserSman NO-LOCK NO-ERROR.
      IF AVAILABLE Brand THEN DISPLAY Brand.BrName WITH FRAME lis.
      
      DISP UserSman.UserCode
           TMSUser.UserName
           UserSman.Brand
           UserSman.Salesman
           Salesman.SmName WHEN AVAILABLE Salesman
           "" WHEN NOT AVAILABLE Salesman @ Salesman.SMName
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN Syst/ufkey.
         
         UPDATE
         UserSman.Salesman  
         WITH FRAME lis
         EDITING:

            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "Salesman" THEN DO:
                   IF INPUT FRAME lis UserSman.Salesman = ""
                   THEN DISPLAY "" ;& Salesman.SmName. 

                   ELSE DO:
                      FIND Salesman WHERE 
                           Salesman.Brand    = lcBrand AND
                           Salesman.Salesman =
                      INPUT FRAME lis UserSman.Salesman NO-LOCK NO-ERROR.
                      IF NOT AVAIL Salesman THEN DO:
                         BELL.
                         MESSAGE "Unknown Salesman !".
                         NEXT.
                      END.
                      DISP Salesman.SMName ;& Salesman.SmName.
                   END.
                END.

             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
END PROCEDURE.



