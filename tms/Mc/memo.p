/* ----------------------------------------------------------------------
  MODULE .......: memo.p
  TASK .........: UPDATE memo records
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 30.10.2001 pt
  CHANGED ......: 10.04.02 lp cancel button corrected.
                  05.11.02 jr Eventlog
                  11.11.02 jr Change button...
                  04.03.03 tk tokens
                  05.09.03 aam brand
                  06.02.04 jp  input parameter custnum
                  11.02.04 aam prinmemo.p for printing customer related memos,
                               view send log (itsendlo)
                  24.06.04 tk token changed from HostTable to 'Memo'
                  20.03.07 kl /tmp/memoprint.dat
                               
  Version ......: M15
 ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Memo

{Syst/commali.i} 
{Syst/eventval.i}
{Func/fuserright.i}            

DEF INPUT PARAMETER iCustNum  LIKE memo.CustNum    NO-UNDO.
DEF INPUT PARAMETER HostTable LIKE memo.HostTable  NO-UNDO.
DEF INPUT PARAMETER KeyValue  LIKE memo.KeyValue   NO-UNDO.
DEF INPUT PARAMETER ftitle    AS  C FORMAT "x(20)" NO-UNDO.

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Memo'} 
/* check for hosttable, not memo table */

DEF VAR MemoTitle  LIKE memo.MemoTitle  NO-UNDO.
DEF VAR UserCode  LIKE memo.CreUser NO-UNDO.

DEF VAR xrecid       AS RECID                           INIT ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  INIT 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  INIT 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  INIT 10.
DEF VAR order        AS INT                    NO-UNDO  INIT 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  INIT 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  INIT TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  INIT 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS LOG FORMAT "Yes/No"    NO-UNDO.
DEF VAR CommitDate   AS C                      NO-UNDO.
DEF VAR new_memo     AS LOG                    NO-UNDO INIT FALSE.
DEF VAR lcSystUser   AS CHAR                   NO-UNDO.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMemo AS HANDLE NO-UNDO.
   lhMemo = BUFFER Memo:HANDLE.
   RUN StarEventInitialize(lhMemo).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhMemo).
   END.
END.

form
    Memo.Brand          FORMAT "x(4)" COLUMN-LABEL "Brand" 
    MeMo.CustNum                                        
    memo.MemoTitle      FORMAT "x(20)"
    memo.CreUser        COLUMN-LABEL "Created By"
    CommitDate          COLUMN-LABEL "Date" FORMAT "x(10)"

WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " MEMOS OF " + fTitle + " " + KeyValue + " "
    FRAME sel.

form
    memo.MemoTitle     /* LABEL FORMAT */ 
    WITH  OVERLAY ROW 2 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc)
    ac-hdr WITH side-labels
    FRAME lis1.

form
    memo.MemoText VIEW-AS EDITOR Size 60 BY 10 SCROLLBAR-VERTICAL
    WITH  OVERLAY ROW 5 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) " memo TEXT " FRAME lis2 NO-LABELS.

{Func/brand.i}

form /* seek Status Code  BY  MemoTitle */
    MemoTitle
    help "Enter Status Code"
    WITH ROW 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND CODE "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Status Code  BY UserCode */
    UserCode
    help "Enter Status"
    WITH ROW 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND Name "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f2.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
view FRAME sel.

lcSystUser = fTokenRights(katun,"SYST").

orders = "By Code,By Name,By 3, By 4".

FIND FIRST memo WHERE 
           memo.Brand     = lcBrand   AND 
           (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND 
           memo.HostTable = HostTable AND 
           memo.KeyValue  = KeyValue NO-LOCK NO-ERROR.
IF AVAILABLE memo THEN ASSIGN
   memory       = RECID(memo)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No memos available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a memo  */
      ASSIGN Syst.CUICommon:cfc = "lis" ufkey = TRUE ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis1 ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.   
        ON F4 GO.
        REPEAT TRANSACTION WITH FRAME lis1:
           RELEASE Memo.

           CLEAR FRAME lis1 NO-PAUSE.
           PROMPT-FOR Memo.MemoTitle.

           IF INPUT frame lis1 Memo.MemoTitle = "" THEN LEAVE add-row.
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              KEYLABEL(lastkey) = "F4" THEN
              LEAVE ADD-ROW.
            REPEAT ON ENDKEY UNDO, LEAVE:
               PROMPT memo.MemoText WITH FRAME lis2.
               LEAVE.
            END.
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              KEYLABEL(lastkey) = "F4" THEN
              UNDO ADD-ROW, LEAVE ADD-ROW.

           CREATE Memo.
           ASSIGN 
              Memo.Brand   = lcBrand
              memo.CreStamp  = Func.Common:mMakeTS()
              memo.Custnum   = iCustnum
              memo.HostTable = HostTable
              memo.KeyValue  = KeyValue
              memo.CreUser   = katun
              memo.MemoTitle = INPUT FRAME lis1 Memo.MemoTitle
              memo.MemoText = INPUT FRAME lis2 Memo.MemoText
              memo.MemoSeq = NEXT-VALUE(MemoSeq).
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMemo).

           ASSIGN
           memory = RECID(memo)
           xrecid = memory.
           FIND CURRENT Memo NO-LOCK.

           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis1 NO-PAUSE.
      HIDE FRAME lis2 NO-PAUSE.
      ASSIGN must-print = TRUE.
      /* is there ANY record ? */
      FIND FIRST memo
      WHERE Memo.Brand     = lcBrand   AND
       (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND
            memo.HostTable = HostTable AND 
            memo.KeyValue  = KeyValue NO-LOCK NO-ERROR.

      IF NOT AVAILABLE memo THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND memo WHERE RECID(memo) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE memo THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = RECID(memo).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        UP FRAME-LINE - 1.
        DOWN FIRSTROW.
        ASSIGN FIRSTROW = 0
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
        Syst.CUICommon:ufk[1]= 0   Syst.CUICommon:ufk[2]= 0  
        Syst.CUICommon:ufk[3]= 1796   
        Syst.CUICommon:ufk[4]= 983
        Syst.CUICommon:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.CUICommon:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        Syst.CUICommon:ufk[7]= 991 Syst.CUICommon:ufk[8]= 8   Syst.CUICommon:ufk[9]= 1
        Syst.CUICommon:ehto = 3    ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW memo.MemoTitle {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) memo.MemoTitle WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW memo.CreUser {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) memo.CreUser WITH FRAME sel.
      END.
      Syst.CUICommon:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.CUICommon:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(Syst.CUICommon:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.CUICommon:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND memo WHERE RECID(memo) = memory NO-LOCK NO-ERROR.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE memo THEN
              ASSIGN FIRSTrow = i memory = RECID(memo).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous ROW */
      IF LOOKUP(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE memo THEN DO:
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
                rtab[1] = RECID(memo)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE memo THEN DO:
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
              rtab[FRAME-DOWN] = RECID(memo).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND memo WHERE RECID(memo) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE memo THEN DO:
           memory = RECID(memo).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE memo THEN memory = RECID(memo).
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
     ELSE IF LOOKUP(Syst.CUICommon:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND memo WHERE RECID(memo) = memory NO-LOCK NO-ERROR.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* view send log */
     ELSE IF LOOKUP(Syst.CUICommon:nap,"3,f3") > 0 THEN DO:  
       RUN local-find-this (FALSE).
       RUN Mc/itsendlo.p(0,
                    0,
                    2,
                    Memo.MemoSeq).
       ufkey = TRUE.
       NEXT LOOP.

     END. 


     ELSE IF LOOKUP(Syst.CUICommon:nap,"4,F4") > 0 THEN DO:
        RUN about-memo.
        NEXT.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF katun NE Memo.CreUser AND lcSystUser NE "RW" THEN DO:
          MESSAGE "Only creator can delete the memo."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT. 
       END.       
             
       RUN local-find-NEXT.
       IF AVAILABLE memo THEN memory = RECID(memo).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE memo THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = RECID(memo).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).
       IF LOCKED memo THEN DO:
          MESSAGE "Memo record is locked by some other user." VIEW-AS ALERT-BOX.
          RUN local-find-this(false).
          delrow = 0.
          LEAVE.
       END. /* IF NOT AVAILABLE memo THEN DO: */

       IF NOT AVAILABLE memo THEN DO:
          MESSAGE "Memo record is not available." VIEW-AS ALERT-BOX.
          delrow = 0.
          LEAVE.
       END. /* IF NOT AVAILABLE memo THEN DO: */

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.CUICommon:ctc)
       memo.MemoTitle memo.CreUser CommitDate .
 
       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.CUICommon:ccc)
       memo.MemoTitle memo.CreUser CommitDate .
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMemo).
           DELETE memo.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST memo
           WHERE Memo.Brand     = lcBrand   AND
            (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND
                 memo.HostTable = HostTable AND 
                 memo.KeyValue  = KeyValue) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.CUICommon:nap,"7,f7") > 0 THEN DO:  /* PRINT */
        
        FIND memo WHERE RECID(memo) = rtab[FRAME-LINE(sel)] NO-LOCK NO-ERROR.
        
        IF Memo.CustNum > 0 
        THEN RUN Mc/prinmemo.p (Memo.HostTable,  
                           Memo.KeyValue,
                           Memo.MemoSeq).
        
        ELSE RUN Mc/prmem.p (INPUT Memo.HostTable,
                        INPUT Memo.KeyValue,
                        INPUT Memo.MemoSeq).
        ufkey = TRUE.          
        NEXT.
     END.

     /* change */
     ELSE IF LOOKUP(Syst.CUICommon:nap,"enter,return") > 0 THEN DO:
       task:
       REPEAT WITH FRAME lis1 TRANSACTION
       ON ENDKEY UNDO, LEAVE:

         RUN local-find-this(FALSE).

         IF NOT AVAILABLE memo THEN DO:
            MESSAGE "Memo record is not available." VIEW-AS ALERT-BOX.
            LEAVE.
         END. /* IF NOT AVAILABLE memo THEN DO: */

         ASSIGN ac-hdr = " Title ".
         Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. 
         CLEAR FRAME lis1 NO-PAUSE.
         HIDE FRAME sel NO-PAUSE.
         DISPLAY memo.MemoTitle.
         RUN local-find-others.
         DISP memo.MemoText
         WITH FRAME lis2.
         
         ASSIGN
          Syst.CUICommon:ufk = 0
          Syst.CUICommon:ufk[1] = (IF lcRight = "RW" THEN 7 ELSE 0)
          Syst.CUICommon:ufk[8] = 8
          Syst.CUICommon:ehto = 0.

         /* only owner can change a memo */
         IF katun NE Memo.CreUser AND lcSystUser NE "RW" THEN Syst.CUICommon:ufk[1] = 0.
          
         RUN Syst/ufkey.p.

         IF Syst.CUICommon:toimi = 1 AND lcRight = "RW" THEN
         DO:
            ufkey = TRUE. Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
            RUN local-update-record.                                  
         
            /* IF  User Wanted TO Cancel this Change TRANSACTION */
            IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
            KEYLABEL(lastkey) = "F4" THEN 
            DO:
               ufkey = TRUE. Syst.CUICommon:ehto = 9.
               HIDE FRAME lis1 NO-PAUSE.
               HIDE FRAME lis2 NO-PAUSE.
               RUN local-disp-row.
               UNDO, LEAVE.
            END.
         
            FIND CURRENT Memo EXCLUSIVE-LOCK.
         
            IF LOCKED memo THEN DO:
               MESSAGE "Memo record is locked by some other user."
               VIEW-AS ALERT-BOX.
               UNDO, LEAVE.          
            END. /* IF NOT AVAILABLE memo THEN DO: */

            IF CURRENT-CHANGED Memo THEN DO:
               
               FIND CURRENT Memo NO-LOCK.
               
               MESSAGE 
                  "This record has been changed elsewhere while updating" 
               VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

               HIDE FRAME lis1 NO-PAUSE.
               HIDE FRAME lis2 NO-PAUSE.
               RUN local-disp-row.
               UNDO, LEAVE.
            END.
            ELSE DO:
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMemo).
               ASSIGN
                  Memo.MemoTitle
                  Memo.MemoText.
               memo.ChgUser  = katun.
               memo.ChgStamp = Func.Common:mMakeTS().
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMemo).
            END.
         END.

         FIND CURRENT Memo NO-LOCK.

         IF Syst.CUICommon:toimi = 8 THEN RUN local-find-others.

         ufkey = TRUE. Syst.CUICommon:ehto = 9.
         HIDE FRAME lis1 NO-PAUSE.
         HIDE FRAME lis2 NO-PAUSE.

         RUN local-disp-row.
         xrecid = RECID(memo).
         LEAVE.  
       END.
       RUN local-find-this (FALSE).
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = RECID(memo) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = RECID(memo) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS LO NO-UNDO.

    IF exlock THEN
      FIND memo WHERE RECID(memo) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    ELSE
      FIND memo WHERE RECID(memo) = rtab[frame-line(sel)] 
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST memo WHERE
        Memo.Brand     = lcBrand   AND
 (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND        
        memo.HostTable = HostTable AND 
        memo.KeyValue  = KeyValue 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST memo WHERE 
          Memo.Brand     = lcBrand   AND
 (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND
          memo.HostTable = HostTable AND 
          memo.KeyValue  = KeyValue 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT memo WHERE 
          Memo.Brand     = lcBrand   AND
 (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND          
          memo.HostTable = HostTable AND 
          memo.KeyValue  = KeyValue 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev memo WHERE 
          Memo.Brand     = lcBrand   AND
 (IF iCustNum = 0 THEN TRUE ELSE memo.CustNum  = iCustNum)  AND
          memo.HostTable = HostTable AND 
          memo.KeyValue  = KeyValue 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   DISPLAY 
   memo.Brand
   memo.MemoTitle
   memo.CreUser
   CommitDate
   WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   CommitDate = Func.Common:mTS2HMS(memo.CreStamp).
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP memo.MemoText
      WITH FRAME lis2.

      PROMPT-FOR memo.MemoTitle WITH FRAME lis1.
      PROMPT-FOR memo.MemoText  WITH FRAME lis2.

      LEAVE.
   END.
END PROCEDURE.

PROCEDURE about-memo:

    DEF VAR modtext AS C  NO-UNDO.

    RUN LOCAL-FIND-THIS(FALSE).

    IF memo.ChgStamp NE 0 THEN
    modtext =   "and modified by user " + memo.ChgUser + 
                " at " + Func.Common:mTS2HMS(memo.ChgStamp).

    MESSAGE
    "Memo '" + memo.MemoTitle + "'" SKIP
    "was created by user" memo.CreUser "at" Func.Common:mTS2HMS(memo.CreStamp) SKIP
    modtext
    VIEW-AS ALERT-BOX TITLE " ABOUT ". 

END PROCEDURE.



