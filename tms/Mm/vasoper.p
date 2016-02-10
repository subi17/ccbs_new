/* ----------------------------------------------------------------------
  MODULE .......: VASOper
  TASK .........: Updates table VASOperator
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 02.06.03 jp 
  CHANGED ......: 08.08.03/aam CustNum, AccNum, MinFee
                  02.10.03/aam BillCode
                  24.03.04 jp m15 version
  VERSION ......: m15
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}
        
    DEF VAR lhVASOper AS HANDLE NO-UNDO.
    lhVASOper = BUFFER VASOper:HANDLE.
    RUN StarEventInitialize(lhVASOper).
                    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhVASOper).
    END.
END.
                                    

DEF BUFFER xxoper FOR VASOper.
def  new  shared var siirto AS char.

DEF VAR OperID       Like VASOper.OperID        NO-UNDO.
DEF VAR VOName  Like VASOper.VOName        NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
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
DEF VAR lctype       AS CHAR FORMAT "X(25)"    NO-UNDO.


form
    VASOper.OperID     /* column-label */ format "x(4)" 
    VASOper.VOName     /* column-label format */ FORMAT "X(24)"
    VASOper.CustNum    FORMAT ">>>>>>>9" 
    VASOper.OrigPrice  column-label "Orig"  FORMAT "zzz9.999"
    VASOper.TermPrice  column-label "Term(own)" FORMAT "zzz9.999"

    VASOper.invFee 

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  VALUE ADDED SERVICE OPERATOR  MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
   "Operator .........:"  VASOper.OperID    format "x(4)"      SKIP                  "Operator Name.....:"  VASOper.VOName FORMAT "X(30)"        SKIP
   "Invoicing customer:"  VASOper.CustNum
      Customer.CustName FORMAT "X(30)" SKIP
   "Product ..........:"  VASOper.BillCode  
       BillItem.BIName NO-LABEL SKIP
   "Originating Price :"  VASOper.OrigPrice                    
      FORMAT ">>>>>9.999"            
      SKIP             
   "Term.Price Own ...:"  VASOper.TermPrice 
      FORMAT ">>>>>9.999"            
      HELP "Terminating to own network"                           SKIP   
   "Invoicing fee% ...:"  VASOper.invFee                       SKIP
   "Minimum fee ......:"  VASOper.MinFee     
       HELP "Minimum fee, used separately for each service"    SKIP
   "Debt account .....:"  VASOper.AccNum                      
       account.accname FORMAT "X(30)" SKIP
WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr  NO-LABELS 
    FRAME lis.

form /* seek  OperID */
    OperID
    HELP "Enter Code of Billing Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek VOName */
   VOName
    HELP "Enter Name of the Billing Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST VASOper
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE VASOper THEN ASSIGN
   memory       = recid(VASOper)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a VASOper  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           create VASOper.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhVASOper).
            
           ASSIGN
           memory = recid(VASOper)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST VASOper
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VASOper THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND VASOper WHERE recid(VASOper) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE VASOper THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(VASOper).
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
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 585
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row VASOper.OperID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASOper.OperID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row VASOper.VOName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASOper.VOName WITH FRAME sel.
      END.
      
      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND VASOper WHERE recid(VASOper) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE VASOper THEN
              ASSIGN FIRSTrow = i memory = recid(VASOper).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE VASOper THEN DO:
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
                rtab[1] = recid(VASOper)
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
           IF NOT AVAILABLE VASOper THEN DO:
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
              rtab[FRAME-down] = recid(VASOper).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND VASOper WHERE recid(VASOper) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE VASOper THEN DO:
           memory = recid(VASOper).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE VASOper THEN memory = recid(VASOper).
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
           FIND VASOper WHERE recid(VASOper) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET OperID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF OperID ENTERED THEN DO:
          FIND FIRST VASOper WHERE VASOper.OperID >= OperID
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VASOper THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VASOper/OperID was found */
          ASSIGN order = 1 memory = recid(VASOper) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET VOName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF VOName ENTERED THEN DO:
          FIND FIRST VASOper WHERE VASOper.VOName >=VOName
          USE-INDEX VOName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VASOper THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VASOper/VOName was found */
          ASSIGN order = 2 memory = recid(VASOper) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* add */
        RUN local-find-this (false).
        RUN Mm/vasbdest.p(input vasoper.operid).
        RUN Syst/ufkey.
     END.


     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       VASOper.OperID     /* column-label format */
       VASOper.VOName     /* column-label format */
       VASOper.OrigPrice
       VASOper.TermPrice


       VASOper.invFee.


       RUN local-find-NEXT.
       IF AVAILABLE VASOper THEN memory = recid(VASOper).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE VASOper THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(VASOper).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       VASOper.OperID     /* column-label format */
       VASOper.VOName     /* column-label format */
       VASOper.OrigPrice
       VASOper.TermPrice
       
       VASOper.invFee.
       
       IF ok THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhVASOper).
           
           DELETE VASOper.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST VASOper
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY VASOper.OperID.
       
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhVASOper).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhVASOper).
       
       RUN local-disp-row.
       xrecid = recid(VASOper).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(VASOper) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(VASOper) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find VASOper WHERE recid(VASOper) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find VASOper WHERE recid(VASOper) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST VASOper
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST VASOper USE-INDEX VOName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST VASOper
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST VASOper USE-INDEX VOName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT VASOper
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT VASOper USE-INDEX VOName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV VASOper
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV VASOper USE-INDEX VOName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       VASOper.OperID     /* column-label format */
       VASOper.CustNum 
       VASOper.VOName     /* column-label format */
       VASOper.OrigPrice
       VASOper.TermPrice
       VASOper.invFee

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.


END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      

      CLEAR FRAME lis NO-PAUSE.
      
      FIND Customer WHERE Customer.CustNum = VASOper.CustNum NO-LOCK NO-ERROR.
      FIND account WHERE account.AccNum  = VASOper.AccNum NO-LOCK NO-ERROR.
      FIND BillItem   WHERE BillItem.BillCode   = VASOper.BillCode NO-LOCK NO-ERROR.
      
      DISPLAY Customer.CustName WHEN AVAILABLE Customer
              account.AccName WHEN AVAILABLE account
              BillItem.BIName   WHEN AVAILABLE BillItem
              WITH FRAME lis.
              
      UPDATE
      VASOper.OperID     /* label format */
      VASOper.VOName    /* label format */
      VASOper.CustNum
      VASOper.BillCode
      VASOper.OrigPrice       
      VASOper.TermPrice                
      VASOper.invFee 
      VASOper.MinFee
      VASOper.AccNum

      WITH FRAME lis
      EDITING:
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "OperID" THEN DO:
                
                   FIND  FIRST xxOper WHERE
                               xxOper.OperId = INPUT vasoper.OperID AND
                               RECID(xxoper) ne RECID(vasoper)
                   NO-LOCK NO-ERROR.            
                   IF AVAIL xxoper THEN DO:   
                      BELL.
                      MESSAGE
                      "Operator ID "  INPUT vasoper.operId " already exists!"
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT OperID. NEXT.                
                   END.
                END.

                ELSE IF FRAME-FIELD = "VOname" THEN DO:
                   FIND  FIRST xxOper WHERE
                               xxOper.VOName = INPUT vasoper.VOName AND
                               RECID(xxOper) ne RECID(VasOper)
                   NO-LOCK NO-ERROR.            
                   IF AVAIL xxoper THEN DO:   
                      BELL.
                      MESSAGE
                      "Operator Name "  INPUT vasoper.VOName " already exists!"
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT VOName. NEXT.                
                   END.
                END.

                ELSE IF FRAME-FIELD = "CustNum" THEN DO:
                
                   IF INPUT VASOper.CustNum = 0 THEN 
                      DISPLAY "" @ Customer.CustName.
                   ELSE DO:
                      FIND Customer WHERE Customer.CustNum = INPUT VASOper.CustNum
                      NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE Customer THEN DO:
                         MESSAGE "Unknown customer.".
                         BELL.
                         NEXT.
                      END.
                      DISPLAY Customer.CustName WITH FRAME lis.
                   END.
                END.

                ELSE IF FRAME-FIELD = "BillCode" THEN DO:
                   FIND FIRST BillItem WHERE BillItem.BillCode =
                   INPUT FRAME lis VASOper.Billcode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Code!".
                      NEXT.
                   END.
                   DISP BillItem.BIName WITH FRAME lis.
                END.
                 
                ELSE IF FRAME-FIELD = "AccNum" THEN DO:
                
                   FIND account WHERE account.AccNum = INPUT VASOper.AccNum
                   NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE account THEN DO:
                      MESSAGE "Unknown account.".
                      BELL.
                      NEXT.
                   END.
                   DISPLAY account.AccName WITH FRAME lis.
                END.

             END.
             APPLY LASTKEY.
          END. /* EDITING */
             
      LEAVE.  
   END.
END PROCEDURE.
