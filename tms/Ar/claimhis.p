/* -----------------------------------------------------------------------------
  MODULE .......: claimhis.p
  TASK .........: Browse table ClaimHist
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 27.05.02/aam 
  CHANGED ......: 07.06.02/aam use Invoice.OverPaym for overpayment
                  15.11.02 lp  use invdet.i for invoice details
                  05.03.03 tk  tokens
                  17.09.03/aam brand
                  04.12.03/aam delete, eventlog
                  30.01.04/aam input CustNum,
                               editor for memo
                  24.01.06/jt  DYNAMIC-FUNCTION("fDispCustName"
                  16.06.06/aam ClaimState instead of Claim and ClaimQty
                  24.05.07/aam Invoice.ExtInvID
  Version ......: M15
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE TMSCodeDef NO
&GLOBAL-DEFINE BrTable ClaimHist

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'claimhist'}
{Syst/eventval.i}
{Ar/invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhClaimHist AS HANDLE NO-UNDO.
   lhClaimHist = BUFFER ClaimHist:HANDLE.
   RUN StarEventInitialize(lhClaimHist).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhClaimHist).
   END.

END.


DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO. 
DEF INPUT PARAMETER iiInvNum  AS INT NO-UNDO.   

DEF VAR cu-text  AS c no-undo.

DEF VAR lInvNum     like ClaimHist.InvNum      NO-UNDO.
DEF VAR liCustNum   LIKE ClaimHist.CustNum     NO-UNDO.
DEF VAR lClaimDate   AS Date                   NO-UNDO. 
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 3.
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
DEF VAR lcCustName   AS CHAR                   NO-UNDO.
DEF VAR lcExtInvID   AS CHAR                   NO-UNDO.

form 
    lcExtInvID           FORMAT "X(12)"    COLUMN-LABEL "Invoice"
    ClaimHist.CustNum    format ">>>>>>>9"
    lcCustName format "x(14)" column-label "Name"
    ClaimHist.ClaimDate  format "99-99-9999" column-label "Claim Date"
    ClaimHist.ClaimState FORMAT ">9.9<"
    ClaimHist.ClaimAmt  
    ClaimHist.Handler    format "x(8)"

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  CLAIMING HISTORY  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form /* seek  invoice */
    "Brand .:" lcBrand skip
    "Invoice:" lInvNum
    HELP "Enter invoice number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND INVOICE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  customer */
    "Brand ..:" lcBrand skip
    "Customer:" liCustNum
    HELP "Enter customer number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.


form /* seek  Date */
    "Brand:" lcBrand skip
    "Date :" lClaimDate FORMAT "99-99-9999"
    HELP "Enter claiming date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date "
    COLOR VALUE(cfc) NO-labels overlay FRAME f3.


form
    ClaimHist.memo
    VIEW-AS EDITOR SIZE 60 BY 5 
    with overlay row 8 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc)
    " memo: " + ClaimHist.handler + " " WITH no-labels 1 columns
    frame f4.

/* if only invoice, no need to search by Date */
IF iiInvNum > 0 THEN MaxOrder = 1.
/* if one customer, search only by date */
ELSE IF iiCustNum > 0 THEN 
   ASSIGN MaxOrder = 1
          Order    = 3.

RUN local-find-first.

IF AVAILABLE ClaimHist THEN ASSIGN
   memory       = recid(ClaimHist)
   must-print   = true
   must-add     = false.
ELSE DO:
   BELL.
   MESSAGE "No claiming history available."
   VIEW-AS ALERT-BOX 
   INFORMATION.
   RETURN.      
END.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND ClaimHist WHERE recid(ClaimHist) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ClaimHist THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(ClaimHist).
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
         ufk[1]= 92   ufk[2]= 714  ufk[3]= 28 ufk[4]= 927 
         ufk[5]= 1491 ufk[6]= 4    ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = false.
         
         IF iiInvNum > 0 THEN ASSIGN 
                ufk[1] = 0
                ufk[2] = 0
                ufk[3] = 0.
         ELSE IF iiCustNum > 0 THEN ASSIGN 
                ufk[1] = 0
                ufk[2] = 0.
                
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row lcExtInvID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) lcExtInvID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row ClaimHist.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ClaimHist.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        choose row ClaimHist.ClaimDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ClaimHist.ClaimDate WITH FRAME sel.
      END.

      nap = keylabel(LASTkey).

      IF rtab[FRAME-line] = ? AND
         LOOKUP(nap,"8,f8") = 0 
      THEN NEXT.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        IF iiCustNum = 0 THEN DO:
           order = order + 1. 
           IF order > maxOrder THEN order = 1.
        END.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        IF iiCustNum = 0 THEN DO:
           order = order - 1. IF order = 0 THEN order = maxOrder.
        END. 
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND ClaimHist WHERE recid(ClaimHist) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE ClaimHist THEN
              ASSIGN FIRSTrow = i memory = recid(ClaimHist).
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
           IF NOT AVAILABLE ClaimHist THEN DO:
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
                rtab[1] = recid(ClaimHist)
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
           IF NOT AVAILABLE ClaimHist THEN DO:
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
              rtab[FRAME-down] = recid(ClaimHist).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ClaimHist WHERE recid(ClaimHist) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ClaimHist THEN DO:
           memory = recid(ClaimHist).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE ClaimHist THEN memory = recid(ClaimHist).
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
           FIND ClaimHist WHERE recid(ClaimHist) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lInvNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lInvNum > 0 THEN DO:
            
          IF lcBrand = "*" THEN 
          FIND FIRST ClaimHist WHERE 
             ClaimHist.InvNum >= lInvNum
             NO-LOCK NO-ERROR.
          ELSE    
          FIND FIRST ClaimHist WHERE 
             ClaimHist.Brand = lcBrand AND
             ClaimHist.InvNum >= lInvNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 
     THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              liCustNum WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF liCustNum > 0 THEN DO:
            
          FIND FIRST ClaimHist WHERE 
             ClaimHist.Brand = lcBrand AND
             ClaimHist.CustNum >= liCustNum
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search by col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME F3.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              lClaimDate WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF lClaimDate NE ? THEN DO:
       
          IF iiCustNum > 0 THEN DO:
             FIND FIRST ClaimHist WHERE 
                        ClaimHist.Brand     = lcBrand   AND
                        ClaimHist.CustNum   = iiCustNum AND
                        ClaimHist.ClaimDate = lClaimDate
             use-index CustNum NO-LOCK NO-ERROR.
             IF NOT AVAILABLE ClaimHist THEN 
             FIND LAST ClaimHist WHERE 
                       ClaimHist.Brand      = lcBrand   AND
                       ClaimHist.CustNum    = iiCustNum AND
                       ClaimHist.ClaimDate >= lClaimDate
             use-index CustNum NO-LOCK NO-ERROR.
          END.

          ELSE DO:
             FIND FIRST ClaimHist WHERE 
                        ClaimHist.Brand     = lcBrand AND
                        ClaimHist.ClaimDate = lClaimDate
             use-index ClaimDate NO-LOCK NO-ERROR.
             IF NOT AVAILABLE ClaimHist THEN 
             FIND LAST ClaimHist WHERE 
                       ClaimHist.Brand      = lcBrand AND
                       ClaimHist.ClaimDate >= lClaimDate
             use-index ClaimDate NO-LOCK NO-ERROR.
          END.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */

     /* update memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS on ENDkey undo, NEXT LOOP:
        {Syst/uright2.i}.

        RUN local-find-this(FALSE).
        PAUSE 0.
        DISPLAY ClaimHist.Memo WITH FRAME f4.
        
        ASSIGN ufk = 0
               ufk[1] = 7
               ufk[8] = 8
               ehto   = 0
               ufkey = true.
        
        RUN ufkey.
        
        IF toimi = 1 THEN DO:
        
           ehto = 9. 
           RUN ufkey. 
           run local-find-this(true).

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhClaimHist).

           update ClaimHist.memo WITH FRAME f4. 
    
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhClaimHist).
        
           RELEASE ClaimHist.
        END.
        
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* view invoice */
        RUN local-find-this(true).

        IF AVAILABLE ClaimHist THEN DO:
           ehto = 5.
           RUN ufkey.

           /* show details */
           RUN pInvoiceDetails(ClaimHist.InvNum,
                               TRUE).
        END.
        ufkey = TRUE.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
     
        {Syst/uright2.i}
        delrow = FRAME-LINE.
        RUN local-find-this (FALSE).
                          
        /* is invoice already changed */
        FIND Invoice WHERE Invoice.InvNum = ClaimHist.InvNum NO-LOCK NO-ERROR.
        IF AVAILABLE Invoice AND Invoice.ClaimState >= ClaimHist.ClaimState
        THEN DO:
           MESSAGE "Invoice has" Invoice.ClaimState
                   "claim times marked on it." SKIP
                   "Deletion not allowed."
           VIEW-AS ALERT-BOX.
           delrow = 0.
           NEXT LOOP.
        END.
        
        RUN local-find-NEXT.
        
        IF AVAILABLE ClaimHist THEN Memory = recid(ClaimHist).
        ELSE DO:
           /* read back the record that is TO be  removed */
           RUN local-find-this (FALSE).
                      
           RUN local-find-PREV.
           IF AVAILABLE ClaimHist THEN DO:
              ASSIGN
              delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
              Memory = recid(ClaimHist).
           END.
        END.
     
        /* FIND back the ROW that is TO be removed */
        RUN local-find-this(TRUE).
             
        /* Highlight */
        COLOR DISPLAY VALUE(ctc)
        lcExtInvID
        ClaimHist.ClaimState
        ClaimHist.ClaimDate
        ClaimHist.ClaimAmt
        ClaimHist.Handler.
                              
        ASSIGN ok = FALSE.
        MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY VALUE(ccc)
        lcExtInvID
        ClaimHist.ClaimState
        ClaimHist.ClaimDate
        ClaimHist.ClaimAmt
        ClaimHist.Handler.
        
        IF ok THEN DO:
          
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhClaimHist).
           DELETE ClaimHist.
           
           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ClaimHist THEN DO:
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
        ASSIGN memory = recid(ClaimHist) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ClaimHist) must-print = true.
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
      FIND ClaimHist WHERE recid(ClaimHist) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ClaimHist WHERE recid(ClaimHist) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
    IF iiInvNum > 0 THEN DO:
       FIND FIRST ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.InvNum = iiInvNum use-index InvNum
       NO-LOCK NO-ERROR.
    END.
    ELSE IF iiCustNum > 0 THEN DO:
       FIND FIRST ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.CustNum = iiCustNum use-index CustNum
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND FIRST ClaimHist use-index InvNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ClaimHist use-index CustNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST ClaimHist use-index ClaimDate
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
    END. 

END PROCEDURE.

PROCEDURE local-find-LAST:
    IF iiInvNum > 0 THEN DO:
       FIND LAST  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.InvNum = iiInvNum use-index InvNum
       NO-LOCK NO-ERROR.
    END.
    ELSE IF iiCustNum > 0 THEN DO:
       FIND LAST  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.CustNum = iiCustNum use-index CustNum
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND LAST ClaimHist use-index InvNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ClaimHist use-index CustNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST ClaimHist use-index ClaimDate
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
    END. 


END PROCEDURE.

PROCEDURE local-find-NEXT:
    IF iiInvNum > 0 THEN DO:
       FIND NEXT  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.InvNum = iiInvNum use-index InvNum
       NO-LOCK NO-ERROR.
    END.
    ELSE IF iiCustNum > 0 THEN DO:
       FIND NEXT  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.CustNum = iiCustNum use-index CustNum
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND NEXT ClaimHist use-index InvNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ClaimHist use-index CustNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT ClaimHist use-index ClaimDate
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
    END. 


END PROCEDURE.

PROCEDURE local-find-PREV:
    IF iiInvNum > 0 THEN DO:
       FIND PREV  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.InvNum = iiInvNum use-index InvNum
       NO-LOCK NO-ERROR.
    END.
    ELSE IF iiCustNum > 0 THEN DO:
       FIND PREV  ClaimHist WHERE 
                  ClaimHist.Brand  = lcBrand AND
                  ClaimHist.CustNum = iiCustNum use-index CustNum
       NO-LOCK NO-ERROR.
    END.
    ELSE DO:
       IF order = 1 THEN FIND PREV ClaimHist use-index InvNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ClaimHist use-index CustNum
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV ClaimHist use-index ClaimDate
          WHERE ClaimHist.Brand = lcBrand NO-LOCK NO-ERROR.
    END. 

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
          lcExtInvID
          ClaimHist.CustNum 
          lcCustName 
          ClaimHist.ClaimState
          ClaimHist.ClaimDate
          ClaimHist.ClaimAmt
          ClaimHist.Handler
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.
    FIND Customer OF ClaimHist NO-LOCK NO-ERROR.

    ASSIGN 
       lcCustName = ""
       lcExtInvID = "".
       
    IF AVAILABLE Customer 
    THEN lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                       BUFFER Customer).

    FIND Invoice WHERE Invoice.InvNum = ClaimHist.InvNum NO-LOCK NO-ERROR.
    IF AVAILABLE Invoice THEN lcExtInvID = Invoice.ExtInvID.
    
END PROCEDURE.

