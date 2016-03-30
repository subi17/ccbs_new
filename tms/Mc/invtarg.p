/* ----------------------------------------------------------------------
  MODULE .......: BillTarget.P
  TASK .........: UPDATE Billing Targets
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 14-06-99
  CHANGED ......: 07.07.99 pt  DirectDebit
                  16.09.99 pt  VALIDATE FRAME lis BillTarget.InvCust.
                  28.09.99 jpo UPDATE FIELD CreditLimit
                  10.09.02 lp  F3 -> A-numbers for this Target
                  16.09.02/aam RatePlan and DiscPlan,
                               check if CLIs or MobSubs exist before deleting,
                               eventlog,
                               invtype removed
                  10.10.02/jr  extent3 addresses => address,postoffice,country
                  10.10.02/jr  Fixed validations              
                  11.10.02/aam check pricelists' currencies against customer
                  15.10.02/aam InvCust, CreditLimit and DirectDebit removed
                  28.02.03/tk  tokens
                  24.03.03/aam country not mandatory
                  15.09.03/aam brand
                  18.11.03/aam show discounts (dpconf, F3)
                  01.12.03/aam check that chosen rateplan has valid pricelists
                  14.12.05/aam BTName removed
                  24.01.06/jt  DYNAMIC-FUNCTION("fDispCustName"
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'BillTarget'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBillTarget AS HANDLE NO-UNDO.
   lhBillTarget = BUFFER BillTarget:HANDLE.
   RUN StarEventInitialize(lhBillTarget).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhBillTarget).
   END.

END.

DEF INPUT PARAMETER CustNum LIKE  Customer.CustNum NO-UNDO.
DEF /* NEW */ shared VAR siirto AS CHAR.

DEF BUFFER inv-cust FOR  Customer.

DEF VAR BillTarget  LIKE BillTarget.BillTarget  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR def-pp-code  AS c                      NO-UNDO.
DEF VAR def-dp-code  AS c                      NO-UNDO.
DEF VAR coname       AS CHAR                   NO-UNDO.
DEF VAR llCurr       AS CHAR                   NO-UNDO. 
DEF VAR llRight      AS LOGIC                  NO-UNDO. 
DEF VAR llValid      AS LOGIC                  NO-UNDO. 
DEF VAR lcCustName   AS CHAR                   NO-UNDO.

form
    BillTarget.BillTarget      /* COLUMN-LABEL FORMAT */
    BillTarget.RatePlan 
    BillTarget.DiscPlan 
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Billing Targets Of Cust " + string(CustNum) + 
    ": " + lcCustName + " "
    FRAME sel.

form
    BillTarget.BillTarget label "No.  of BillT."  SKIP
    BillTarget.RatePlan   LABEL "Rating Plan ."   
       RatePlan.RPName NO-LABEL AT 29 SKIP
    BillTarget.DiscPlan   LABEL "Discount Plan"
       DiscPlan.DPName NO-LABEL AT 29 SKIP
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    side-labels 
    FRAME lis.

form /* seek Invoicing Target  BY  BillTarget */
    BillTarget
    help "Enter Billing Target"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Target NO. "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FIND  Customer where Customer.CustNum = CustNum no-lock.
lcCustName =  DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                        BUFFER Customer).
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By No. ,By name,By 3, By 4".


FIND FIRST BillTarget where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE BillTarget THEN DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing targets available !" VIEW-AS ALERT-BOX.
      hide frame sel no-pause.
      RETURN.
   END.
   ELSE ASSIGN
      memory = ?
      must-print = FALSE
      must-add = FALSE.
END.    

ELSE ASSIGN 
   memory = recid(BillTarget)
   must-print   = TRUE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a BillTarget  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR BillTarget.BillTarget.
           IF INPUT FRAME  LIS BillTarget.BillTarget = 0 THEN LEAVE add-row.

           FIND BillTarget where BillTarget.CustNum = CustNum AND
                BillTarget.BillTarget = INPUT FRAME  lis BillTarget.BillTarget
           no-lock no-error.

           IF AVAIL BillTarget THEN DO:     
              BELL.
              MESSAGE
              "Billing Target " + INPUT FRAME lis BillTarget.BillTarget +
              " already exists on Customer " + string(CustNum) + " !".
              NEXT.
           END.

           CREATE BillTarget.
           ASSIGN
           BillTarget.BillTarget = INPUT FRAME lis BillTarget.BillTarget
           BillTarget.CustNum    = CustNum.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBillTarget).

           ASSIGN
           memory = recid(BillTarget)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST BillTarget
      where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BillTarget THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :

      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND BillTarget WHERE recid(BillTarget) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE BillTarget THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(BillTarget).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 235 ufk[4]= 876
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRIght = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW BillTarget.BillTarget {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) BillTarget.BillTarget WITH FRAME sel.
      END.

      nap = keylabel(LASTkey).

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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND BillTarget WHERE recid(BillTarget) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE BillTarget THEN
              ASSIGN FIRSTrow = i memory = recid(BillTarget).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE BillTarget THEN DO:
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
                rtab[1] = recid(BillTarget)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE BillTarget THEN DO:
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
              rtab[FRAME-DOWN] = recid(BillTarget).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND BillTarget WHERE recid(BillTarget) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE BillTarget THEN DO:
           memory = recid(BillTarget).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE BillTarget THEN memory = recid(BillTarget).
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
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND BillTarget WHERE recid(BillTarget) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       SET BillTarget WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BillTarget ENTERED THEN DO:
          FIND FIRST BillTarget WHERE 
                     BillTarget.BillTarget >= BillTarget  AND
                     BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
          IF NOT AVAILABLE BillTarget THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 no-MESSAGE.
             NEXT BROWSE.
          END.
          /* some invtarg/it-no was found */
          ASSIGN order = 1 memory = recid(BillTarget) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF lookup(nap,"3,f3") > 0 THEN DO: /* discounts */
        {Syst/uright2.i}
        FIND BillTarget WHERE recid(BillTarget) = rtab[FRAME-LINE] no-lock.
        IF BillTarget.DiscPlan = "" THEN DO:
           MESSAGE "Discount plan has not been selected."
           VIEW-AS ALERT-BOX
           INFORMATION.
        END.
        ELSE DO:
           ASSIGN ufkey = TRUE.
           RUN Mc/dpconf (BillTarget.DiscPlan).
           NEXT LOOP.
        END.
     END.

     /* price list configuration  */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:

        FIND BillTarget WHERE recid(BillTarget) = rtab[FRAME-LINE] 
           NO-LOCK NO-ERROR.

        IF AVAILABLE BillTarget AND BillTarget.RatePlan NE "" THEN DO:
            /* set update-mode as prohibited */
            ASSIGN llRight = qupd
                   qupd    = FALSE.
            RUN Mc/plistconf(BillTarget.RatePlan).
            qupd = llRight.
        END.
        ELSE MESSAGE "No rating information is available."
             VIEW-AS ALERT-BOX ERROR.

        ufkey = true.
        NEXT loop.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       BillTarget.BillTarget BillTarget.RatePlan BillTarget.DiscPlan .

       RUN local-find-NEXT.
       IF AVAILABLE BillTarget THEN memory = recid(BillTarget).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE BillTarget THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(BillTarget).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       IF CAN-FIND(FIRST CLI WHERE 
                         CLI.CustNum    = BillTarget.CustNum AND
                         CLI.BillTarget = BillTarget.BillTarget) OR
          CAN-FIND(FIRST MobSub WHERE 
                         MobSub.CustNum    = BillTarget.CustNum AND
                         MobSub.BillTarget = BillTarget.BillTarget)
       THEN DO:
          MESSAGE "This billing target has subscriptions attached to it."
                  "Deletion is not allowed."
          VIEW-AS ALERT-BOX
          ERROR.
          NEXT.
       END. 


       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       BillTarget.BillTarget BillTarget.RatePlan BillTarget.DiscPlan.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBillTarget).

           DELETE BillTarget.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST BillTarget
           where BillTarget.CustNum = CustNum) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY BillTarget.BillTarget.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBillTarget).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBillTarget).

       RUN local-disp-row.
       xrecid = recid(BillTarget).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(BillTarget) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(BillTarget) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND BillTarget WHERE recid(BillTarget) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND BillTarget WHERE recid(BillTarget) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST BillTarget
       where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST BillTarget
       where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT BillTarget
       where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev BillTarget
       where BillTarget.CustNum = CustNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       BillTarget.BillTarget
       BillTarget.RatePlan
       BillTarget.DiscPlan
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.

      DO WITH FRAME lis:

         FIND RatePlan WHERE 
              RatePlan.Brand    = Customer.Brand AND
              RatePlan.RatePlan = BillTarget.RatePlan 
         NO-LOCK NO-ERROR.

         IF AVAILABLE RatePlan THEN DISPLAY RatePlan.RPName.
         ELSE DISPLAY "" ;& RatePlan.RPName.

         FIND DiscPlan WHERE 
              DiscPlan.Brand    = Customer.Brand AND
              DiscPlan.DiscPlan = BillTarget.DiscPlan 
          NO-LOCK NO-ERROR.
         IF AVAILABLE DiscPlan THEN DISPLAY DiscPlan.DPName.
         ELSE DISPLAY "" ;& DiscPlan.DPName.

      END. 

      DISPLAY
          BillTarget.RatePlan
          BillTarget.DiscPlan
      WITH FRAME lis.

      IF lcRight = "RW" THEN DO:
         UPDATE
             BillTarget.RatePlan
             BillTarget.DiscPlan
         WITH FRAME lis EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "RatePlan" THEN DO.
                   /* mandatory */
                   FIND RatePlan WHERE 
                        RatePlan.Brand    = Customer.Brand AND
                        RatePlan.RatePlan = INPUT FRAME lis BillTarget.RatePlan
                   NO-LOCK NO-ERROR.

                   IF NOT AVAILABLE RatePlan THEN DO:
                      BELL. 
                      MESSAGE "Unknown rating plan.".
                      NEXT.
                   END.

                   /* check pricelists;
                      - should have same currency as chosen for customer 
                      - atleast one should be currently valid
                   */
                   ASSIGN llCurr  = Customer.Currency
                          llValid = FALSE.
                          
                   FOR EACH PListConf OF RatePlan NO-LOCK,
                      FIRST PriceList OF PListConf NO-LOCK:
                      
                      IF PListConf.DFrom <= TODAY AND
                         PListConf.DTo   >= TODAY
                      THEN llValid = TRUE.
                      
                      IF PriceList.Currency NE llCurr THEN DO:
                         llCurr = PriceList.Currency.
                         LEAVE.
                      END.
                   END.

                   IF llCurr NE Customer.Currency THEN DO:
                      BELL.
                      MESSAGE "Customer's currency (" Customer.Currency
                              ") is different than chosen rating plan's"
                              "pricelists have (" llCurr ")".
                      NEXT. 
                   END. 
                                             
                   IF NOT llValid THEN DO:
                      BELL.
                      MESSAGE "Rateplan hasn't got any currently valid"
                              "pricelist definitions.".
                      NEXT.
                   END.

                   DISPLAY RatePlan.RPName. 
                END.

                ELSE IF FRAME-FIELD = "DiscPlan" THEN DO.
                   /* not mandatory */
                   IF INPUT FRAME lis BillTarget.DiscPlan = "" THEN 
                      DISPLAY "" ;& DiscPlan.DPName. 

                   ELSE DO:

                      FIND DiscPlan WHERE 
                         DiscPlan.Brand    = Customer.Brand AND
                         DiscPlan.DiscPlan = INPUT FRAME lis BillTarget.DiscPlan
                      NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE DiscPlan THEN DO:
                         BELL. 
                         MESSAGE "Unknown discount plan.".
                         NEXT.
                      END.
                      DISPLAY DiscPlan.DPName. 
                   END. 
                END.

             END.

             APPLY LASTKEY.
         END. /* EDITING */

      END.   
      ELSE PAUSE.

      LEAVE.

   END.
END PROCEDURE.

