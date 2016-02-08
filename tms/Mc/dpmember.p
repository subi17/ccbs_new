/* ----------------------------------------------------------------------
  MODULE .......: DPMember.p
  TASK .........: UPDATEs table DPMember
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.04.11
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'DPMember'}
{date.i}
{dpmember.i}
{tmsconst.i}

{eventval.i}

IF llDoEvent THEN DO:
   
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   lhDPMember = BUFFER DPMember:HANDLE.
   RUN StarEventInitialize(lhDPMember).
   
   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhDPMember).
   END.

END.

DEF INPUT PARAMETER iiDPID      AS INT  NO-UNDO.
DEF INPUT PARAMETER icHostTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER icKeyValue  AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 5.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcMember       AS CHAR NO-UNDO.
DEF VAR lcHostTable    AS CHAR NO-UNDO.
DEF VAR lcKeyValue     AS CHAR NO-UNDO. 
DEF VAR lcDPRuleID     AS CHAR NO-UNDO.
DEF VAR lcDiscountUnit AS CHAR NO-UNDO.
DEF VAR lcUnit         AS CHAR NO-UNDO.

IF icHostTable EQ "MobSub" AND
   NOT CAN-FIND(FIRST MobSub NO-LOCK WHERE
                      MobSub.MsSeq = INT(icKeyValue)) THEN lcRight = "R".

FORM
    DPMember.HostTable FORMAT "X(10)"
    DPMember.KeyValue  FORMAT "X(10)"
    lcMember           FORMAT "X(15)" COLUMN-LABEL "Member"
    lcDPRuleID         FORMAT "X(16)" COLUMN-LABEL "Plan ID"
    DPMember.DiscValue FORMAT "->>>>>9.99" COLUMN-LABEL "Discount"
    lcUnit             FORMAT "X(3)"  COLUMN-LABEL "Un."
    DPMember.ValidTo   FORMAT "99-99-99"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " DISCOUNT PLAN MEMBERS "
    FRAME sel.

FORM
    SKIP
    DPMember.HostTable   COLON 18 FORMAT "X(16)"
       lcHostTable NO-LABEL FORMAT "X(30)" SKIP
    DPMember.KeyValue    COLON 18 FORMAT "X(12)"
       lcMember NO-LABEL FORMAT "X(35)" SKIP
    DPMember.OrderId  COLON 18
      LABEL "Order ID"
    lcDPRuleID  COLON 18 
       FORMAT "X(16)" 
       LABEL "Discount Plan" 
       HELP "Discount plan ID"
    DPMember.DPId       COLON 18
       DiscountPlan.DPName NO-LABEL FORMAT "X(35)" SKIP
    DPMember.DiscValue  COLON 18 FORMAT "->>>>>9.99"    
    lcDiscountUnit COLON 18 
       LABEL "Discount Unit" 
       FORMAT "X(12)"
    DPMember.ValidFrom  COLON 18 
    DPMember.ValidTo    COLON 18
       SKIP(1)
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Table:" lcHostTable FORMAT "X(20)" 
       HELP "Enter table name" SKIP
    "Key Value:" lcKeyValue FORMAT "X(20)"
       HELP "Enter key value"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Member "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fDispHostTable RETURNS LOGIC
   (icHostTable AS INT):

   lcHostTable = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "DPMember",
                                  "HostTable",
                                  STRING(icHostTable)).
                                  
   DISP lcHostTable WITH FRAME lis.
       
END FUNCTION.


cfc = "sel". RUN ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE DPMember THEN ASSIGN
   Memory       = recid(DPMember)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No members available!" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a DPMember  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           CREATE DPMember.
           ASSIGN 
              DPMember.DPID       = iiDPID
              DPMember.HostTable  = icHostTable
              DPMember.KeyValue   = icKeyValue
              DPMember.ValidFrom  = TODAY
              DPMember.ValidTo    = 12/31/2049.

           IF iiDPId > 0 THEN DO:
              FOR FIRST DPRate NO-LOCK WHERE
                        DPRate.DPId = iiDPId AND
                        DPRate.ValidTo >= TODAY AND
                        DPRate.ValidFrom <= TODAY:
                DpMember.DiscValue = DPRate.DiscValue.
              END.

              FIND FIRST DiscountPlan WHERE DiscountPlan.DPID = iiDPID 
                 NO-LOCK.
              IF DiscountPlan.ValidPeriods > 0 THEN 
                 DPMember.ValidTo = 
                    fCalcDPMemberValidTo(DPMember.ValidFrom,
                                         DiscountPlan.ValidPeriods).
           END.
             
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              DPMember.DPId = 0 OR DPMember.DiscValue = 0
           THEN UNDO add-row, LEAVE add-row.

           /* dpmember creations not logged anymore YDR-1078 */
           /*
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDPMember).
           */

           ASSIGN
           Memory = recid(DPMember)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DPMember THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DPMember WHERE recid(DPMember) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DPMember THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DPMember).
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
        ufk[1]= 1852 WHEN iiDPId > 0
        ufk[5]= (IF lcRight = "RW" AND iiDPId = 0 THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DPMember.KeyValue {uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DPMember.KeyValue WITH FRAME sel.
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
        FIND DPMember WHERE recid(DPMember) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DPMember THEN
              ASSIGN FIRSTrow = i Memory = recid(DPMember).
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
           IF NOT AVAILABLE DPMember THEN DO:
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
                rtab[1] = recid(DPMember)
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
           IF NOT AVAILABLE DPMember THEN DO:
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
              rtab[FRAME-DOWN] = recid(DPMember).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DPMember WHERE recid(DPMember) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DPMember THEN DO:
           Memory = recid(DPMember).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DPMember THEN Memory = recid(DPMember).
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
           FIND DPMember WHERE recid(DPMember) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.p.
       ehto = 9. RUN ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       lcHostTable = "MobSub".
       UPDATE lcHostTable lcKeyValue WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcHostTable > "" AND lcKeyValue > "" THEN DO:
          FIND FIRST DPMember WHERE 
                     DPMember.DPID = iiDPId  AND
                     DPMember.HostTable = lcHostTable AND
                     DPMember.KeyValue >= lcKeyValue
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE DPMember THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          DPMember.HostTable
          DPMember.KeyValue
          DPMember.DiscValue
          DPMember.ValidTo.

       RUN local-find-NEXT.
       IF AVAILABLE DPMember THEN Memory = recid(DPMember).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DPMember THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DPMember).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DPMember.HostTable
          DPMember.KeyValue
          DPMember.DiscValue
          DPMember.ValidTo.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPMember).

           DELETE DPMember.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DPMember THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.p.
       cfc = "lis". RUN ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(DPMember).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DPMember) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DPMember) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND DPMember WHERE recid(DPMember) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DPMember WHERE recid(DPMember) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiDPId > 0 THEN 
     FIND FIRST DPMember WHERE 
         DPMember.DPID = iiDPId NO-LOCK NO-ERROR.
   
   ELSE DO:
     IF order = 1 THEN FIND FIRST DPMember WHERE 
         DPMember.HostTable = icHostTable AND
         DPMember.KeyValue = icKeyValue NO-LOCK NO-ERROR.
   END.      
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiDPId > 0 THEN 
     FIND LAST DPMember WHERE 
         DPMember.DPID = iiDPId NO-LOCK NO-ERROR.
   
   ELSE DO:
      IF order = 1 THEN FIND LAST DPMember WHERE 
         DPMember.HostTable = icHostTable AND
         DPMember.KeyValue = icKeyValue NO-LOCK NO-ERROR.
   END.       
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiDPId > 0 THEN 
     FIND NEXT DPMember WHERE 
         DPMember.DPID = iiDPId NO-LOCK NO-ERROR.
   
   ELSE DO:
      IF order = 1 THEN FIND NEXT DPMember WHERE 
         DPMember.HostTable = icHostTable AND
         DPMember.KeyValue = icKeyValue NO-LOCK NO-ERROR.
   END.      
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiDPId > 0 THEN 
     FIND PREV DPMember WHERE 
         DPMember.DPID = iiDPId NO-LOCK NO-ERROR.
   
   ELSE DO:
      IF order = 1 THEN FIND PREV DPMember WHERE 
         DPMember.HostTable = icHostTable AND
         DPMember.KeyValue = icKeyValue NO-LOCK NO-ERROR.
   END.      
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DPMember.HostTable
       DPMember.KeyValue
       lcMember
       lcDPRuleID
       DPMember.DiscValue
       lcUnit
       DPMember.ValidTo
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR liKeyValue AS INT NO-UNDO.

   ASSIGN
      lcMember = ""
      lcDPRuleID = ""
      liKeyValue = INT(DPMember.KeyValue) NO-ERROR.

   FIND FIRST DiscountPlan WHERE 
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPId = DPMember.DPId NO-LOCK NO-ERROR.
   IF AVAILABLE DiscountPlan THEN ASSIGN
      lcDPRuleID = DiscountPlan.DPRuleID
      lcDiscountUnit = DiscountPlan.DPUnit
      lcUnit = IF DiscountPlan.DPUnit = "Percentage" 
               THEN "%"
               ELSE "EUR". 
   
   IF liKeyValue = 0 THEN RETURN.
        
   CASE DPMember.HostTable:
   WHEN "Customer" THEN DO:
      FIND FIRST Customer WHERE Customer.CustNum = liKeyValue NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN 
         lcMember = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                     BUFFER Customer).
   END.
   WHEN "MobSub" THEN DO:
      FIND FIRST MobSub WHERE MobSub.MsSeq = liKeyValue NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MobSub THEN DO:
         FIND FIRST MsOwner WHERE MsOwner.MsSeq = liKeyValue NO-LOCK NO-ERROR.
         IF AVAILABLE MsOwner THEN lcMember = MsOwner.CLI.
      END.
      ELSE lcMember = MobSub.CLI.

      /* Discount at Order level and order is not completed yet */
      IF lcMember = "" THEN DO:
         FIND FIRST Order WHERE
                    Order.MsSeq = liKeyValue NO-LOCK NO-ERROR.
         IF AVAIL Order THEN lcMember = Order.CLI.
      END. /* IF lcMember = "" THEN DO: */
   END.
   END CASE.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bMember FOR DPMember.
   
   DEF VAR ldaValidTo AS DATE NO-UNDO.
   DEF VAR lcCLIType  AS CHAR NO-UNDO. 

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         DPMember.HostTable
         DPMember.KeyValue
         lcMember
         DPMember.OrderId
         lcDPRuleID 
         DPMember.DPId        
         DiscountPlan.DPName WHEN AVAILABLE DiscountPlan
         DPMember.DiscValue
         lcDiscountUnit
         DPMember.ValidFrom
         DPMember.ValidTo        
      WITH FRAME lis.

      IF NOT NEW DPMember THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.p.
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateMember:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         ehto = 9.
         RUN ufkey.p.
         
         PROMPT
            lcDPRuleID WHEN NEW DPMember AND iiDPId = 0
            DPMember.HostTable WHEN iiDPId > 0
            DPMember.KeyValue WHEN iiDPId > 0
            DPMember.DiscValue WHEN NEW DPMember
            DPMember.ValidFrom
            DPMember.ValidTo        
         WITH FRAME lis EDITING:
 
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "lcDPRuleID" THEN DO:
                  IF INPUT lcDPRuleID = "" THEN UNDO, LEAVE UpdateMember.
                  
                  FIND FIRST DiscountPlan WHERE 
                     DiscountPlan.Brand = gcBrand AND
                     DiscountPlan.DPRuleId = INPUT lcDPRuleID NO-LOCK NO-ERROR.
  
                  IF NOT AVAILABLE DiscountPlan THEN DO:
                     MESSAGE "Unknown discount plan"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  DISP DiscountPlan.DPId @ DPMember.DPId.
                  
                  lcDiscountUnit = DiscountPlan.DPUnit.

                  FIND FIRST DPRate NO-LOCK WHERE
                            DPRate.DPId = DiscountPlan.DPId AND
                            DPRate.ValidTo >= TODAY AND
                            DPRate.ValidFrom <= TODAY NO-ERROR.
                   IF AVAIL DPRate THEN
                     DISP DPRate.DiscValue @ DpMember.DiscValue.

                  IF DiscountPlan.ValidPeriods > 0 THEN 
                        DISP 
                           fCalcDPMemberValidTo(INPUT INPUT DPMember.ValidFrom,
                                                DiscountPlan.ValidPeriods) @ 
                                                DPMember.ValidTo.
                  
                  DISPLAY DiscountPlan.DPName 
                          lcDiscountUnit WITH FRAME lis.
               END.

               ELSE IF FRAME-FIELD = "ValidFrom" THEN DO:
                  IF DPMember.ValidFrom ENTERED AND 
                     AVAILABLE DiscountPlan AND
                     DiscountPlan.ValidPeriods > 0 THEN DO:
                        DISP 
                           fCalcDPMemberValidTo(INPUT INPUT DPMember.ValidFrom,
                                                DiscountPlan.ValidPeriods)
                                               @ DPMember.ValidTo.
                  END.
               END.

            END.
            
            APPLY LASTKEY.
         END.

         IF AVAILABLE DiscountPlan THEN DO:
            IF INPUT DPMember.ValidFrom < DiscountPlan.ValidFrom OR
               INPUT DPMember.ValidFrom > DiscountPlan.ValidTo 
            THEN DO:
               MESSAGE "Plan is not valid on given period begin date"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.
 
            IF DiscountPlan.Subject = "Contract Target" AND 
               DiscountPlan.SubjectType = "List"
            THEN DO:
               lcCLIType = "".
               FIND FIRST MobSub WHERE MobSub.MsSeq = INT(INPUT DPMember.KeyValue) 
                  NO-LOCK NO-ERROR.
               IF NOT AVAILABLE MobSub THEN DO:
                  FIND FIRST MsOwner WHERE 
                     MsOwner.MsSeq = INT(INPUT DPMember.KeyValue) NO-LOCK NO-ERROR.
                  IF AVAILABLE MsOwner THEN lcCLIType = MsOwner.CLIType.
               END.
               ELSE lcCLIType = MobSub.CLIType.

               IF NOT CAN-FIND(FIRST DPSubject WHERE 
                   DPSubject.DPId      = DiscountPlan.DPId AND
                   DPSubject.DPSubject = lcCliType AND
                   DPSubject.ValidFrom <= INPUT DPMember.ValidTo AND
                   DPSubject.ValidTo   >= INPUT DPMember.ValidFrom) 
               THEN DO:
                  MESSAGE "Subscription type is not valid for discount plan"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.   
            END.
         END.
       
         IF CAN-FIND(FIRST bMember WHERE
                           bMember.DPId = INPUT DPMember.DPId AND
                           bMember.HostTable  = INPUT DPMember.HostTable  AND
                           bMember.KeyValue   = INPUT DPMember.KeyValue   AND
                           bMember.ValidTo   >= INPUT DPMember.ValidFrom  AND
                           bMember.ValidFrom <= INPUT DPMember.ValidTo    AND
                           RECID(bMember)  NE RECID(DPMember))
         THEN DO:
            MESSAGE "A similar row already exists for given period"
            VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT UpdateMember.
         END.

         IF AVAILABLE DiscountPlan AND
            DiscountPlan.ValidPeriods > 0 THEN DO:
            
            ldaValidTo = fCalcDPMemberValidTo(INPUT INPUT DPMember.ValidFrom,
                                              DiscountPlan.ValidPeriods).
            IF INPUT DPMember.ValidTo > ldaValidTo THEN DO:
               Ok = FALSE. 
               MESSAGE 
                  "Discount plan has a limit for" DiscountPlan.ValidPeriods
                  "periods. Do You want to override that limit?"
               VIEW-AS ALERT-BOX QUESTION
               BUTTONS YES-NO
               SET Ok.
               IF NOT Ok THEN DO:
                  DISP ldaValidTo @ DPMember.ValidTo.
                  NEXT.
               END.   
            END.
         END.
      
         IF NOT NEW DPMember THEN
         FIND CURRENT DpMember NO-LOCK.
         
         IF CURRENT-CHANGED DpMember THEN DO:
            
            MESSAGE ({&MSG_RECORD_CHANGED})
            VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
               
            RETURN.
         END. 
         ELSE DO: 
         
            IF NOT NEW DpMember THEN DO:
               FIND CURRENT DpMember EXCLUSIVE-LOCK.
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPMember). 
            END.
         
            ASSIGN
               DpMember.DPId WHEN NEW DPMember AND iiDPId = 0
               DPMember.HostTable WHEN iiDPId > 0
               DPMember.KeyValue WHEN iiDPId > 0
               DPMember.DiscValue WHEN NEW DPMember
               DPMember.ValidFrom
               DPMember.ValidTo.
            
         END.

         IF NOT NEW DPMember THEN DO:
            FIND CURRENT DPMember NO-LOCK.
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPMember). 
         END.
         
         LEAVE UpdateMember.
      END.

      IF NEW DPMember THEN LEAVE.   
   END.
   
END PROCEDURE.

