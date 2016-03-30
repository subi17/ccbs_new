/* ----------------------------------------------------------------------
  MODULE .......: conlist
  TASK .........: Calling list of reminded/claimed customers
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 29.04.04
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Contact'}
{Func/timestamp.i}
{Syst/eventval.i}
{Func/cparam2.i}
{Func/finvbal.i}

DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO.
DEF INPUT PARAMETER idtConDate AS DATE NO-UNDO. 
DEF INPUT PARAMETER iiCustNum  AS INT  NO-UNDO. 

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhContact AS HANDLE NO-UNDO.
   lhContact = BUFFER Contact:HANDLE.
   RUN StarEventInitialize(lhContact).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhContact).
   END.

END.


DEF NEW shared VAR siirto AS CHAR.

DEF VAR liCustNum   AS INT NO-UNDO.
DEF VAR ldAmt       AS DEC NO-UNDO. 

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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

DEF VAR lcBankAcc    AS CHAR                   NO-UNDO.
DEF VAR ldtDate      AS DATE                   NO-UNDO.
DEF VAR ldTotal      AS DEC                    NO-UNDO. 
DEF VAR liQty        AS INT                    NO-UNDO. 
DEF VAR lcError      AS CHAR                   NO-UNDO. 
DEF VAR liState      AS INT                    NO-UNDO. 
DEF VAR ldtStampDate AS DATE                   NO-UNDO.
DEF VAR liStampTime  AS INT                    NO-UNDO.
DEF VAR lcDispStamp  AS CHAR                   NO-UNDO. 
DEF VAR llHandled    AS LOG                    NO-UNDO. 
DEF VAR lcTypeDenied AS CHAR                   NO-UNDO. 
DEF VAR lcPhone      AS CHAR                   NO-UNDO.

 
form
    Contact.ConDate    
    Contact.CustNum    FORMAT ">>>>>>>9"      COLUMN-LABEL "Customer"
    Customer.CustName  FORMAT "X(26)"         COLUMN-LABEL "Name"
    Contact.CustBal    FORMAT "->,>>>,>>9.99" COLUMN-LABEL "Open Balance"
    lcPhone            FORMAT "x(16)"         COLUMN-LABEL "Phone"
    llHandled          FORMAT "*/"            COLUMN-LABEL "H"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " CONTACT LIST "  + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    ldTotal   AT 2 
       LABEL "Total Balance"    
       FORMAT "->,>>>,>>9.99"
       
    liQty AT 45
       LABEL "Customer Qty "
       FORMAT ">,>>>,>>9"
       SKIP
WITH  OVERLAY ROW 17 WIDTH 80
    COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) " UNHANDLED CUSTOMERS "
    SIDE-LABELS FRAME fTotal.

form
    Contact.PlanDate COLON 20 SKIP
    lcDispStamp      COLON 20 FORMAT "X(20)" LABEL "Contact Taken" SKIP
    Contact.CustNum  COLON 20
       Customer.CustName NO-LABEL SKIP
    Contact.UserCode COLON 20
       TMSUser.UserName NO-LABEL SKIP
    Contact.CustBal  COLON 20 SKIP
    Contact.ConState COLON 20 
WITH  OVERLAY ROW 5 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


form /* seek  Contact */
    "Customer:" liCustNum
    HELP "Enter customer nbr"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  Contact */
    "Amount:" ldAmt FORMAT "->,>>>,>>9.99"
    HELP "Enter amount"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Amount "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fDispTotal RETURNS LOGICAL.

   IF iiCustNum > 0 THEN RETURN FALSE.
   
   ASSIGN ldTotal = 0
          liQty   = 0.
          
   FOR EACH Contact WHERE 
            Contact.Brand    = gcBrand    AND
            Contact.UserCode = icUserCode AND
            Contact.ConState = liState    AND
            Contact.ConDate  = idtConDate:
            
      ASSIGN ldTotal = ldTotal + Contact.CustBal
             liQty   = liQty + 1.
   END.
   
   PAUSE 0.
   DISPLAY ldTotal liQty WITH FRAME fTotal.
    
END FUNCTION.

IF iiCustNum > 0 THEN FrmDown = 15. 

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

ASSIGN orders       = "By Customer ," +
                      "By Amount    "
       liState      = 0
       lcTypeDenied = fCParamC("InvTypeDenied").

HIDE MESSAGE NO-PAUSE.

fDispTotal().

RUN local-find-first.

IF AVAILABLE Contact THEN ASSIGN
   Memory       = recid(Contact)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
   END.

   IF must-add THEN DO:  /* Add a Contact  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE ADD-ROW:
                        
           IF icUserCode > "" THEN DO WITH FRAME lis:
              FIND TMSUser WHERE TMSUser.UserCode = icUserCode 
              NO-LOCK NO-ERROR.
              IF AVAILABLE TMSUser 
              THEN DISPLAY icUserCode @ Contact.UserCode
                           TMSUser.UserName.
           END.
           IF icUserCode = "" OR NOT AVAILABLE TMSUser
           THEN DISPLAY katun @ Contact.UserCode.

           IF iiCustNum > 0 THEN DO WITH FRAME lis: 
               DISPLAY iiCustNum @ Contact.CustNum. 
               FIND Customer WHERE 
                    Customer.Brand   = gcBrand AND
                    Customer.CustNum = iiCustNum 
               NO-LOCK NO-ERROR.
               IF AVAILABLE Customer THEN DISPLAY Customer.CustName.
           END. 
           
           IF idtCondate NE ? 
           THEN DISPLAY idtConDate @ Contact.PlanDate.
           
           PROMPT-FOR Contact.PlanDate
                      Contact.CustNum  WHEN iiCustNum = 0
                      Contact.UserCode WHEN icUserCode = ""
           WITH FRAME lis EDITING:
              
              READKEY.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.

                 IF FRAME-FIELD = "CustNum" THEN DO:
                    IF INPUT Contact.CustNum NE 0 THEN DO:
                       FIND Customer WHERE 
                            Customer.Brand   = gcBrand AND
                            Customer.CustNum = INPUT Contact.CustNum 
                       NO-LOCK NO-ERROR.
                       IF NOT AVAILABLE Customer THEN DO:
                          BELL.
                          MESSAGE "Unknown customer".
                          NEXT.
                       END.
                       DISPLAY Customer.CustName WITH FRAME lis.
                    END.   
                 END.
                    
                 ELSE IF FRAME-FIELD = "UserCode" THEN DO:
                    FIND TMSUser WHERE
                         TMSUser.UserCode = INPUT Contact.UserCode
                    NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE TMSUser THEN DO:
                       BELL.
                       MESSAGE "Unknown user".
                       NEXT.
                    END.
                    DISPLAY TMSUser.UserName WITH FRAME lis.
                 END.
                 
              END. 
              
              APPLY LASTKEY.
              
           END.

           IF INPUT FRAME lis Contact.PlanDate = ? OR
              INPUT FRAME lis Contact.CustNum = 0
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST Contact WHERE
                             Contact.Brand    = gcBrand                AND
                             Contact.CustNum  = INPUT Contact.CustNum  AND
                             Contact.ConDate  = INPUT Contact.PlanDate AND
                             Contact.ConState = 0)
           THEN DO:
              MESSAGE "There is already an unhandled contact for customer"
                      INPUT Contact.CustNum ", user" INPUT Contact.UserCode
                      "and date" INPUT Contact.PlanDate
              VIEW-AS ALERT-BOX.
              NEXT.
           END.
           
           CREATE Contact.
           ASSIGN
           Contact.Brand    = gcBrand
           Contact.ConId    = NEXT-VALUE(ConID)
           Contact.CustNum  = INPUT FRAME lis Contact.CustNum
           Contact.PlanDate = INPUT FRAME lis Contact.PlanDate
           Contact.ConDate  = Contact.PlanDate
           Contact.UserCode = INPUT FRAME lis Contact.UserCode.

           /* open balance */
           FOR EACH Invoice OF Customer NO-LOCK WHERE
              LOOKUP(STRING(Invoice.InvType),lcTypeDenied) = 0:

              Contact.CustBal = Contact.CustBal + fInvBal(BUFFER Invoice,
                                                          TODAY).
           END.
            
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhContact).

           ASSIGN
           Memory = recid(Contact)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE Contact THEN LEAVE LOOP.
      NEXT LOOP.
   END.
   
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Contact WHERE recid(Contact) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Contact THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Contact).
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
        ufk[1] = 714  
        ufk[2] = 1888
        ufk[3] = 1883 
        ufk[4] = 927
        ufk[5] = (IF lcRight = "RW" AND liState = 0 THEN 1790 ELSE 0) 
        ufk[6] = IF liState = 1 THEN 1789 ELSE 1787
        ufk[7] = 1152
        ufk[8] = 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF iiCustNum > 0 THEN ASSIGN 
           ufk[1] = 0
           ufk[2] = 0
           ufk[3] = 0
           ufk[6] = 0.
           
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW Contact.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contact.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Contact.CustBal {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Contact.CustBal WITH FRAME sel.
      END.


      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8,7,F7,6,F6") = 0 THEN DO:
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
        FIND Contact WHERE recid(Contact) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Contact THEN
              ASSIGN FIRSTrow = i Memory = recid(Contact).
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
           IF NOT AVAILABLE Contact THEN DO:
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
                rtab[1] = recid(Contact)
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
           IF NOT AVAILABLE Contact THEN DO:
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
              rtab[FRAME-DOWN] = recid(Contact).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Contact WHERE recid(Contact) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Contact THEN DO:
           Memory = recid(Contact).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Contact THEN Memory = recid(Contact).
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
           FIND Contact WHERE recid(Contact) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        UPDATE liCustNum WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF liCustNum > 0 THEN DO:
           FIND FIRST Contact WHERE 
                      Contact.Brand    = gcBrand    AND
                      Contact.UserCode = icUserCOde AND
                      Contact.ConState = liState    AND
                      Contact.ConDate  = idtConDate AND
                      Contact.CustNum >= liCustNum
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE Contact THEN DO:
              BELL.
              message "NONE FOUND !".
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.

           ASSIGN order = 1 memory = recid(Contact) must-print = TRUE.

           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
        RUN local-find-this (FALSE).

        RUN Mc/mobilett(Contact.CustNum). 

        ufkey = TRUE.
        NEXT.
     END.

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
        RUN local-find-this (FALSE).

        RUN Mc/commontt(Contact.CustNum). 

        ufkey = TRUE.
        NEXT.
     END.

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        RUN local-find-this (FALSE).

        RUN Mc/memo(INPUT Contact.CustNum,
                 INPUT "Contact",
                 INPUT STRING(Contact.ConID),
                 INPUT "Contact").

        ufkey = TRUE.
        NEXT.
     END.
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0
     THEN DO TRANSACTION:  /* mark handled */

       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF Contact.ConState > 0 THEN DO:
          MESSAGE "Contact has already been handled"
          VIEW-AS ALERT-BOX
          INFORMATION.
          NEXT. 
       END.
       
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Contact.CustNum Customer.CustName Contact.CustBal.

       RUN local-find-NEXT.
       IF AVAILABLE Contact THEN Memory = recid(Contact).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Contact THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Contact).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "Mark this row as handled ?" UPDATE ok FORMAT "Yes/No".
       COLOR DISPLAY VALUE(ccc)
       Contact.CustNum Customer.CustName Contact.CustBal.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhContact).
           
           ASSIGN Contact.ConState = 1
                  Contact.ConDate  = TODAY
                  Contact.UserCode = katun.
                  Contact.ConStamp = fMakeTS().

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhContact).

           fDispTotal().
           
           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE Contact THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
       
     END. /* handled */

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
     
         IF liState = 0 THEN ASSIGN
            FRAME fTotal:TITLE = " HANDLED CUSTOMERS "
            liState = 1.
         ELSE ASSIGN 
            FRAME fTotal:TITLE = " UNHANDLED CUSTOMERS "
            liState = 0.
         
         RUN local-find-first.
         ASSIGN memory = RECID(Contact)         
                must-print = TRUE
                ufkey      = TRUE.

                
         fDispTotal().
                
         NEXT loop.
     END.    

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:
     
        ASSIGN ufk    = 0
               ufk[3] = 638
               ufk[5] = (IF lcRight = "RW" AND liState = 0 THEN 5 ELSE 0)
               ufk[6] = (IF lcRight = "RW" AND liState = 0 THEN 4 ELSE 0)
               ufk[8] = 8
               ehto   = 0
               ufkey  = TRUE.
        RUN Syst/ufkey.

        IF toimi = 3 THEN DO:
           RUN local-find-this (FALSE).
           RUN Ar/conrepui (IF AVAILABLE Contact THEN Contact.UserCode ELSE "",
                         IF AVAILABLE Contact THEN Contact.ConDate ELSE ?).
        END.
        
        ELSE IF toimi = 5 THEN DO:
           {Syst/uright2.i}
           must-add = TRUE.
           NEXT LOOP.
        END.

        ELSE IF toimi = 6 THEN DO TRANS:
       
           {Syst/uright2.i}
           delrow = FRAME-LINE.
           RUN local-find-this (FALSE).

           IF Contact.ConState > 0 THEN DO:
              MESSAGE "Contact has already been handled"
              VIEW-AS ALERT-BOX
              INFORMATION.
              NEXT. 
           END.
       
           /* Highlight */
           COLOR DISPLAY VALUE(ctc)
           Contact.CustNum Customer.CustName Contact.CustBal.

           RUN local-find-NEXT.
           IF AVAILABLE Contact THEN Memory = recid(Contact).
           ELSE DO:
              /* read back the record that is TO be  removed */
              RUN local-find-this (FALSE).                     

              RUN local-find-PREV.
              IF AVAILABLE Contact THEN DO:
                  ASSIGN
                  delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
                  Memory = recid(Contact).
              END.
           END.

           /* FIND back the ROW that is TO be removed */
           RUN local-find-this(TRUE).

           ASSIGN ok = FALSE.
           MESSAGE "Delete this row ?" UPDATE ok FORMAT "Yes/No".
           COLOR DISPLAY VALUE(ccc)
           Contact.CustNum Customer.CustName Contact.CustBal.

           IF ok THEN DO:

               IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhContact).
    
               DELETE Contact.

               fDispTotal().
           
               /* was LAST record DELETEd ? */
               RUN local-find-first.
               IF NOT AVAILABLE Contact THEN DO:
                  CLEAR FRAME sel NO-PAUSE.
                  PAUSE 0 NO-MESSAGE.
                  LEAVE LOOP.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE delrow = 0. /* UNDO DELETE */
       
        END. /* delete */
        
     END.
     
     ELSE IF LOOKUP(nap,"enter,return") > 0
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " VIEW " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
       
       RUN local-disp-row.
       xrecid = recid(Contact).
       ufkey = TRUE.
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Contact) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Contact) must-print = TRUE.
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
      FIND Contact WHERE recid(Contact) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Contact WHERE recid(Contact) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN FIND FIRST Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.CustNum  = iiCustNum USE-INDEX CustNum
      NO-LOCK NO-ERROR.
    
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND FIRST Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.UserCode = icUserCOde AND
         Contact.ConState = liState    AND
         Contact.ConDate  = idtConDate
      NO-LOCK NO-ERROR.
   END.
      
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN FIND LAST Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.CustNum  = iiCustNum USE-INDEX CustNum
      NO-LOCK NO-ERROR.
    
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND LAST Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.UserCode = icUserCOde AND
         Contact.ConState = liState    AND
         Contact.ConDate  = idtConDate
         NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN FIND NEXT Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.CustNum  = iiCustNum USE-INDEX CustNum
      NO-LOCK NO-ERROR.
    
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND NEXT Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.UserCode = icUserCOde AND
         Contact.ConState = liState    AND
         Contact.ConDate  = idtConDate
         NO-LOCK NO-ERROR.
   END.
      
END PROCEDURE.

PROCEDURE local-find-PREV:
 
   IF iiCustNum > 0 THEN DO:
      IF order = 1 THEN FIND PREV Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.CustNum  = iiCustNum USE-INDEX CustNum
      NO-LOCK NO-ERROR.
    
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND PREV Contact WHERE 
         Contact.Brand    = gcBrand    AND
         Contact.UserCode = icUserCOde AND
         Contact.ConState = liState    AND
         Contact.ConDate  = idtConDate
         NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       Contact.ConDate
       Contact.CustNum
       Customer.CustName 
       Contact.CustBal
       lcPhone
       llHandled
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   FIND Customer WHERE
        Customer.Brand   = gcBrand AND
        Customer.CustNum = Contact.CustNum NO-LOCK NO-ERROR. 
   IF AVAILABLE Customer THEN lcPhone = Customer.Phone.
   
   FOR FIRST MobSub OF Customer NO-LOCK:
       lcPhone = MobSub.CLI.
   END.
   
   llHandled = (Contact.ConState = 1).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE. 
      ehto = 5.
      RUN Syst/ufkey.

      FIND TMSUser WHERE TMSUser.UserCode = Contact.UserCode NO-LOCK NO-ERROR.
      
      fSplitTS(Contact.ConStamp,
               OUTPUT ldtStampDate,
               OUTPUT liStampTime).
      lcDispStamp = STRING(ldtStampDate,"99-99-99") + " " +
                    STRING(liStampTime,"hh:mm:ss").
                    
      DISP Contact.PlanDate
           lcDispStamp
           Contact.CustNum
           Customer.CustName WHEN AVAILABLE Customer
           "Unknown" WHEN NOT AVAILABLE Customer @ Customer.CustName
           Contact.CustBal
           Contact.UserCode
           TMSUser.UserName WHEN AVAILABLE TMSUser
      WITH FRAME lis.

      PAUSE MESSAGE "Press ENTER to continue".
      
      LEAVE.
   END.
   
END PROCEDURE.

