/* ----------------------------------------------------------------------
  MODULE .......: TopupSchemeRow.p
  TASK .........: UPDATEs table TopupSchemeRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 11.03.09
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'TopupSchemeRow'}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTopupSchemeRow AS HANDLE NO-UNDO.
   lhTopupSchemeRow = BUFFER TopupSchemeRow:HANDLE.
   RUN StarEventInitialize(lhTopupSchemeRow).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhTopupSchemeRow).
   END.

END.

DEF INPUT PARAMETER icTopupScheme AS CHAR  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

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
DEF VAR lcHelp       AS CHAR NO-UNDO.
DEF VAR lcItemName   AS CHAR NO-UNDO. 
DEF VAR ldDefFrom    AS DEC  NO-UNDO.
DEF VAR lcDiscount   AS CHAR NO-UNDO.
DEF VAR lcBillItem      AS CHAR NO-UNDO.
DEF VAR lcDiscBillItem  AS CHAR NO-UNDO.
DEF VAR ldPayable       AS DEC  NO-UNDO.


FORM
    TopupSchemeRow.TopupSchemeRowID    FORMAT ">>>>>>9"
    TopupSchemeRow.Amount              FORMAT "->>>>9.99"
    TopupSchemeRow.BillCode  
    TopupSchemeRow.DiscountAmount      FORMAT "->>>>9.99"
    TopupSchemeRow.DiscountBillCode   
    ldtToDate          FORMAT "99-99-99"    COLUMN-LABEL "To"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " ROWS FOR TopupScheme " + STRING(icTopupScheme) + " "
    FRAME sel.

FORM
    TopupSchemeRow.Brand COLON 20 
    TopupSchemeRow.TopupScheme  COLON 20
       TopupScheme.Description FORMAT "X(40)" NO-LABEL SKIP
    TopupSchemeRow.TopupSchemeRowID COLON 20 SKIP(1)
    TopupScheme.VatIncl           COLON 20 LABEL "Tax"
    TopupSchemeRow.Amount         COLON 20
    TopupSchemeRow.BillCode       COLON 20 
       lcBillItem NO-LABEL FORMAT "X(30)" SKIP
    TopupSchemeRow.DiscountAmount COLON 20 
    TopupSchemeRow.DiscountBillCode COLON 20 LABEL "Discount Bill.Item"
       lcDiscBillItem NO-LABEL FORMAT "X(30)" SKIP
    ldPayable COLON 20
       LABEL "Payable Amount" 
       FORMAT "->>>>>>9.99" 
    TopupSchemeRow.DisplayAmount COLON 20
       FORMAT "->>>>>>9.99" 
    TopupSchemeRow.BeginStamp COLON 20
       lcBegin FORMAT "X(20)" NO-LABEL SKIP
    TopupSchemeRow.EndStamp COLON 20
       lcEnd FORMAT "X(20)" NO-LABEL SKIP
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fBillItem RETURNS CHAR
   (icBillCode AS CHAR):
   
   IF icBillCode = "" THEN RETURN "".
   
   FIND FIRST BillItem WHERE
              BillItem.Brand = gcBrand AND
              BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN RETURN BillItem.BIName.
   ELSE RETURN "".

END FUNCTION.
 
 
cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST TopupScheme WHERE 
           TopupScheme.Brand = gcBrand AND 
           TopupScheme.TopupScheme = icTopupScheme NO-LOCK NO-ERROR.
IF NOT AVAILABLE TopupScheme THEN DO:
   MESSAGE "Topup Scheme not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE TopupSchemeRow THEN ASSIGN
   Memory       = recid(TopupSchemeRow)
   must-print   = TRUE
   must-add     = FALSE
   ldDefFrom    = fMake2DT(TODAY,0).
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rows available" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE
      ldDefFrom    = fMake2DT(TopupScheme.FromDate,0).
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a TopupSchemeRow  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY icTopupScheme @ TopupSchemeRow.TopupScheme.

           i = 1. 
           FIND LAST TopupSchemeRow USE-INDEX TopupSchemeRowId
           NO-LOCK NO-ERROR.
           IF AVAILABLE TopupSchemeRow THEN 
              i = TopupSchemeRow.TopupSchemeRowID + 1.
 
           CREATE TopupSchemeRow.
           ASSIGN 
              TopupSchemeRow.Brand   = gcBrand 
              TopupSchemeRow.TopupSchemeRowID = i
              TopupSchemeRow.TopupScheme   = icTopupScheme
              TopupSchemeRow.BeginStamp = ldDefFrom.
              TopupSchemeRow.EndStamp   = fMake2DT(12/31/2049,86399).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              TopupSchemeRow.Amount = 0
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTopupSchemeRow).

           ASSIGN
           Memory = recid(TopupSchemeRow)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TopupSchemeRow THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = Memory 
        NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TopupSchemeRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TopupSchemeRow).
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TopupSchemeRow.TopupSchemeRowID ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TopupSchemeRow.TopupSchemeRowID 
           WITH FRAME sel.
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
        FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TopupSchemeRow THEN
              ASSIGN FIRSTrow = i Memory = recid(TopupSchemeRow).
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
           IF NOT AVAILABLE TopupSchemeRow THEN DO:
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
                rtab[1] = recid(TopupSchemeRow)
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
           IF NOT AVAILABLE TopupSchemeRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(TopupSchemeRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = Memory 
        NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TopupSchemeRow THEN DO:
           Memory = recid(TopupSchemeRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TopupSchemeRow THEN Memory = recid(TopupSchemeRow).
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
           FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = Memory NO-LOCK.
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

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST OfferItem WHERE 
                         OfferItem.Brand    = gcBrand AND
                         OfferItem.ItemType = "Topup" AND
                         OfferItem.ItemKey = TopupSchemeRow.TopupScheme)
       THEN DO:
          MESSAGE "Offer definitions exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          TopupSchemeRow.TopupSchemeRowID
          TopupSchemeRow.Amount
          TopupSchemeRow.BillCode.

       RUN local-find-NEXT.
       IF AVAILABLE TopupSchemeRow THEN Memory = recid(TopupSchemeRow).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TopupSchemeRow THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TopupSchemeRow).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          TopupSchemeRow.TopupSchemeRowID
          TopupSchemeRow.Amount
          TopupSchemeRow.BillCode.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTopupSchemeRow).

           DELETE TopupSchemeRow.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TopupSchemeRow THEN DO:
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
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTopupSchemeRow).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTopupSchemeRow).

       RUN local-disp-row.
       xrecid = recid(TopupSchemeRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TopupSchemeRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TopupSchemeRow) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TopupSchemeRow WHERE recid(TopupSchemeRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST TopupSchemeRow WHERE 
      TopupSchemeRow.Brand = gcBrand AND
      TopupSchemeRow.TopupScheme = icTopupScheme NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST TopupSchemeRow WHERE 
      TopupSchemeRow.Brand = gcBrand AND
      TopupSchemeRow.TopupScheme = icTopupScheme NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT TopupSchemeRow WHERE 
      TopupSchemeRow.Brand = gcBrand AND
      TopupSchemeRow.TopupScheme = icTopupScheme NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV TopupSchemeRow WHERE 
      TopupSchemeRow.Brand = gcBrand AND
      TopupSchemeRow.TopupScheme = icTopupScheme NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      TopupSchemeRow.TopupSchemeRowID
      TopupSchemeRow.Amount
      TopupSchemeRow.BillCode
      TopupSchemeRow.DiscountAmount
      TopupSchemeRow.DiscountBillCode
      ldtToDate
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR liTime AS INT NO-UNDO.
   
   fSplitTS(TopupSchemeRow.BeginStamp,
             OUTPUT ldtFromDate,
             OUTPUT liTime).
   fSplitTS(TopupSchemeRow.EndStamp,
            OUTPUT ldtToDate,
            OUTPUT liTime).

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llUpdateAmount AS LOG  NO-UNDO.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      ASSIGN
         lcBegin        = fTS2HMS(TopupSchemeRow.BeginStamp)
         lcEnd          = fTS2HMS(TopupSchemeRow.EndStamp)
         lcBillItem     = fBillItem(TopupSchemeRow.BillCode)
         lcDiscBillItem = fBillItem(TopupSchemeRow.DiscountBillCode)
         ldPayable      = TopupSchemeRow.Amount - 
                          TopupSchemeRow.DiscountAmount.
        
      DISP 
         TopupSchemeRow.Brand
         TopupSchemeRow.TopupScheme        
         TopupSchemeRow.TopupSchemeRowID
         TopupSchemeRow.Amount         
         TopupSchemeRow.BillCode           
         lcBillItem
         TopupScheme.VatIncl    
         TopupSchemeRow.DiscountAmount
         TopupSchemeRow.DiscountBillCode        
         ldPayable 
         lcDiscBillItem
         TopupSchemeRow.DisplayAmount
         TopupSchemeRow.BeginStamp
         lcBegin
         TopupSchemeRow.EndStamp
         lcEnd
      WITH FRAME lis.

      IF NOT NEW TopupSchemeRow THEN DO:
      
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      llUpdateAmount = (NEW TopupSchemeRow OR
                        TopupSchemeRow.BeginStamp > fMakeTS()).

      IF NOT llUpdateAmount THEN DO:
         IF NOT CAN-FIND(FIRST OfferItem WHERE 
                               OfferItem.Brand    = gcBrand AND
                               OfferItem.ItemType = "Topup" AND
                               OfferItem.ItemKey = TopupSchemeRow.TopupScheme)
         THEN llUpdateAmount = TRUE.                      
      END.
                        
      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT TopupSchemeRow EXCLUSIVE-LOCK.
         ehto = 9.
         RUN ufkey.
         
         UPDATE
            TopupSchemeRow.Amount         WHEN llUpdateAmount
            TopupSchemeRow.BillCode  
            TopupSchemeRow.DiscountAmount WHEN llUpdateAmount
            TopupSchemeRow.DiscountBillCode 
            TopupSchemeRow.DisplayAmount
            TopupSchemeRow.BeginStamp     WHEN llUpdateAmount
            TopupSchemeRow.EndStamp
         WITH FRAME lis EDITING:
 
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
               
               IF FRAME-FIELD = "BillCode" THEN DO:
                  lcBillItem = fBillItem(INPUT INPUT TopupSchemeRow.BillCode).
                  IF lcBillItem = "" THEN DO:
                     MESSAGE "Unknown billing item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "DiscountBillCode" THEN DO:
                 
                  IF INPUT TopupSchemeRow.DiscountBillCode = "" AND
                     INPUT TopupSchemeRow.DiscountAmount NE 0 
                  THEN DO:
                     MESSAGE "Billing item is mandatory"
                     VIEW-AS ALERT-BOX INFORMATION.
                     NEXT.
                  END.
                  
                  lcDiscBillItem = fBillItem(INPUT 
                                       INPUT TopupSchemeRow.DiscountBillCode).
                  IF lcDiscBillItem = "" THEN DO:
                     MESSAGE "Unknown billing item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "Amount" OR 
                       FRAME-FIELD = "DiscountAmount" THEN DO:

                  IF INPUT TopupSchemeRow.Amount = 0 THEN LEAVE.
                  
                  ldPayable = INPUT TopupSchemeRow.Amount -   
                              INPUT TopupSchemeRow.DiscountAmount.
                  DISPLAY ldPayable WITH FRAME lis.
               END.

            END.
            
            APPLY LASTKEY.
         END.

         LEAVE UpdateField.
         
      END.
      
      IF NEW TopupSchemeRow THEN DO:
         ldDefFrom = TopupSchemeRow.BeginStamp.
         LEAVE.
      END.
   END.

END PROCEDURE.

