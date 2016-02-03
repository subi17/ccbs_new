/* ---------------------------------------- 
  MODULE .......: NNKOYP.P
  FUNCTION .....: Korkotietojen yllApito
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 01.09.1997 pt
  changePVM ....: 28.07.1998 pt DISP CustIntEvent.DueDate kun ENTER
                  25.02.2000 jp APPLY TO user CREATE a NEW korko-fields
                  20.06.2000 pt Ask invoice no. FOR NEW CustIntEvent record
                  29.04.2002 tk eventlogging added
                  26.09.2002/aam customer balances in table CustBal 
                  05.03.2003/tk tokens
                  17.09.2003/aam brand
                  23.09.2004/aam billed events can not be deleted
                  20.12.2005/aam iiCustNum,
                                 F4 text removed
                  12.01.2006/aam calculate interest amount in update
                  24.01.2006/jt  DYNAMIC-FUNCTION("fDispCustName"
  Version ......: M15
----------------  ------------------------------------------------------ */
&GLOBAL-DEFINE BrTable CustIntEvent

{Syst/commali.i}
{Syst/eventval.i}
{Func/fcustbal.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'custintevent'}
{Func/cparam2.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustIntEvent AS HANDLE NO-UNDO.
   lhCustIntEvent = BUFFER CustIntEvent:HANDLE.
   RUN StarEventInitialize(lhCustIntEvent).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhCustIntEvent).
   END.

END.


DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.


DEF new shared VAR siirto AS CHAR.

def var ok         as log format "Yes/No"     NO-UNDO.
DEF VAR haku-asno  LIKE CustIntEvent.CustNum  NO-UNDO.
DEF VAR haku-lanro LIKE CustIntEvent.CustNum  NO-UNDO.
DEF VAR firstline  AS INT              NO-UNDO.
DEF VAR order      AS INT              NO-UNDO.
DEF VAR ex-order   AS INT              NO-UNDO.
DEF VAR memory     AS RECID            NO-UNDO.
def var line       as int format "99"  NO-UNDO.
DEF VAR delline    AS INT              NO-UNDO.
DEF VAR must-print AS LOG              NO-UNDO.
DEF VAR must-add   AS LOG              NO-UNDO.
DEF VAR ufkey      AS LOG              NO-UNDO.
DEF VAR fr-header  AS CHAR             NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24  NO-UNDO.
DEF VAR i          AS INT              NO-UNDO.
DEF VAR ac-hdr     AS c                NO-UNDO.

DEF VAR nlkm         AS INT   NO-UNDO.
DEF VAR nlkm2        AS INT   NO-UNDO.
DEF VAR xrecid       AS RECID NO-UNDO.

DEF VAR rc           AS INT   NO-UNDO.
DEF VAR seli         AS CHAR  NO-UNDO.
DEF VAR liRow        AS INT   NO-UNDO.
DEF VAR liDown       AS INT   NO-UNDO. 
DEF VAR ldInterest   AS DEC   NO-UNDO.
DEF VAR lcIDays      AS CHAR  NO-UNDO.
DEF VAR lcIPerc      AS CHAR  NO-UNDO.
DEF VAR liIntCalcMet AS INT   NO-UNDO.
DEF VAR lcCustName   AS CHAR  NO-UNDO.

/* Interests */
DEF NEW SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.



form
    CustIntEvent.Brand      column-label "Bran" format "x(4)" 
    CustIntEvent.CustNum    column-label "CustNo"
    lcCustName              column-label "CustName" format "x(10)"
    CustIntEvent.InvNum     column-label "InvNo"   
    CustIntEvent.PaymDate   column-label "PaymDay"
    CustIntEvent.LateDays   column-label "Late"
    CustIntEvent.PaidAmt    column-label "Paid amt"
    CustIntEvent.Percent    column-label "Int%%"  
    CustIntEvent.Amt        column-label "Int tot"  
WITH width 80 ROW liRow OVERLAY scroll 1 liDown DOWN
    color value(cfc) title color value(ctc) " " + ynimi +
    " OVERTIME INTERESTS " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    CustIntEvent.CustNum   
    lcCustName 
    Customer.COName  
    Customer.Address                  
    Customer.PostOffice  
    CustIntEvent.InvNum                    
    CustIntEvent.DueDate       LABEL "Due Date"
    CustIntEvent.PaymDate      LABEL "Payment Date"                  
    CustIntEvent.LateDays      LABEL "Late Days"                      
    CustIntEvent.PaidAmt       LABEL "Paid Amount"     FORMAT "->>>>>>>9.99"
    CustIntEvent.Amt           LABEL "Interest Amount" FORMAT "->>>>>>>9.99"
    CustIntEvent.BilledInvNum  LABEL "Billed on Inv.Nbr"                       
WITH  OVERLAY ROW 3 col 3
    COLOR value(cfc) TITLE COLOR value(ctc) ac-hdr 1 col FRAME lis.

{Func/brand.i}

form
   "Days   %%      Total"                          SKIP
   "---- ---- ----------"                          SKIP
   CustIntEvent.IntDays[ 1]  CustIntEvent.IntPerc[ 1] CustIntEvent.IntAmt[ 1] 
   SKIP
   CustIntEvent.IntDays[ 2]  CustIntEvent.IntPerc[ 2] CustIntEvent.IntAmt[ 2] 
   SKIP
   CustIntEvent.IntDays[ 3]  CustIntEvent.IntPerc[ 3] CustIntEvent.IntAmt[ 3] 
   SKIP
   CustIntEvent.IntDays[ 4]  CustIntEvent.IntPerc[ 4] CustIntEvent.IntAmt[ 4] 
   SKIP
   CustIntEvent.IntDays[ 5]  CustIntEvent.IntPerc[ 5] CustIntEvent.IntAmt[ 5] 
   SKIP
   CustIntEvent.IntDays[ 6]  CustIntEvent.IntPerc[ 6] CustIntEvent.IntAmt[ 6] 
   SKIP
   CustIntEvent.IntDays[ 7]  CustIntEvent.IntPerc[ 7] CustIntEvent.IntAmt[ 7] 
   SKIP
   CustIntEvent.IntDays[ 8]  CustIntEvent.IntPerc[ 8] CustIntEvent.IntAmt[ 8] 
   SKIP
   CustIntEvent.IntDays[ 9]  CustIntEvent.IntPerc[ 9] CustIntEvent.IntAmt[ 9] 
   SKIP
   CustIntEvent.IntDays[10]  CustIntEvent.IntPerc[10] CustIntEvent.IntAmt[10] 
   SKIP
WITH                   
   row 3 overlay col 53 title " Interest consists of " NO-LABEL FRAME inter.

form /* Asiakasnumerohaku */
    "Brand ..:" lcBrand skip
    "Customer:" haku-asno
    help "Enter number of Customer"
    with row 4 col 2 title color value(ctc) " FIND CUSTOMER "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* LAskunumerohaku */
    "Brand .:" lcBrand skip
    "Invoice:" haku-lanro
    help "Enter number of Invoice"
    with row 4 col 2 title color value(ctc) " FIND INVOICE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

IF iiCustNum > 0 THEN ASSIGN 
   liRow  = 4 
   liDown = 10.
ELSE ASSIGN 
   liRow  = 1
   liDown = 15.
   
cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

order = 1.

RUN local-find-first.

IF AVAILABLE CustIntEvent THEN ASSIGN 
   memory = recid(CustIntEvent)
   must-print = TRUE must-add    = FALSE.
   
ELSE ASSIGN memory = ? must-print = FALSE 
     must-add    = FALSE.

ASSIGN xrecid = ? delline = 0 ufkey = TRUE firstline = 0.

liIntCalcMet = fCParamI("IntCalcMet").

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.

       if order = 1 then put screen row 19 col 30 "  By Customer No.  ".
       if order = 2 then put screen row 19 col 30 "  By Invoice No.   ".

    END.


   IF must-add THEN DO:  /* Add a BillType  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.


      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           IF iiCustNum = 0 THEN 
           PROMPT-FOR CustIntEvent.CustNum
           VALIDATE(INPUT CustNum = "" OR
                    CAN-FIND(FIRST Customer WHERE 
                                   Customer.Brand   = lcBrand AND
                                   Customer.CustNum = INPUT CustNum),
                             "Invalid customer number !").

           ELSE DISPLAY iiCustNum @ CustIntEvent.CustNum WITH FRAME lis.
           
           IF INPUT FRAME lis CustIntEvent.CustNum = "" THEN 
           LEAVE ADD-ROW.

           CREATE CustIntEvent.
           ASSIGN
           CustIntEvent.Brand   = lcBrand  
           CustIntEvent.CustNum = INPUT FRAME lis CustIntEvent.CustNum.

           RUN local-update-record.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustIntEvent).                                                                         
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(CustIntEvent)
           xrecid = memory.
           LEAVE.
        END.

      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CustIntEvent
      WHERE CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CustIntEvent THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:

         up FRAME-LINE - 1.
         FIND CustIntEvent where recid(CustIntEvent) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE CustIntEvent THEN DO:

               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(CustIntEvent).
               
               RUN local-find-next.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.  
         up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
         the cursor is in the upmost line FOR 'choose' */
      END. /*must-print = TRUE.*/ 


   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 702  ufk[2]= 92 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)   
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         
         IF iiCustNum > 0 THEN ASSIGN ufk[1] = 0
                                      ufk[2] = 0.
         
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW CustIntEvent.CustNum  ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CustIntEvent.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW CustIntEvent.InvNum ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CustIntEvent.InvNum WITH FRAME sel. 
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

      if lookup(nap,"cursor-right") > 0 AND iiCustNum = 0 THEN DO:
         order = order + 1. IF order = 3 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 AND iiCustNum = 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND CustIntEvent where recid(CustIntEvent) = memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.

            IF AVAILABLE CustIntEvent THEN
               ASSIGN firstline = i memory = recid(CustIntEvent).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND CustIntEvent where recid(CustIntEvent) = rtab[1] no-lock.
            RUN local-find-prev.

            IF NOT AVAILABLE CustIntEvent THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CustIntEvent)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CustIntEvent where recid(CustIntEvent) = rtab[FRAME-DOWN] 
            no-lock .
            
            RUN local-find-next.
            
            IF NOT AVAILABLE CustIntEvent THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CustIntEvent).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND CustIntEvent where recid(CustIntEvent) = memory no-lock no-error.
         
         RUN local-find-prev.
         
         IF AVAILABLE CustIntEvent THEN DO:
            memory = recid(CustIntEvent).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               
               RUN local-find-prev.
               
               IF AVAILABLE CustIntEvent THEN memory = recid(CustIntEvent).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND CustIntEvent where recid(CustIntEvent) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN ufcolor.
        haku-asno = 0.
        ehto = 9. RUN ufkey. ufkey = TRUE.

        DISPLAY lcBrand WITH FRAME hayr.
        UPDATE lcBrand WHEN gcAllBrand
               haku-asno WITH FRAME hayr.
        HIDE FRAME hayr no-pause.

        IF haku-asno <> 0 THEN DO:
           FIND FIRST CustIntEvent where 
                      CustIntEvent.Brand    = lcBrand AND
                      CustIntEvent.CustNum >= haku-asno no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN ufcolor.
        haku-lanro = 0.
        ehto = 9. RUN ufkey. ufkey = TRUE.

        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN gcAllBrand
               haku-lanro WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.

        IF haku-lanro <> 0 THEN DO:
           FIND FIRST CustIntEvent where 
                      CustIntEvent.Brand   = lcBrand AND
                      CustIntEvent.InvNum >= haku-lanro no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */


     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */
        {Syst/uright2.i}
        delline = FRAME-LINE.
        FIND CustIntEvent where recid(CustIntEvent) = rtab[FRAME-LINE] no-lock.
        
        IF CustIntEvent.BilledInvNum > 0 THEN DO:
           MESSAGE "Event has already been billed, delete not allowed."
           VIEW-AS ALERT-BOX
           INFORMATION.
           delline = 0.
           NEXT.
        END.
        
        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc)
        lcCustName CustIntEvent.CustNum CustIntEvent.PaymDate 
        CustIntEvent.InvNum CustIntEvent.LateDays
        CustIntEvent.PaidAmt CustIntEvent.Percent CustIntEvent.Amt.

        RUN local-find-next.

        IF AVAILABLE CustIntEvent THEN memory = recid(CustIntEvent).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND CustIntEvent where recid(CustIntEvent) = rtab[FRAME-LINE] 
           no-lock.
           
           /* AND THEN the previous one */
           RUN local-find-prev.
           IF AVAILABLE CustIntEvent THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(CustIntEvent).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND CustIntEvent where recid(CustIntEvent) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.

        COLOR DISPLAY value(ccc)
        lcCustName CustIntEvent.CustNum CustIntEvent.PaymDate 
        CustIntEvent.InvNum CustIntEvent.LateDays
        CustIntEvent.PaidAmt CustIntEvent.Percent CustIntEvent.Amt.

        IF ok THEN DO:
            /* reduce customer's balance */
            fCustBal(CustIntEvent.CustNum,
                     "",   
                     "INT",
                     -1 * CustIntEvent.Amt). 

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustIntEvent).

            DELETE CustIntEvent.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST CustIntEvent WHERE 
                                  CustIntEvent.Brand = lcBrand) 
            THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.


     else if lookup(nap,"enter,return") > 0 THEN DO:
        ac-hdr = " VIEW ".
        cfc = "lis". RUN ufcolor.
        ufk = 0. ehto = 3. RUN ufkey. ufkey = TRUE.

        FIND CustIntEvent where recid(CustIntEvent) = rtab[FRAME-LINE] no-lock.

        FIND Customer of CustIntEvent no-lock no-error.

        PAUSE 0.
        DISP
          CustIntEvent.CustNum
          lcCustName
          Customer.COName
          Customer.Address
          Customer.PostOffice
          CustIntEvent.InvNum
          CustIntEvent.DueDate
          CustIntEvent.PaymDate
          CustIntEvent.LateDays
          CustIntEvent.PaidAmt
          CustIntEvent.Amt
          CustIntEvent.BilledInvNum   when CustIntEvent.BilledInvNum NE 0
        WITH FRAME lis.
        PAUSE 0.

        DISP 
        CustIntEvent.IntPerc CustIntEvent.IntDays CustIntEvent.IntAmt 
        WITH FRAME inter.

        ASSIGN ufk =  0 ufkey = TRUE ehto = 3.
        RUN ufkey.
        PAUSE 0.
        message "Press ENTER !".
        PAUSE no-message.
        HIDE FRAME lis. CLEAR FRAME lis. HIDE FRAME inter.
        NEXT.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:

        RUN local-find-first.

        ASSIGN memory = recid(CustIntEvent) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
     
        RUN local-find-last.

        ASSIGN memory = recid(CustIntEvent) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-find-others:
  FIND Customer OF CustIntEvent NO-LOCK NO-ERROR.

END PROCEDURE.



PROCEDURE local-update-record:

   DEF VAR ldOldAmt AS DEC NO-UNDO.

   ldOldAmt = CustIntEvent.Amt.

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                     BUFFER Customer).
                                                 
      DISP
       lcCustName
       Customer.COName  
       Customer.Address                  
       Customer.PostOffice  
       CustIntEvent.DueDate
       CustIntEvent.BilledInvNum

      WITH FRAME lis.

      UPDATE
         CustIntEvent.InvNum   
         CustIntEvent.PaymDate  
         CustIntEvent.LateDays  
         CustIntEvent.PaidAmt   
         CustIntEvent.Amt                       
      WITH FRAME lis
      EDITING:

             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),"F9") > 0 AND FRAME-FIELD = "InvNum" 
             THEN DO:
                RUN hcustinv.p(CustIntEvent.CustNum, TRUE).
                IF siirto NE ? THEN disp siirto @ 
                               CustIntEvent.InvNum with frame lis.
                ehto = 9.  run ufkey.
                next.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "InvNum" THEN DO:
                   FIND Invoice WHERE 
                        Invoice.InvNum = INPUT CustIntEvent.InvNum 
                   NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE Invoice OR
                      Invoice.CustNum NE CustIntEvent.CustNum
                   THEN DO:
                      BELL.
                      MESSAGE "Customer hasn't got an invoice with given nbr".
                      NEXT.
                   END.
                    
                   CustIntEvent.DueDate = Invoice.DueDate.
                   DISPLAY CustIntEvent.DueDate WITH FRAME lis.
                   
                END.
                
                ELSE IF FRAME-FIELD = "PaymDate" THEN DO:
                   IF INPUT CustIntEvent.PaymDate NE ? AND
                      CustIntEvent.DueDate NE ? AND
                      INPUT CustIntEvent.PaymDate > CustIntEvent.DueDate
                   THEN DISPLAY INPUT CustIntEvent.PaymDate - 
                                CustIntEvent.DueDate @ CustIntEvent.LateDays
                        WITH FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "PaidAmt" THEN DO:

                   RUN calcint(INPUT CustIntEvent.DueDate,
                               INPUT INPUT CustIntEvent.PaymDate,
                               INPUT liIntCalcMet,
                               INPUT INPUT CustIntEvent.PaidAmt,
                               INPUT CustIntEvent.CustNum,
                               OUTPUT lcIDays,
                               OUTPUT lcIPerc,
                               OUTPUT ldInterest).

                   CustIntEvent.Percent = DECIMAL(ENTRY(1,lcIPerc)) / 100.
                   DO i = 1 TO 10:
                      ASSIGN
                      CustIntEvent.IntPerc[i]   = intpro[i]
                      CustIntEvent.IntDays[i]   = intdays[i]
                      CustIntEvent.IntAmt[i] = intsumm[i].
                   END.
  
                   IF INPUT CustIntEvent.Amt = 0 
                   THEN DISPLAY ldInterest @ CustIntEvent.Amt 
                        WITH FRAME lis.
                   ELSE IF ldInterest NE INPUT CustIntEvent.Amt THEN DO:
                       
                      ok = TRUE.
                      MESSAGE "Interest amount with given late days and paid"
                              "amount is" 
                              TRIM(STRING(ldInterest,"->>>>>>9.99"))
                              ". Current interest is" 
                              TRIM(STRING(INPUT CustIntEvent.Amt,
                                          "->>>>>>9.99"))
                              ", replace it with the new interest amount ?"    
                      VIEW-AS ALERT-BOX QUESTION
                      BUTTONS YES-NO
                      SET ok.
                      IF ok THEN DISPLAY ldInterest @ CustIntEvent.Amt
                                 WITH FRAME lis.
                   END.
                END.
                
                ELSE IF FRAME-FIELD = "Amt" THEN DO:
                   IF INPUT FRAME lis CustIntEvent.Amt = 0 THEN DO:
                      BELL.
                      MESSAGE "You MUST supply an Sum of Interest".
                      NEXT.
                   END.
                   /*DISP <table>.<field2>.*/
                END.

             END.
             APPLY LASTKEY.
      END. /* EDITING */

      LEAVE.
   END.

   /* update customer's balance */
   fCustBal(CustIntEvent.CustNum,
            "",
            "INT",
            CustIntEvent.Amt - ldOldAmt). 

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.
   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                               BUFFER Customer).
                                              
   DISPLAY CustIntEvent.Brand
           lcCustName when AVAIL Customer
           "!! BLANK !!" when NOT AVAIL Customer @ lcCustName
           CustIntEvent.CustNum 
           CustIntEvent.PaymDate CustIntEvent.InvNum CustIntEvent.LateDays
           CustIntEvent.PaidAmt CustIntEvent.Percent CustIntEvent.Amt
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-first:

   IF iiCustNum > 0 THEN DO:
      FIND FIRST CustIntEvent WHERE
                 CustIntEvent.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND FIRST CustIntEvent WHERE 
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
     ELSE IF order = 2 THEN FIND FIRST CustIntEvent USE-INDEX InvNum WHERE
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
   END.
          
END PROCEDURE.

PROCEDURE local-find-last:

   IF iiCustNum > 0 THEN DO:
      FIND LAST CustIntEvent WHERE
                CustIntEvent.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND LAST CustIntEvent WHERE 
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
     ELSE IF order = 2 THEN FIND LAST CustIntEvent USE-INDEX InvNum WHERE
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
   END.
          
END PROCEDURE.

PROCEDURE local-find-next:

   IF iiCustNum > 0 THEN DO:
      FIND NEXT CustIntEvent WHERE
                CustIntEvent.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND NEXT CustIntEvent WHERE 
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
     ELSE IF order = 2 THEN FIND NEXT CustIntEvent USE-INDEX InvNum WHERE
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
   END.
          
END PROCEDURE.

PROCEDURE local-find-prev:

   IF iiCustNum > 0 THEN DO:
      FIND PREV CustIntEvent WHERE
                CustIntEvent.CustNum = iiCustNum NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN FIND PREV CustIntEvent WHERE 
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
     ELSE IF order = 2 THEN FIND PREV CustIntEvent USE-INDEX InvNum WHERE
         CustIntEvent.Brand = lcBrand NO-LOCK NO-ERROR.
   END.
          
END PROCEDURE.


