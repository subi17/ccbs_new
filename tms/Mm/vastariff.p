/* ----------------------------------------------------------------------
  MODULE .......: VASTariff
  TASK .........: Updates table VASTariff
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 02.06.03 jp 
  CHANGED ......: 15.08.03/aam allow multiple services for one TariffClass
                  04.11.03/aam CCN
                  23.03.04 jp new installation
  VERSION ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {lib/eventlog.i}
        
    DEF VAR lhVASTariff AS HANDLE NO-UNDO.
    lhVASTariff = BUFFER VASTariff:HANDLE.
    RUN StarEventInitialize(lhVASTariff).
                    
    ON F12 ANYWHERE DO:
        run eventview2.p(lhVASTariff).
    END.
END.
                                    
def  new  shared var siirto AS char.

DEF VAR TariffClass      like VASTariff.TariffClass        NO-UNDO.
DEF VAR TariffClassname  like VASTariff.TariffClassname        NO-UNDO.
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
DEF VAR lcrateccn       AS CHAR FORMAT "X(25)"    NO-UNDO.

DEF BUFFER bService FOR VASTariff.

form
    VASTariff.TariffClass        FORMAT "x(4)"  column-label  "TC"
    VASTariff.TariffClassName     FORMAT "X(11)"
    VASTariff.BillCode    FORMAT "x(10)" /* column-label format */
    VASTariff.CCN        COLUMN-LABEL "CCN"
    VASTariff.priceIncVat     FORMAT "z,zz9.999" column-label "InclVat"
    VASTariff.PriceExclVat    FORMAT "z,zz9.999" column-label "ExlcVat"
    VASTariff.todate

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  VALUE ADDED SERVICE MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
   "TariffClass .....:"  VASTariff.TariffClass                         SKIP
   "TariffClass Name.:"  VASTariff.TariffClassname    /* label format */ SKIP
   "Product .........:"  VASTariff.BillCode  BillItem.BIName   AT 40     SKIP
   "Rate ............:"  VASTariff.CCN  lcrateccn                         SKIP
   "Price With VAT ..:"  VASTariff.PriceIncVAT                        SKIP         "Price WithOut VAT:"   VASTariff.PriceExclVAT                       SKIP   
   "From Date .......:"  VASTariff.FromDate                           SKIP
   "to Date .........:"  VASTariff.ToDate   

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS no-label
    FRAME lis.

form /* seek  TariffClass */
    TariffClass
    HELP "Enter Code of Billing Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek TariffClass */
   TariffClass
    HELP "Enter Name of the Billing Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST VASTariff
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE VASTariff THEN ASSIGN
   memory       = recid(VASTariff)
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

   IF must-add THEN DO:  /* Add a VASTariff  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR VASTariff.TariffClass.
           
           IF can-find(first vastariff where 
           vastariff.tariffclass = INPUT FRAME lis VASTariff.TariffClass)
           THEN DO:
              MESSAGE 
              "VAS tariff allready exists " 
              INPUT FRAME lis  VASTariff.TariffClass
              VIEW-AS ALERT-BOX.
              NEXT.
           ENd.

           IF INPUT FRAME lis  VASTariff.TariffClass = "" THEN 
           LEAVE add-row.

           create VASTariff.
           ASSIGN
           VASTariff.TariffClass = INPUT FRAME lis VASTariff.TariffClass.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhVASTariff).
            
           ASSIGN
           memory = recid(VASTariff)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST VASTariff
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VASTariff THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND VASTariff WHERE recid(VASTariff) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE VASTariff THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(VASTariff).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row VASTariff.TariffClass ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASTariff.TariffClass WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row VASTariff.TariffClass ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VASTariff.TariffClass WITH FRAME sel.
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
        FIND VASTariff WHERE recid(VASTariff) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE VASTariff THEN
              ASSIGN FIRSTrow = i memory = recid(VASTariff).
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
           IF NOT AVAILABLE VASTariff THEN DO:
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
                rtab[1] = recid(VASTariff)
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
           IF NOT AVAILABLE VASTariff THEN DO:
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
              rtab[FRAME-down] = recid(VASTariff).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND VASTariff WHERE recid(VASTariff) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE VASTariff THEN DO:
           memory = recid(VASTariff).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE VASTariff THEN memory = recid(VASTariff).
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
           FIND VASTariff WHERE recid(VASTariff) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET TariffClass WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF TariffClass ENTERED THEN DO:
          FIND FIRST VASTariff WHERE VASTariff.TariffClass >= TariffClass
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VASTariff THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VASTariff/TariffClass was found */
          ASSIGN order = 1 memory = recid(VASTariff) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

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
       VASTariff.TariffClass     /* column-label format */
       VASTariff.TariffClassName     /* column-label format */
       VASTariff.BillCode  /* column-label format */
       VASTariff.priceIncVat
       VASTariff.PriceExclVat
       VASTariff.todate .


       RUN local-find-NEXT.
       IF AVAILABLE VASTariff THEN memory = recid(VASTariff).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE VASTariff THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(VASTariff).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       VASTariff.TariffClass     /* column-label format */
       VASTariff.TariffClassname     /* column-label format */
       VASTariff.BillCode  /* column-label format */
       VASTariff.priceIncVat
       VASTariff.PriceExclVat
       VASTariff.todate  .
       
       IF ok THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhVASTariff).
           
           DELETE VASTariff.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST VASTariff
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
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY VASTariff.TariffClass.
       
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhVASTariff).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhVASTariff).
       
       RUN local-disp-row.
       xrecid = recid(VASTariff).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(VASTariff) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(VASTariff) must-print = true.
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
      find VASTariff WHERE recid(VASTariff) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find VASTariff WHERE recid(VASTariff) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST VASTariff
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST VASTariff USE-INDEX TariffClass
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST VASTariff
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST VASTariff USE-INDEX TariffClass
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT VASTariff
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT VASTariff USE-INDEX TariffClass
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV VASTariff
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV VASTariff USE-INDEX TariffClass
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       VASTariff.TariffClass     /* column-label format */
       VASTariff.TariffClassname     /* column-label format */
       VASTariff.BillCode  /* column-label format */
       VASTariff.CCN
       VASTariff.priceIncVat
       VASTariff.PriceExclVat
       VASTariff.todate 
       

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
              
   FIND FIRST BillItem  WHERE 
              BillItem.BillCode  = VASTariff.BillCode NO-LOCK NO-ERROR.

   FIND first ccn WHERE
              ccn.ccn = vastariff.ccn no-lock no-error.
              
   if avail ccn THEN lcrateccn = ccn.ccnname.           
    

END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      BillItem.BIName  WHEN AVAIL BillItem
      lcrateccn
      WITH FRAME lis.
      UPDATE
      VASTariff.TariffClassname     /* label format */
      VASTariff.BillCode
      VASTariff.CCN
      VASTariff.PriceIncVAT       
      VASTariff.PriceExclVAT                
      VASTariff.FromDate 
      VASTariff.ToDate 

      WITH FRAME lis
      EDITING:
             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "BillCode" THEN DO:
                   FIND FIRST BillItem WHERE BillItem.BillCode =
                   INPUT FRAME lis VASTariff.Billcode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Code!".
                      NEXT.
                   END.
                   DISP BillItem.BIName WITH FRAME lis.
                END.
                ELSE IF FRAME-FIELD = "ccn" THEN DO:
                   FIND FIRST ccn WHERE ccn.ccn =
                       INPUT FRAME lis VASTariff.ccn NO-LOCK NO-ERROR.
                   
                   IF NOT AVAIL ccn THEN DO:
                      BELL.
                      MESSAGE "Unknown CCN !".
                      NEXT.
                   END.
                   DISP ccn.ccnname WITH FRAME lis.
                END.
                



                ELSE IF FRAME-FIELD = "TariffClass" OR 
                        FRAME-FIELD = "FromDate"
                THEN DO:
                   IF CAN-FIND(FIRST bService WHERE
                       bService.TariffClass       = 
                            INPUT FRAME lis VASTariff.TariffClass AND
                       bService.TariffClass = 
                            INPUT FRAME lis VASTariff.TariffClass AND
                       bService.CCN    = 
                            INPUT FRAME lis VASTariff.CCN   AND
                       bService.FromDate    = 
                           INPUT FRAME lis VASTariff.FromDate    AND
                       RECID(bService) NE RECID(VASTariff))
                   THEN DO:
                       BELL.
                       MESSAGE "Service already exists.".
                       NEXT.
                   END.
                       
                END.

             END.
             
             APPLY LASTKEY.
             
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.
