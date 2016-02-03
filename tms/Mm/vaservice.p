/* ----------------------------------------------------------------------
  MODULE .......: VAService
  TASK .........: Updates table VAService
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 02.06.03 jp 
  CHANGED ......: 15.08.03/aam allow multiple services for one bdest
                  04.11.03/aam ServType
  VERSION ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {lib/eventlog.i}
        
    DEF VAR lhVAService AS HANDLE NO-UNDO.
    lhVAService = BUFFER VAService:HANDLE.
    RUN StarEventInitialize(lhVAService).
                    
    ON F12 ANYWHERE DO:
        run eventview2.p(lhVAService).
    END.
END.
                                    
def  new  shared var siirto AS char.

DEF VAR BDest      like VAService.BDest        NO-UNDO.
DEF VAR ServiceName      like VAService.ServiceName        NO-UNDO.
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

DEF BUFFER bService FOR VAService.

form
    VAService.BDest          column-label  "B-Number"
    VAService.ServiceName     FORMAT "X(11)"
    VAService.ServiceAddress  /* column-label format */
    VAService.ServType        COLUMN-LABEL "ST"
    Vaservice.priceIncVat     FORMAT "z,zz9.999" column-label "InclVat"
    Vaservice.PriceExclVat    FORMAT "z,zz9.999" column-label "ExlcVat"
    Vaservice.todate
    lctype                FORMAT "X(9)"    COLUMN-LABEL "InvEvent"

WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  VALUE ADDED SERVICE MENU  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
   "B-Dest ..........:"  VAService.BDest     Bdest.BDName AT 40     SKIP
   "Product .........:"  VaService.BillCode  BillItem.BIName   AT 40     SKIP
   "Service Name ....:"  VAService.ServiceName    /* label format */ SKIP
   "Service Address .:"  VAservice.ServiceAddress                     SKIP
   "Service Type ....:"  VAService.ServType                           SKIP
   "Price With VAT ..:"  Vaservice.PriceIncVAT                        SKIP         "Price WithOut VAT:"  Vaservice.PriceExclVAT                       SKIP   
   "Invoicable Event :"  Vaservice.InvEvent     lctype  AT 30         SKIP
   "From Date .......:"  Vaservice.FromDate                           SKIP
   "to Date .........:"  Vaservice.ToDate   

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS no-label
    FRAME lis.

form /* seek  BDest */
    BDest
    HELP "Enter Code of Billing Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek ServiceName */
   ServiceName
    HELP "Enter Name of the Billing Type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST VAService
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE VAService THEN ASSIGN
   memory       = recid(VAService)
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

   IF must-add THEN DO:  /* Add a VAService  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR VAService.BDest.
           
           IF INPUT FRAME lis VAService.BDest = "" THEN 
           LEAVE add-row.

           FIND FIRST Bdest WHERE Bdest.Bdest =
           INPUT FRAME lis VAService.Bdest NO-LOCK NO-ERROR.
           IF NOT AVAIL Bdest THEN DO:
              BELL.
              MESSAGE "Unknown B-Destination!".
              NEXT.
           END.
           
           create VAService.
           ASSIGN
           VAService.BDest = INPUT FRAME lis VAService.BDest.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhVAService).
            
           ASSIGN
           memory = recid(VAService)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST VAService
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE VAService THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND VAService WHERE recid(VAService) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE VAService THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(VAService).
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
        choose row VAService.BDest ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VAService.BDest WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row VAService.ServiceName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) VAService.ServiceName WITH FRAME sel.
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
        FIND VAService WHERE recid(VAService) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE VAService THEN
              ASSIGN FIRSTrow = i memory = recid(VAService).
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
           IF NOT AVAILABLE VAService THEN DO:
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
                rtab[1] = recid(VAService)
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
           IF NOT AVAILABLE VAService THEN DO:
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
              rtab[FRAME-down] = recid(VAService).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND VAService WHERE recid(VAService) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE VAService THEN DO:
           memory = recid(VAService).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE VAService THEN memory = recid(VAService).
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
           FIND VAService WHERE recid(VAService) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET BDest WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF BDest ENTERED THEN DO:
          FIND FIRST VAService WHERE VAService.BDest >= BDest
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VAService THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VAService/BDest was found */
          ASSIGN order = 1 memory = recid(VAService) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET ServiceName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ServiceName ENTERED THEN DO:
          FIND FIRST VAService WHERE VAService.ServiceName >=ServiceName
          USE-INDEX ServiceName /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE VAService THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some VAService/ServiceName was found */
          ASSIGN order = 2 memory = recid(VAService) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */


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
       VAService.BDest     /* column-label format */
       VAService.ServiceName     /* column-label format */
       VAService.ServiceAddress  /* column-label format */
       Vaservice.priceIncVat
       Vaservice.PriceExclVat
       Vaservice.todate .


       RUN local-find-NEXT.
       IF AVAILABLE VAService THEN memory = recid(VAService).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE VAService THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(VAService).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       VAService.BDest     /* column-label format */
       VAService.ServiceName     /* column-label format */
       VAService.ServiceAddress  /* column-label format */
       Vaservice.priceIncVat
       Vaservice.PriceExclVat
       Vaservice.todate  .
       
       IF ok THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhVAService).
           
           DELETE VAService.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST VAService
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
       DISPLAY VAService.BDest.
       
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhVAService).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhVAService).
       
       RUN local-disp-row.
       xrecid = recid(VAService).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(VAService) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(VAService) must-print = true.
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
      find VAService WHERE recid(VAService) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find VAService WHERE recid(VAService) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST VAService
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST VAService USE-INDEX ServiceName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST VAService
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST VAService USE-INDEX ServiceName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT VAService
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT VAService USE-INDEX ServiceName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV VAService
       /* srule */ NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV VAService USE-INDEX ServiceName
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       VAService.BDest     /* column-label format */
       VAService.ServiceName     /* column-label format */
       VAService.ServiceAddress  /* column-label format */
       VAService.ServType
       Vaservice.priceIncVat
       Vaservice.PriceExclVat
       Vaservice.todate 
       lctype

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND FIRST Bdest WHERE 
              Bdest.Bdest = VAService.BDest NO-LOCK NO-ERROR.
              
   FIND FIRST BillItem  WHERE 
              BillItem.BillCode  = VAService.BillCode NO-LOCK NO-ERROR.

   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "VAService" AND
              TMSCodes.FieldName = "InvEVent" AND
              TMSCodes.CodeGroup   = "InvEvent" AND
              TMSCodes.CodeValue = STRING(VaService.InvEvent)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes 
   THEN lcType = TMSCodes.CodeName.
   ELSE lcType = "unknown".


END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      Bdest.BDName WHEN avail Bdest
      BillItem.BIName  WHEN AVAIL BillItem
      lctype
      WITH FRAME lis.
      UPDATE
      VAService.BDest     /* label format */
      VaService.BillCode
      VAService.ServiceName    /* label format */
      VAservice.ServiceAddress
      VAService.ServType
      Vaservice.PriceIncVAT       
      Vaservice.PriceExclVAT                
      Vaservice.InvEvent 
      Vaservice.FromDate 
      Vaservice.ToDate 

      WITH FRAME lis
      EDITING:
             READKEY.
             IF FRAME-FIELD = "InvEvent" AND keylabel(lastkey) = "F9" THEN DO:
                 RUN h-tmscodes(INPUT "VAservice",  /* TableName*/
                                      "InvEvent", /* FieldName */
                                      "InvEvent", /* GroupCode */
                                OUTPUT siirto).
                 ASSIGN
                    VAService.InvEvent = INT(siirto).
                 DISP 
                    Vaservice.InvEvent with frame lis.
                 ehto = 9.
                 RUN ufkey.p.
             END.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "BDEST" THEN DO:
                   FIND FIRST Bdest WHERE Bdest.Bdest =
                   INPUT FRAME lis VAService.Bdest NO-LOCK NO-ERROR.
                   IF NOT AVAIL Bdest THEN DO:
                      BELL.
                      MESSAGE "Unknown B-Destination!".
                      NEXT.
                   END.
                   DISP Bdest.BDName.
                END.

                ELSE IF FRAME-FIELD = "BillCode" THEN DO:
                   FIND FIRST BillItem WHERE BillItem.BillCode =
                   INPUT FRAME lis VAService.Billcode NO-LOCK NO-ERROR.
                   IF NOT AVAIL BillItem THEN DO:
                      BELL.
                      MESSAGE "Unknown Billing Code!".
                      NEXT.
                   END.
                   DISP BillItem.BIName WITH FRAME lis.
                END.
                
                ELSE IF FRAME-FIELD = "InvEvent" THEN DO:
                   FIND FIRST TMSCodes WHERE
                              TMSCodes.TableName = "VAService" AND
                              TMSCodes.FieldName = "InvEVent" AND
                              TMSCodes.CodeGroup   = "InvEvent" AND
                              TMSCodes.CodeValue = STRING 
                                                   (INPUT Vaservice.InvEvent)  
                   NO-LOCK NO-ERROR.
                   IF NOT AVAIL tmscodes THEN DO:
                      BELL.
                      MESSAGE
                      "Unknown Invoice Event"
                      VIEW-AS ALERT-BOX.
                      NEXT-PROMPT InvEvent. NEXT.
                   END.
                   ELSE IF AVAIL TMSCodes 
                   THEN lcType = TMSCodes.CodeName.
                   
                   disp lctype with FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "ServiceName" OR 
                        FRAME-FIELD = "FromDate"
                THEN DO:
                   IF CAN-FIND(FIRST bService WHERE
                       bService.BDest       = 
                            INPUT FRAME lis VAService.BDest AND
                       bService.ServiceName = 
                            INPUT FRAME lis VAService.ServiceName AND
                       bService.ServType    = 
                            INPUT FRAME lis VAService.ServType   AND
                       bService.FromDate    = 
                           INPUT FRAME lis VAService.FromDate    AND
                       RECID(bService) NE RECID(VAService))
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
