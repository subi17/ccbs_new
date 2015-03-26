/* ----------------------------------------------------------------------
  MODULE .......: Mmservicelpool
  TASK .........: Show Mmservicelpool records
  APPLICATION ..: TMS
  AUTHOR .......: as 
  CREATED ......: 11/2010 
  VERSION ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{date.i}
{timestamp.i}
 
DEF INPUT PARAM iiMSSeq as INT  no-undo.
DEF INPUT PARAM iiSlSeq AS INT NO-UNDO.
DEF INPUT PARAM idaValidFrom AS DATE NO-UNDO.
DEF INPUT PARAM idaValidTo AS DATE NO-UNDO.

DEFINE VARIABLE ldeEndTS AS DECIMAL NO-UNDO. 
DEFINE VARIABLE ldeFromTS AS DECIMAL NO-UNDO. 

ldeEndTS = fMake2Dt(idaValidTo,86399).
ldeFromTs = fMake2Dt(idaValidFrom,0).

def /* new */ shared var siirto AS char.

DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 1.
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
DEF VAR lcName       AS char                   NO-UNDO.
DEF VAR lceventname  AS CHAR                   NO-UNDO.
DEF VAR usedamt      AS CHAR                   NO-UNDO.
DEF VAR lcInclUnit   AS CHAR                   NO-UNDO.
DEF VAR lcUsage      AS CHAR                   NO-UNDO.
DEF VAR lcLimit      AS CHAR                   NO-UNDO.
DEF VAR lcBillItem   AS CHAR                   NO-UNDO.
DEF VAR lcPeriodType AS CHAR                   NO-UNDO.
DEF VAR ldaFromDate  AS DATE                   NO-UNDO.
DEF VAR ldaEndDate   AS DATE                   NO-UNDO.
DEF VAR ldeStartingFees AS DECIMAL NO-UNDO. 
DEF VAR ldeUnitCharge   AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcFromTs AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcEndTs AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcServiceLimit AS CHARACTER NO-UNDO. 

FIND FIRST Servicelimit WHERE 
           ServiceLimit.slseq = iiSLSeq no-lock no-error.
IF AVAIL Servicelimit THEN lcServiceLimit = Servicelimit.SLCode.

form
    mservicelpool.MsSeq
    lcFromTS FORMAT "x(20)" column-label "From"
    lcEndTS FORMAT "x(20)" column-label "To"
    lcLimit FORMAT "x(8)" column-label "Limit"
    lcInclUnit FORMAT "x(10)" column-label "Unit"
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    " Rating Pools " + lcServiceLimit
    FRAME sel.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By MSSeq   , By SLSeq ,By 3, By 4".

RUN local-find-first.

IF AVAILABLE mservicelpool THEN ASSIGN
   memory       = recid(mservicelpool)
   must-print   = true
   must-add     = false.
ELSE DO:
   
   MESSAGE "No mservicelpool records available !" VIEW-AS ALERT-BOX.
   RETURN.
END.
   
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a mservicelpool  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR mservicelpool.MSSeq
           validate
              (mservicelpool.MSSeq NOT ENTERED or
              NOT CAN-FIND(mservicelpool using  mservicelpool.MSSeq),
              "Billing Type " + string(INPUT mservicelpool.MSSeq) +
              " already exists !").
           IF INPUT FRAME lis mservicelpool.MSSeq = "" THEN 
           LEAVE add-row.
           create mservicelpool.
           ASSIGN
           mservicelpool.MSSeq = INPUT FRAME lis mservicelpool.MSSeq.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           ASSIGN
           memory = recid(mservicelpool)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE mservicelpool THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND mservicelpool WHERE recid(mservicelpool) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE mservicelpool THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(mservicelpool).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row msseq ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) msseq WITH FRAME sel.
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
        FIND mservicelpool WHERE recid(mservicelpool) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE mservicelpool THEN
              ASSIGN FIRSTrow = i memory = recid(mservicelpool).
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
           IF NOT AVAILABLE mservicelpool THEN DO:
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
                rtab[1] = recid(mservicelpool)
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
           IF NOT AVAILABLE mservicelpool THEN DO:
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
              rtab[FRAME-down] = recid(mservicelpool).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND mservicelpool WHERE recid(mservicelpool) = memory 
        NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE mservicelpool THEN DO:
           memory = recid(mservicelpool).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE mservicelpool THEN memory = recid(mservicelpool).
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
           FIND mservicelpool WHERE recid(mservicelpool) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN DO TRANS:
       MESSAGE "This view is not yet implemented" VIEW-AS ALERT-BOX. 
       LEAVE. /* This view is not yet implemented */
/*
       /* change */
       RUN local-find-this(false).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. 
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       
       RUN local-disp-row.
       xrecid = recid(mservicelpool).
       LEAVE. */
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(mservicelpool) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(mservicelpool) must-print = true.
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
      find mservicelpool WHERE recid(mservicelpool) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find mservicelpool WHERE recid(mservicelpool) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
        FIND FIRST mservicelpool WHERE 
                   mservicelpool.MsSeq = iiMsSeq AND
                   mservicelpool.SLSeq = iiSLSeq AND
                   mservicelpool.endts <= ldeEndTS AND
                   mservicelpool.fromts >= ldeFromTS
        NO-LOCK NO-ERROR.              
END PROCEDURE.

PROCEDURE local-find-LAST:
        
        FIND LAST mservicelpool WHERE 
                   mservicelpool.MsSeq = iiMsSeq AND
                   mservicelpool.SLSeq = iiSLSeq AND
                   mservicelpool.endts <= ldeEndTS AND
                   mservicelpool.fromts >= ldeFromTS
        NO-LOCK NO-ERROR.              
    
END PROCEDURE.

PROCEDURE local-find-NEXT:
        
        FIND NEXT mservicelpool WHERE 
                   mservicelpool.MsSeq = iiMsSeq AND
                   mservicelpool.SLSeq = iiSLSeq AND
                   mservicelpool.endts <= ldeEndTS AND
                   mservicelpool.fromts >= ldeFromTS
        NO-LOCK NO-ERROR.              
    
END PROCEDURE.

PROCEDURE local-find-PREV:
        
        FIND PREV mservicelpool WHERE 
                   mservicelpool.MsSeq = iiMsSeq AND
                   mservicelpool.SLSeq = iiSLSeq AND
                   mservicelpool.endts <= ldeEndTS AND
                   mservicelpool.fromts >= ldeFromTS
        NO-LOCK NO-ERROR.              
    
END PROCEDURE.

PROCEDURE local-disp-row:
    RUN local-find-others.
    CLEAR FRAME sel NO-PAUSE.

    DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
    lcFromTS = fTS2HMS(mservicelpool.fromts).
    lcEndTs = fTS2HMS(mservicelpool.endts).

    DISPLAY 
       MServiceLPool.MsSeq
       lcFromTs
       lcEndTs
       lcLimit
       lcInclUnit
    WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   
   
   FIND FIRST Servicelimit WHERE 
              ServiceLimit.slseq = mservicelpool.SLSeq no-lock no-error.
   IF AVAIL Servicelimit THEN DO:
    
      /* Textual presentation for unit */
      FIND FIRST TMSCodes WHERE                                       
          TMSCodes.Tablename    = "Tariff"   AND               
          TMSCodes.FieldName    = "DataType"      AND          
          TMSCodes.CodeGroup    = "Tariff"          AND        
          TMSCodes.CodeValue    = STRING(ServiceLimit.InclUnit)
      NO-LOCK NO-ERROR.                                               
      IF AVAIL TMSCodes THEN lcInclUnit = TMSCodes.CodeName.           
    
      CASE ServiceLimit.InclUnit:
         WHEN 1 THEN DO:
            lcLimit = STRING(INT(mServiceLPool.LimitAmt * 60),"HH:MM:SS").
         END.
         WHEN 2 THEN DO:
            lcLimit = STRING(INT(mServiceLPool.LimitAmt),"HH:MM:SS").
         END.
         OTHERWISE DO:
            lcLimit = STRING(mServiceLPool.LimitAm).
         END.
      END.
   
   END.   
   ELSE lcLimit = STRING(mServiceLPool.LimitAm).

END PROCEDURE.
