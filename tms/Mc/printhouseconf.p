/* ----------------------------------------------------------------------
  MODULE .......: PrintHouseConf
  TASK .........: UPDATEs table PrintHouseConf
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.04.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable PrintHouseConf

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PrintHouseConf'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPrintHouseConf AS HANDLE NO-UNDO.
   lhPrintHouseConf = BUFFER PrintHouseConf:HANDLE.
   RUN StarEventInitialize(lhPrintHouseConf).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPrintHouseConf).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcPrintHouse AS CHAR                   NO-UNDO.
DEF VAR lcZipCode    AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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

DEF VAR lcField          AS CHAR NO-UNDO. 
DEF VAR lcCode           AS CHAR NO-UNDO. 


FORM
    PrintHouseConf.PrintHouse
    PrintHouseConf.Report
    PrintHouseConf.FieldName 
    PrintHouseConf.KeyValue
    PrintHouseConf.FromDate COLUMN-LABEL "From"
    PrintHouseConf.ToDate
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " PRINTHOUSE CONFIGURATION "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    PrintHouseConf.Brand          COLON 15
    PrintHouseConf.PrintHouse     COLON 15 
    PrintHouseConf.Report         COLON 15
    PrintHouseConf.TableName      COLON 15 
    PrintHouseConf.FieldName      COLON 15
    PrintHouseConf.KeyValue       COLON 15
    PrintHouseConf.FromDate       COLON 15
    PrintHouseConf.ToDate         COLON 15
       VALIDATE(INPUT PrintHouseConf.ToDate NE ? AND 
                INPUT PrintHouseConf.FromDate NE ? AND
                INPUT PrintHouseConf.ToDate >= INPUT PrintHouseConf.FromDate,
                "End date cannot be earlier than begin date")
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "PrintHouse:" lcPrintHouse FORMAT "X(12)" skip
    "ZipCode...:" lcZipCode    FORMAT "X(6)" 
    HELP "Enter PrintHouse number "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND PrintHouse"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE PrintHouseConf THEN ASSIGN
   Memory       = recid(PrintHouseConf)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No configuration rows available" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a PrintHouseConf  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ PrintHouseConf.Brand.

           PROMPT-FOR PrintHouseConf.PrintHouse WITH FRAME lis.
           IF INPUT PrintHouseConf.PrintHouse = "" THEN UNDO, LEAVE ADD-ROW.
           
           CREATE PrintHouseConf.
           ASSIGN 
              PrintHouseConf.Brand      = lcBrand
              PrintHouseConf.PrintHouse = INPUT FRAME lis
                                             PrintHouseConf.PrintHouse
              PrintHouseConf.TableName  = "Customer"
              PrintHouseConf.FieldName  = "Region"
              PrintHouseConf.Report     = "Invoice".

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPrintHouseConf).

           ASSIGN
           Memory = recid(PrintHouseConf)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PrintHouseConf THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND PrintHouseConf WHERE recid(PrintHouseConf) = Memory 
           NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PrintHouseConf THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PrintHouseConf).
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
        ufk[1]= 816 
        ufk[2]= 0  
        ufk[3]= 0  
        ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PrintHouseConf.PrintHouse {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PrintHouseConf.PrintHouse WITH FRAME sel.
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
        FIND PrintHouseConf WHERE recid(PrintHouseConf) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PrintHouseConf THEN
              ASSIGN FIRSTrow = i Memory = recid(PrintHouseConf).
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
           IF NOT AVAILABLE PrintHouseConf THEN DO:
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
                rtab[1] = recid(PrintHouseConf)
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
           IF NOT AVAILABLE PrintHouseConf THEN DO:
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
              rtab[FRAME-DOWN] = recid(PrintHouseConf).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PrintHouseConf WHERE recid(PrintHouseConf) = Memory 
           NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PrintHouseConf THEN DO:
           Memory = recid(PrintHouseConf).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PrintHouseConf THEN Memory = recid(PrintHouseConf).
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
           FIND PrintHouseConf WHERE recid(PrintHouseConf) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcPrintHouse 
           lcZipCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcPrintHouse > "" THEN DO:

          IF lcZipCode > "" THEN
          FIND FIRST PrintHouseConf WHERE 
                     PrintHouseConf.Brand = lcBrand AND
                     PrintHouseConf.PrintHouse >= lcPrintHouse AND
                     PrintHouseConf.FieldName = "ZipCode" AND
                     PrintHouseConf.KeyValue = lcZipCode
          NO-LOCK NO-ERROR.
          ELSE
          FIND FIRST PrintHouseConf WHERE 
                     PrintHouseConf.Brand = lcBrand AND
                     PrintHouseConf.PrintHouse >= lcPrintHouse
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
       ELSE IF lcZipCode > "" THEN DO:
          FIND FIRST PrintHouseConf WHERE 
                     PrintHouseConf.Brand = lcBrand AND
                     PrintHouseConf.Report = "Invoice" AND
                     PrintHouseConf.TableName = "Customer" AND
                     PrintHouseConf.FieldName = "ZipCode" AND
                     PrintHouseConf.KeyValue = lcZipCode
          NO-LOCK NO-ERROR.
          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       PrintHouseConf.PrintHouse PrintHouseConf.Report
       PrintHouseConf.FieldName PrintHouseConf.KeyValue .

       RUN local-find-NEXT.
       IF AVAILABLE PrintHouseConf THEN Memory = recid(PrintHouseConf).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PrintHouseConf THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PrintHouseConf).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PrintHouseConf.PrintHouse PrintHouseConf.Report
       PrintHouseConf.FieldName PrintHouseConf.KeyValue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPrintHouseConf).

           DELETE PrintHouseConf.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST PrintHouseConf WHERE 
                                 PrintHouseConf.Brand = lcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPrintHouseConf).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PrintHouseConf.PrintHouse.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPrintHouseConf).

       RUN local-disp-row.
       xrecid = recid(PrintHouseConf).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PrintHouseConf) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PrintHouseConf) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PrintHouseConf WHERE recid(PrintHouseConf) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PrintHouseConf WHERE recid(PrintHouseConf) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PrintHouseConf 
          WHERE PrintHouseConf.Brand = lcBrand
          USE-INDEX PrintHouse NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PrintHouseConf
          WHERE PrintHouseConf.Brand = lcBrand
          USE-INDEX PrintHouse NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PrintHouseConf
          WHERE PrintHouseConf.Brand = lcBrand
          USE-INDEX PrintHouse NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PrintHouseConf
          WHERE PrintHouseConf.Brand = lcBrand
          USE-INDEX PrintHouse NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       PrintHouseConf.PrintHouse 
       PrintHouseConf.Report
       PrintHouseConf.FieldName
       PrintHouseConf.KeyValue
       PrintHouseConf.FromDate
       PrintHouseConf.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bConf FOR PrintHouseConf.
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP PrintHouseConf.Brand
           PrintHouseConf.PrintHouse
           PrintHouseConf.Report          
           PrintHouseConf.TableName           
           PrintHouseConf.FieldName
           PrintHouseConf.KeyValue       
           PrintHouseConf.FromDate
           PrintHouseConf.ToDate       
       WITH FRAME lis.

      
      IF NOT NEW PrintHouseConf THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.
         
         IF toimi = 8 THEN LEAVE.
      END.
      
      FIND CURRENT PrintHouseConf EXCLUSIVE-LOCK.
      
      UPDATE
           PrintHouseConf.KeyValue       
           PrintHouseConf.FromDate
           PrintHouseConf.ToDate       
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.
 
      IF CAN-FIND(FIRST bConf WHERE 
                        bConf.Brand      = gcBrand                   AND
                        bConf.Report     = PrintHouseConf.Report     AND
                        bConf.TableName  = PrintHouseConf.TableName  AND
                        bConf.FieldName  = PrintHouseConf.FieldName  AND
                        bConf.KeyValue   = PrintHouseConf.KeyValue   AND
                        bConf.PrintHouse = PrintHouseConf.PrintHouse AND
                        bConf.ToDate    >= PrintHouseConf.FromDate   AND
                        bConf.FromDate  <= PrintHouseConf.ToDate     AND
                        RECID(bConf) NE RECID(PrintHouseConf))
      THEN DO:
         MESSAGE "A similar configuration row already exists on given period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.

      LEAVE.
   
   END.
   
END PROCEDURE.

