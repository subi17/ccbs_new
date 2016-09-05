/*----------------------------------------------------------------------
  MODULE .......: RZItem
  TASK .........: Updates table RZItem
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 03-07-01
  CHANGED ......: 19.12.02 tk eventlog
                  21.06.07 vk changed this module so, that it can be used
                              with an empty input field
  VERSION ......: SCRUNKO4 (10.06.99)
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Syst/eventval.i}

DEF INPUT PARAMETER     RZItem-code AS CHAR NO-UNDO.

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}
    
    DEF VAR lhRZItem AS HANDLE NO-UNDO.
    lhRZItem = BUFFER RZItem:HANDLE.
    RUN StarEventInitialize(lhRZItem).
    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhRZItem).
    END.
END.
    
def /* new */ shared var siirto AS char.


DEF VAR comm-name    like PLMN.commName       NO-UNDO.
DEF VAR lcCountryPrefix      AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 3.
DEF VAR FrmDown      AS int                    NO-UNDO  init 12.
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
DEF VAR llEmpty      AS LOGI                   NO-UNDO.  
DEF VAR liWidth      AS INT                    NO-UNDO.
DEF VAR lcDialTypeName AS CHAR  FORMAT "X(20)"               NO-UNDO.


IF RZItem-Code ne "" THEN liWidth = 78.
                       ELSE liWidth = 60.
form
    RZItem.PLMNCode     column-label "PLMN"
    RZItem.CountryPrefix
    BDest.BDName   FORMAT "X(17)" 
    RZItem.RoamZone      
    RoamZone.RZname  FORMAT "X(24)" 
    RZItem.DialType  COLUMN-LABEL "DT"  FORMAT ">9"
WITH ROW FrmRow WIDTH 76 CENTERED overlay FrmDown  down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  RZItem MENU " 
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    RZItem.PLMNCode     /* label format */
    RZItem.CountryPrefix
    BDest.BDName
    RZItem.RoamZone
    RoamZone.RZName FORMAT "X(35)" 
    RZItem.DialType 
    lcDialTypeName  LABEL "DT Name"
                
            /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek  RZItem-code */
    RZItem-code
    HELP "Enter Code of RZItem "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  CountryPrefix */
    lcCountryPrefix
    HELP "Enter CountryPrefix"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CountryPrefix "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

 

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


IF RZItem-Code ne "" THEN DO:
    llEmpty = FALSE.
    FIND FIRST RZItem WHERE RZItem.PLMNCode = RZItem-Code 
        NO-LOCK NO-ERROR.
    IF AVAIL RZItem THEN lcCountryPrefix = RZItem.CountryPrefix.
                      ELSE lcCountryPrefix = "".
END.
ELSE DO:
    llEmpty = TRUE.
    FIND FIRST RZItem USE-INDEX CountryPrefix NO-LOCK NO-ERROR.
    IF AVAIL RZItem THEN lcCountryPrefix = RZItem.CountryPrefix.
                      ELSE lcCountryPrefix = "".
    FIND FIRST RZItem  NO-LOCK NO-ERROR.
END.

IF AVAILABLE RZItem THEN ASSIGN
   memory       = recid(RZItem)
   must-print   = true
   must-add     = false.
ELSE IF RZItem-Code ne ""  AND 
        NOT AVAIL RZItem THEN DO:
   MESSAGE 
   "Do You Want to add new PLMN Rate for " RZItem-Code "?"
   VIEW-AS ALERT-BOX  BUTTONS YES-NO UPDATE ok.

   IF ok = FALSE THEN LEAVE.
   ELSE 
   ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.
   
END.
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

   IF must-add THEN DO:  /* Add a RZItem  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PAUSE 0.
           PROMPT-FOR RZItem.PLMNCode
           validate
              (RZItem.PLMNCode NOT ENTERED or
              NOT CAN-FIND(RZItem using  RZItem.PLMNCode),
              "RZItem Code " + string(INPUT RZItem.PLMNCode) +
              " already exists !").
           IF INPUT FRAME lis RZItem.PLMNCode = "" THEN
               LEAVE add-row.
           RZItem-code = INPUT FRAME lis RZItem.PLMNCode.
           FIND FIRST plmn WHERE 
               plmn.PLMN = INPUT FRAME lis RZItem.PLMNCode 
               NO-LOCK NO-ERROR.
           IF NOT AVAIL plmn THEN DO:
               MESSAGE "The given PLMNCode does not exist in PLMN table."
                   VIEW-AS ALERT-BOX.
               NEXT.
           END.     
           create RZItem.
           ASSIGN
           RZItem.PLMNCode = RZItem-code.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRZItem).
           
           ASSIGN
           memory = recid(RZItem)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      IF RZItem-code ne "" THEN FIND FIRST RZItem
      WHERE RZItem.PLMNCode = RZItem-Code NO-LOCK NO-ERROR.
      ELSE  FIND FIRST RZItem  NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE RZItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND RZItem WHERE recid(RZItem) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delr */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RZItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(RZItem).
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
        ufk[3]= 0  ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        IF llEmpty THEN ASSIGN ufk[1]= 95
                               ufk[2]= 96.
                   ELSE ASSIGN ufk[1]= 0
                               UFK[2]= 0.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row RZItem.PLMNCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RZItem.PLMNCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row RZItem.CountryPrefix {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RZItem.CountryPrefix WITH FRAME sel.
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
        FIND RZItem WHERE recid(RZItem) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE RZItem THEN
              ASSIGN FIRSTrow = i memory = recid(RZItem).
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
           IF NOT AVAILABLE RZItem THEN DO:
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
                rtab[1] = recid(RZItem)
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
           IF NOT AVAILABLE RZItem THEN DO:
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
              rtab[FRAME-down] = recid(RZItem).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND RZItem WHERE recid(RZItem) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RZItem THEN DO:
           memory = recid(RZItem).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE RZItem THEN memory = recid(RZItem).
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
           FIND RZItem WHERE recid(RZItem) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF llEmpty AND
     LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f1.
       SET RZItem-code WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF RZItem-code ENTERED THEN DO:
          FIND FIRST RZItem WHERE RZItem.PLMNCode = RZItem-code
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE RZItem THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some RZItem/RZItem-code was found */
          ASSIGN order = 1 memory = recid(RZItem) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF llEmpty AND
     LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME F2.
       SET lcCountryPrefix WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF lcCountryPrefix ENTERED THEN DO:
          FIND FIRST RZItem USE-INDEX CountryPrefix WHERE RZItem.CountryPrefix >= lcCountryPrefix
              NO-LOCK NO-ERROR.
          IF NOT AVAILABLE RZItem THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some RZItem/CountryPrefix was found */
          ASSIGN order = 2 memory = recid(RZItem) must-print = true.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Update Memo */

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
       RZItem.PLMNCode RZItem.CountryPrefix .

       RUN local-find-NEXT.
       IF AVAILABLE RZItem THEN memory = recid(RZItem).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE RZItem THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(RZItem).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RZItem.PLMNCode RZItem.CountryPrefix .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRZItem).
           
           DELETE RZItem.
           /* was LAST record DELETEd ? */
           IF NOT llEmpty AND NOT CAN-FIND(FIRST RZItem
           WHERE RZItem.PLMNCode = RZItem-Code) THEN DO:
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
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY RZItem.PLMNCode.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRZItem).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRZItem).
       
       RUN local-disp-row.
       xrecid = recid(RZItem).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(RZItem) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(RZItem) must-print = true.
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
      find RZItem WHERE recid(RZItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find RZItem WHERE recid(RZItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF NOT llEmpty THEN DO:
           IF order = 1 THEN FIND FIRST RZItem USE-INDEX PLMNCode WHERE
               RZItem.PLMNCode = RZItem-Code NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND FIRST RZItem USE-INDEX CountryPrefix WHERE
               RZItem.CountryPrefix = lcCountryPrefix NO-LOCK NO-ERROR.
       END.
       ELSE DO:
           IF order = 1 THEN FIND FIRST RZItem USE-INDEX PLMNCode
               NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND FIRST RZItem USE-INDEX CountryPrefix 
               NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF NOT llEmpty THEN DO:
           IF order = 1 THEN FIND LAST RZItem USE-INDEX PLMNCode WHERE
               RZItem.PLMNCode = RZItem-Code NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND LAST RZItem USE-INDEX CountryPrefix WHERE
               RZItem.CountryPrefix = lcCountryPrefix NO-LOCK NO-ERROR.
       END.
       ELSE DO:
           IF order = 1 THEN FIND LAST RZItem USE-INDEX PLMNCode
               NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND LAST RZItem USE-INDEX CountryPrefix 
               NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF NOT llEmpty THEN DO:
           IF order = 1 THEN FIND NEXT RZItem USE-INDEX PLMNCode
           WHERE RZItem.PLMNCode = RZItem-Code NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND NEXT RZItem USE-INDEX CountryPrefix
           WHERE RZItem.PLMNCode = lcCountryPrefix NO-LOCK NO-ERROR.
       END.
       ELSE DO:
           IF order = 1 THEN FIND NEXT RZItem USE-INDEX PLMNCode
               NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND NEXT RZItem USE-INDEX CountryPrefix
               NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF NOT llEmpty THEN DO:
           IF order = 1 THEN FIND PREV RZItem USE-INDEX PLMNCode
           WHERE RZItem.PLMNCode = RZItem-Code NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND PREV RZItem USE-INDEX CountryPrefix
           WHERE RZItem.PLMNCode = lcCountryPrefix NO-LOCK NO-ERROR.
       END.
       ELSE DO:
           IF order = 1 THEN FIND PREV RZItem USE-INDEX PLMNCode
               NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND PREV RZItem USE-INDEX CountryPrefix
               NO-LOCK NO-ERROR.
       END.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       RZItem.PLMNCode 
       RZItem.CountryPrefix
       RZItem.RoamZone
       RZItem.DialType
       RoamZone.RZName WHEN AVAIL RoamZone
       BDest.BDName  WHEN Avail BDest           
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST RoamZone WHERE 
              RoamZone.RoamZone = RZItem.RoamZone NO-LOCK NO-ERROR.
   
   FIND FIRST BDest WHERE 
              BDest.Brand = gcBrand AND 
              BDest.BDest = RZitem.CountryPrefix AND
              BDest.DestType = 1 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BDest THEN
   FIND FIRST BDest WHERE 
              BDest.Brand = gcBrand AND 
              BDest.BDest = RZitem.CountryPrefix NO-LOCK NO-ERROR.
   
   FIND FIRST TMSCodes WHERE
              TMSCodes.TableName = "RateBSub" AND
              TMSCodes.FieldName = "AsubType" AND
              TMSCodes.CodeGroup = "asubtype" AND
              TMSCodes.CodeValue = STRING(rzItem.Dialtype)
   NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcDialTypeName = TMSCodes.CodeName.
   ELSE lcDialTypeName = "unknown".

END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      lcDialTypeName
      BDest.BDName WHEN AVAIL bdest 
      RoamZone.RZname WHEN AVAIL roamzone
      WITH FRAME lis.
      UPDATE
          RZItem.CountryPrefix
          RZItem.RoamZone
          RZItem.DialType
                      

      WITH FRAME lis
      EDITING:
             READKEY.
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "RoamZone" THEN DO:
                   FIND FIRST RoamZone WHERE RoamZone.RoamZone = 
                   INPUT FRAME lis RZItem.ROAMZoNE NO-LOCK NO-ERROR.
                   IF NOT AVAIL RoamZone THEN DO:
                      BELL.
                      MESSAGE "Unknown RoamZone !".
                      NEXT.
                   END.
                   DISP RoamZone.RZName.
                END.
                ELSE IF FRAME-FIELD = "CountryPrefix" THEN DO:
                   FIND FIRST BDest WHERE 
                        BDest.Brand = gcBrand AND
                        BDest.BDest = 
                   INPUT FRAME lis RZItem.CountryPrefix AND
                        BDest.DestType = 1
                   NO-LOCK NO-ERROR.
                   IF NOT AVAILABLE BDest THEN 
                   FIND FIRST BDest WHERE 
                        BDest.Brand = gcBrand AND
                        BDest.BDest = 
                   INPUT FRAME lis RZItem.CountryPrefix
                   NO-LOCK NO-ERROR.

                   IF NOT AVAIL BDest THEN DO:
                      BELL.
                      MESSAGE "Unknown Country Prefix !".
                   END.
                   DISP BDest.BDName WHEN AVAIL bdest.
                END.

             END.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.
