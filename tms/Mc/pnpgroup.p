/* -----------------------------------------------
  MODULE .......: PNPGroup
  FUNCTION .....: Maintain rating PNPGroup numbers
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 25-02-01
  MODIFIED .....: 06.03.03 tk tokens
                  20.03.03/aam one parameter added for tariff.p
                  27.03.03 kl BDest not used anymore
                  31.03.03 tk added order by name
                  04.04.03 kl run tariff, new parameter
                  26.06.03 kl run tariff, new parameter
                  04.07.03 kl run tariff, new parameter
                  16.09.03 jp Brand
                  15.03.04 tk eventlog
                  13.01.05/aam create fees for new group,
                               delete fees when group is terminated
                  08.02.05 jp  f1 repaired           
                  14.03.05/mvi pnpgroup.pnpgroup FORMAT "X(11)"
                               Coppa:Task 6351, samalla
  VERSION ......: M15
  ------------------------------------------------------ */
&GLOBAL-DEFINE BrTable PNPGroup

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PNPGroup'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPNPGroup AS HANDLE NO-UNDO.
   lhPNPGroup = BUFFER PNPGroup:HANDLE.
   RUN StarEventInitialize(lhPNPGroup).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhPNPGroup).
   END.
END.

{Func/remfees.i}

DEF VAR haku-PNPGroup    LIKE PNPGroup.PNPGroup FORMAT "X(11)" NO-UNDO.
DEF VAR haku-name        LIKE PNPGroup.Name      NO-UNDO.    
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 3.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR lcGroupType AS CHAR                  NO-UNDO FORMAT "x(12)" .
DEF NEW SHARED VAR siirto AS CHAR            NO-UNDO.
DEF VAR llFind     AS  LOG                   NO-UNDO.
DEF VAR new_pnpgroup AS LOG                  NO-UNDO INIT FALSE.
DEF VAR ldtDate    AS DATE                   NO-UNDO.
DEF VAR lcInfo     AS CHAR                   NO-UNDO. 

DEF BUFFER xxPNPGroup FOR PNPGroup.

form
   PNPGroup.Brand   FORMAT "X(4)"  COLUMN-LABEL "BRND"
   LCGroupType      FORMAT "X(5)" label "Type"
   PNPGroup.PNPGroup FORMAT "X(11)"  
   PNPGroup.Name   FORMAT "X(14)"
   PNPGroup.RateCCN COLUMN-LABEL "RateCCN"
   PNPGroup.CCN
      CCN.CCNName  FORMAT "X(10)"
   PNPGroup.dFrom
   PNPGroup.dto
WITH width 80 OVERLAY scroll 1 15 DOWN ROW 1
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain PNP groups "
   + string(pvm,"99-99-99") + " "
FRAME sel.

{Func/brand.i}

form
   "Group code :" PNPGroup.PNPGroup FORMAT "X(11)"  SKIP
   "Group Type :" PNPGroup.GroupType lcGroupType    SKIP 
   "Group name :" PNPGroup.Name      FORMAT "X(40)" SKIP
   "Rate CCN...:" PnpGroup.RateCCN                  SKIP
   "Report CCN.:" PNPGroup.ccn 
      HELP "Report CCN"
      CCN.CCNName                    SKIP
   "Valid From.:" PNPGroup.dfrom     SKIP
   "Valid To...:" PNPGroup.dto      
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH no-labels
FRAME lis.

form /*  search WITH FIELD PNPGroup */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
   "PnPGroup..:" haku-PNPGroup                          
   help "Give ...."
with row 4 col 2 title color value(ctc) " FIND PNPGroup "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form /*  search WITH FIELD Name */
   "Brand Code:" lcBrand  HELP "Enter Brand"
   VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
   "PnPname..:" haku-Name
   help "Give ...."
with row 4 col 2 title color value(ctc) " FIND NAME "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f2.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE PNPGroup THEN ASSIGN
   memory     = recid(PNPGroup)
   must-print = TRUE
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No PNPGroups available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory     = ?
      must-print = FALSE
      must-add   = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by Group Code ".
       if order = 2 then put screen row 19 col 30 " Order by Group NAme ".
       if order = 3 then put screen row 19 col 30 " Order by CCN number ".
    END.

   IF must-add THEN DO:  /* PNPGroup -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSACTION:

           
           CREATE PNPGroup.
           ASSIGN
           PNPGroup.pnpseq = NEXT-VALUE(pnpseq)
           PnpGroup.Brand  = gcBrand .
           
           new_pnpgroup  = TRUE .
           
           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           PNPGroup.PNPGroup = "" THEN
           UNDO add-new, LEAVE add-new.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPNPGroup).
           
           /* create fees */
           FIND MobSub WHERE MobSub.CLI = PNPGroup.PNPGroup NO-LOCK NO-ERROR.
           IF AVAILABLE MobSub THEN DO:
           
              ldtDate = PNPGroup.dFrom.
              
              /* service is always billed from the beginning of month */
              IF DAY(ldtDate) NE 1 
              THEN ldtDate = DATE(MONTH(ldtDate),1,YEAR(ldtDate)).
              
              RUN creasfee (MobSub.CustNum,
                            MobSub.MsSeq,
                            ldtDate,
                            "PNP",
                            "cc_Open",
                            1,
                            ?,
                            "",
                            TRUE,
                            katun,
                            "",
                            0,
                            "",
                            "",
                            OUTPUT lcInfo).    
           END.
           
           ASSIGN
              memory = recid(PNPGroup)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE PNPGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND PNPGroup where recid(PNPGroup) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE PNPGroup THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(PNPGroup).
              RUN LOCAL-FIND-NEXT.
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
               must-print = FALSE
               .
        PAUSE 0 no-message.

        /* one page of data has been printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
           ufk[1] = 35
           ufk[2] = 30
           ufk[3] = 0
           ufk[4] = 1764
           ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)
           ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
           ufk[7] = 1765
           ufk[8] = 8
           ufk[9] = 1
           ehto   = 3
           ufkey  = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW PNPGroup.PNPGroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PNPGroup.PNPGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PNPGroup.Name ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PNPGroup.Name WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW PNPGroup.CCN ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PNPGroup.PNPGroup WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN
           firstline = 0
           memory    = rtab[FRAME-LINE].
        FIND PNPGroup where recid(PNPGroup) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
           IF AVAILABLE PNPGroup THEN ASSIGN
              firstline = i
              memory    = recid(PNPGroup).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.
      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND PNPGroup where recid(PNPGroup) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE PNPGroup THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              RUN LOCAL-DISP-ROW.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(PNPGroup)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND PNPGroup where recid(PNPGroup) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE PNPGroup THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              RUN LOCAL-DISP-ROW.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(PNPGroup).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND PNPGroup where recid(PNPGroup) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE PNPGroup THEN DO:
           memory = recid(PNPGroup).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE PNPGroup THEN memory = recid(PNPGroup).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND PNPGroup where recid(PNPGroup) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       haku-PNPGroup = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       Disp lcBrand With FRAME haku-f1.
       UPDATE lcBrand 
              haku-PNPGroup WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if haku-PNPGroup <> "" THEN DO:
          FIND FIRST PNPGroup where 
                     PNPGroup.PNPGroup >= haku-PNPGroup AND 
                     Pnpgroup.grouptype = 2             AND  
                     PnpGroup.Brand     = lcBrand 
          no-lock no-error.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     /* Haku 1 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       haku-Name = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       Disp lcBrand With FRAME haku-f2.

       UPDATE  lcBrand WHEN gcAllBrand = TRUE  
               haku-Name WITH FRAME haku-f2.
       HIDE FRAME haku-f2 no-pause.
       if haku-Name <> "" THEN DO:
          FIND FIRST PNPGroup where 
                     PNPGroup.Brand = lcBrand AND 
                     PNPGroup.Name >= haku-Name
          use-index name no-lock no-error.

          IF NOT  fRecFound(2) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND PNPGroup where recid(PNPGroup) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          PNPGroup.PNPGroup 
          lcGroupType
          PNPGroup.Name
          PNPGroup.CCN
          CCN.CCNName
          PNPGroup.dFrom
          PNPGroup.dTo
          PNPGroup.Brand 
          .

       RUN LOCAL-FIND-NEXT.

       IF AVAILABLE PNPGroup THEN memory = recid(PNPGroup).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND PNPGroup where recid(PNPGroup) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE PNPGroup THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(PNPGroup).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND PNPGroup where recid(PNPGroup) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.

       MESSAGE
          "ARE YOU SURE YOU WANT TO REMOVE GROUP AND ALL ITEMS (Y/N) ? "
       UPDATE ok.

       COLOR DISPLAY value(ccc)
          PNPGroup.PNPGroup 
          PNPGroup.Name
          PNPGroup.CCN
          CCN.CCNName
          PNPGroup.dFrom
          PNPGroup.dTo.

       IF ok THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPNPGroup).

           DELETE PNPGroup.

           FOR EACH PNPList OF PNPGroup EXCLUSIVE-LOCK:
              DELETE PNPList.
           END.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST PNPGroup WHERE 
                                 PnpGroup.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
       FIND FIRST PNPGroup WHERE 
            RECID(PNPGroup) = rtab[FRAME-LINE] 
       NO-LOCK NO-ERROR.

       IF PnpGroup.GroupType = 0 THEN 
          RUN pnplist(PNPGroup.pnpSeq).
       ELSE  run matepnplist.p(pnpgroup.pnpseq,pnpgroup.pnpgroup). 
       
       ufkey = true.
       PAUSE 0.
       NEXT loop.
     END.

     else if lookup(nap,"7,f7") > 0 THEN DO:  /* hinnasto */

        FIND FIRST PNPGroup WHERE 
             RECID(PNPGroup) = rtab[FRAME-LINE] 
        NO-LOCK NO-ERROR.

        ufkey = TRUE.
        RUN tariff(2,PNPGroup.CCN,"",0,"",0).

     END.

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST PNPGroup where 
            recid(PNPGroup) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.

       cfc = "lis". RUN ufcolor.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPNPGroup).

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPNPGroup).

       xrecid = recid(PNPGroup).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(PNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(PNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 

    FIND FIRST TMSCodes WHERE
               TMSCodes.TableName = "PNPGroup" AND
               TMSCodes.FieldName = "GroupType" AND
               TMSCodes.CodeGroup = "GroupType" AND
               TMSCodes.CodeValue = STRING(Pnpgroup.GroupType)
    NO-LOCK NO-ERROR.
                                          
   IF AVAIL TMSCodes THEN lcGroupType = TMSCodes.CodeName.
   ELSE lcGroupType = "".
          


   FIND FIRST CCN WHERE
              CCN.Brand = gcBrand AND 
              CCN.CCN   = PNPGroup.ccn
   NO-LOCK NO-ERROR.

   DISPLAY
      PNPGRoup.Brand 
      LCGroupType
      PNPGroup.PNPGroup  
      PNPGroup.Name
      PNPGroup.RateCCN
      PNPGroup.CCN     
      CCN.CCNName WHEN AVAILABLE CCN
      PNPGroup.dFrom
      PNPGroup.dTo
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT PNPGroup  USE-INDEX pnpgroup WHERE 
      pnpgroup.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND NEXT PNPGroup USE-INDEX Name  WHERE pnpgroup.Brand = lcBrand 
      NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND NEXT PNPGroup USE-INDEX CCN  WHERE pnpgroup.Brand = lcBrand 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV PNPGroup USE-INDEX pnpgroup WHERE 
                pnpgroup.Brand = lcBrand no-lock no-error.
   ELSE IF order = 2 THEN 
      FIND PREV PNPGroup USE-INDEX Name NO-LOCK   WHERE 
                pnpgroup.Brand = lcBrand no-error.
   ELSE IF order = 3 THEN 
      FIND PREV PNPGroup USE-INDEX CCN NO-LOCK   WHERE 
                pnpgroup.Brand = lcBrand no-error.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 THEN 
      FIND FIRST PNPGroup NO-LOCK USE-INDEX pnpgroup WHERE 
      pnpgroup.Brand = lcBrand no-error.
   ELSE IF order = 2 THEN 
      FIND FIRST PNPGroup USE-INDEX Name NO-LOCK  WHERE 
      pnpgroup.Brand = lcBrand no-error.
   ELSE IF order = 3 THEN 
      FIND FIRST PNPGroup USE-INDEX CCN NO-LOCK   WHERE 
                 pnpgroup.Brand = lcBrand no-error .

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST PNPGroup NO-LOCK USE-INDEX pnpgroup  WHERE 
      pnpgroup.Brand = lcBrand no-error.
   ELSE IF order = 2 THEN 
      FIND LAST PNPGroup USE-INDEX Name NO-LOCK   WHERE 
                pnpgroup.Brand = lcBrand  NO-ERROR.
   ELSE IF order = 3 THEN 
      FIND LAST PNPGroup USE-INDEX CCN NO-LOCK  WHERE 
                pnpgroup.Brand = lcBrand NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.
   
   DEF VAR ldtChgDate AS DATE NO-UNDO. 
   DEF VAR ldAmt      AS DEC  NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO. 

   IF NOT bNew THEN DO:

    FIND FIRST TMSCodes WHERE
               TMSCodes.TableName = "PNPGroup" AND
               TMSCodes.FieldName = "GroupType" AND
               TMSCodes.CodeGroup = "GroupType" AND
               TMSCodes.CodeValue = STRING(Pnpgroup.GroupType)
    NO-LOCK NO-ERROR.
                    
    IF AVAIL TMSCodes THEN lcGroupType = TMSCodes.CodeName.
    ELSE lcGroupType = "".

    FIND FIRST CCN WHERE
               CCN.Brand = lcBrand AND 
               CCN.CCN = PNPGroup.ccn
    NO-LOCK NO-ERROR.

    DISP 
         PNPGroup.PNPGroup 
         PnpGroup.GroupType
         lcGroupType
         PnpGroup.Rateccn
         CCN.CCNName       WHEN AVAIL CCN
         PNPGroup.Name 
         PNPGroup.CCN 
         PNPGroup.dFrom
         PNPGroup.dTo
      WITH FRAME lis.

   END.

   IF lcRight = "RW" THEN DO:

      ldtChgDate = PNPGroup.dTo.
      
      UPDATE 
         PNPGroup.PNPGroup WHEN bNew
         PnpGroup.GroupType 
         PNPGroup.Name 
         PnpGroup.RateCCN 
         PNPGroup.CCN 
         PNPGroup.dFrom
         PNPGroup.dTo
      WITH FRAME lis EDITING: 

         READKEY. 

         IF FRAME-FIELD = "GroupType" AND
            keylabel(lastkey) = "F9" THEN DO:

            RUN h-tmscodes(INPUT "PnpGroup",  /* TableName*/
                                 "GroupType", /* FieldName */
                                 "GroupType", /* GroupCode */
                                 OUTPUT siirto).
            ASSIGN
            PnpGroup.GroupType = INT(siirto) WHEN siirto NE ?.
            disp PnpGroup.GroupType with frame lis.
            NEXT.
         END.
                                                 
         nap = KEYLABEL(LASTKEY). 
         IF lookup(nap,poisnap) > 0 THEN DO:
            if keylabel(lastkey) = "F4" THEN LEAVE . 

            IF FRAME-FIELD = "PNPGroup" THEN DO:
               if input frame lis PNPGroup.PNPGroup  = "" THEN DO:
                  LEAVE .
               END.
            END.

            ELSE IF FRAME-FIELD = "dfrom" THEN DO:
               if input frame lis PNPGroup.dfrom  = ? THEN DO:
                  BELL.
                  MESSAGE 
                  "from Date can't be empty!".
                  NEXT-prompt dfrom.
                  NEXT.
               END.
            END.

            ELSE IF FRAME-FIELD = "dto" THEN DO:
               if input frame lis PNPGroup.dto  = ? THEN DO:
                  BELL.
                  MESSAGE 
                  "To Date can't be empty!".
                  NEXT-prompt dto.
                  NEXT.
               END.

               IF CAN-FIND(FIRST xxPNPGroup WHERE
                                 xxPnpGroup.Brand    = lcBrand AND 
                                 xxPNPGroup.PNPGroup = 
                                 input PNPGroup.PNPGroup AND   
                                 xxPNPGroup.dto      = input PNPGroup.dto AND
                                 RECID(xxPNPGroup)   ne RECID(PNPGroup))
               THEN DO:
                  BELL.
                  MESSAGE "PNP Group allready exist!".
                  NEXT-PROMPT PNPGroup.dto.
                  NEXT.               
               END.                  
            END.

            ELSE IF FRAME-FIELD = "rateccn" THEN DO:

               FIND FIRST CCN WHERE 
                          CCN.Brand   = gcBrand AND 
                          Ccn.Ccn     = input frame lis PNPGroup.Rateccn 
               NO-LOCK NO-ERROR.           
               IF NOT AVAIL CCN THEN DO:
                  BELL.
                  MESSAGE
                     "Unknown Rate CCN".
                  NEXT-prompt pnpgroup.Rateccn.
                  NEXT.
               END.
            END.
                                                                 
            ELSE IF FRAME-FIELD = "GroupType" THEN DO:
               RUN v-tmscodes(INPUT "PnpGroup",    /* TableName */
                                    "GroupType", /* FieldName */
                                    "GroupType",     /* GroupCode */
                              INPUT INPUT PnpGroup.GroupType,
                                     OUTPUT llFind).
               IF NOT llFind THEN  DO:
                  NEXT-PROMPT PnpGroup.Grouptype.
                  NEXT.
               END.
                                          
               FIND FIRST TMSCodes WHERE
                          TMSCodes.TableName = "PnpGroup"  AND
                          TMSCodes.FieldName = "GroupType" AND
                          TMSCodes.CodeGroup = "GroupType" AND
                          TMSCodes.CodeValue = STRING(INPUT
                                              FRAME lis Pnpgroup.GroupType)
               NO-LOCK NO-ERROR.

               IF AVAIL TMSCodes THEN lcGroupType = TMSCodes.CodeName.
               ELSE lcGroupType = "unknown".
                                       
               DISP lcGroupType  WITH FRAME lis.
 
            END.

         END.

         APPLY LASTKEY. 

      END.
   
      IF new_pnpgroup AND
         llDoEvent THEN RUN StarEventMakeCreateEvent(lhPnpGroup).
   
      /* group terminated (or end date has atleast been moved) */
      IF ldtChgDate NE ? AND NOT bNew AND ldtChgDate > PNPGroup.DTo
      THEN DO:
      
         /* if subscription has been closed then fees have already been 
            handled */
         FIND MobSub WHERE MobSub.CLI = PNPGroup.PNPGroup NO-LOCK NO-ERROR.
         IF AVAILABLE MobSub THEN FIND Customer OF MobSub NO-LOCK.
         
         IF AVAILABLE MobSub AND AVAILABLE Customer THEN DO:
         
            ldtChgDate = PNPGroup.DTo.

            /* pnp service is always billed to the end of change month */
            IF DAY(ldtChgDate) NE 1 THEN DO:
               IF MONTH(ldtChgDate) = 12
               THEN ldtChgDate = DATE(1,1,YEAR(ldtChgDate) + 1).
               ELSE ldtChgDate = DATE(MONTH(ldtChgDate) + 1,1,
                                      YEAR(ldtChgDate)).
            END.
               
            /* remove fees concerning pnp */
            RUN pDelFixedFee("",
                             "PNPAVAUS",
                             ldtChgDate,
                             ?,
                             TRUE,  /* clean credit fees also */
                             FALSE,  /* credit singlefee for billed items */
                             katun,
                             "",
                             OUTPUT ldAmt,
                             OUTPUT liCnt).
         END.
         
      END.
      
   END.
   
   
   ELSE PAUSE.   

   HIDE FRAME lis.

END.
