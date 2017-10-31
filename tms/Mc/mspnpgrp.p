/* -----------------------------------------------
  MODULE .......: msPNPGroup
  FUNCTION .....: rating PNPGroup numbers
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 25-02-01
  MODIFIED .....: 17.09.03 jp Brand 
                  15.03.04 tk eventlog
                  13.01.05/aam create fees for new group,
                               delete fees when group is terminated
                  22.02.05/aam disp PNPSeq in detail frame
                  15.04.05/aam only one input parameter, 
                               don't use lookup in find-routines,
                               use GroupType
  VERSION ......: M15
  ------------------------------------------------------ */
{Syst/commali.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPNPGroup AS HANDLE NO-UNDO.
   lhPNPGroup = BUFFER PNPGroup:HANDLE.
   RUN StarEventInitialize(lhPNPGroup).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPNPGroup).
   END.
END.

{Func/remfees.i}

DEF  INPUT PARAMETER   icCLI AS CHAR NO-UNDO.

DEF VAR haku-PNPGroup    LIKE PNPGroup.PNPGroup  NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 1.
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
DEF VAR ldtDate    AS DATE                   NO-UNDO.
DEF VAR lcInfo     AS CHAR                   NO-UNDO. 


DEF BUFFER xxPNPGroup FOR PNPGroup.

form
   PNPGroup.PNPGroup    
   PNPGroup.name      format "x(34)"
   PNPGroup.ccn       format ">>>9" 
      CCN.CCNName     format "x(10)"
   PNPGroup.dFrom
   PNPGroup.dTo
WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(Syst.CUICommon:cfc)
   title color value(Syst.CUICommon:ctc) " " + Syst.CUICommon:ynimi +
   "  " + icCLI + " PNP groups "
   + string(TODAY,"99-99-99") + " "
   FRAME sel.

form
   "Group code :" PNPGroup.PNPGroup              SKIP
   "Group name :" PNPGroup.name                  SKIP
   "Group Seq. :" PNPGroup.PNPSeq FORMAT ">>>>>>>>9" SKIP
   "CCN .......:" PNPGroup.ccn FORMAT ">>>9" 
      CCN.CCNName  SKIP
   "Valid From.:" PNPGroup.dfrom                 SKIP
   "Valid To...:" PNPGroup.dto      
WITH OVERLAY ROW 4 centered
   COLOR value(Syst.CUICommon:cfc)
   TITLE COLOR value(Syst.CUICommon:ctc)
   fr-header WITH no-labels
   FRAME lis.

form /*  search WITH FIELD PNPGroup */
    haku-PNPGroup
    help "Give ...."
    with row 4 col 2 title color value(Syst.CUICommon:ctc) " FIND xxxxxxx "
    COLOR value(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME haku-f1.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE PNPGroup THEN ASSIGN
   memory       = recid(PNPGroup)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by 11111 ".
       if order = 2 then put screen row 19 col 30 " Order by 22222 ".
    END.

   IF must-add THEN DO:  /* PNPGroup -ADD  */
      HIDE FRAME lis.
      assign Syst.CUICommon:cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.

        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
        
        DO TRANSACTION:
           FIND FIRST mobsub where 
                      mobsub.cli = icCLI no-lock no-error.

           FIND FIRST customer where 
                      Customer.CustNum = Mobsub.CustNum NO-LOCK NO-ERROR.
           
           FIND FIRST xxPnpgroup WHERE
                      xxPnpgroup.Brand    = Syst.CUICommon:gcBrand AND 
                      xxPnpgroup.Pnpgroup = "FRIENDS" NO-LOCK NO-ERROR.

           IF AVAIL xxPnpgroup THEN DO:
              CREATE PNPGroup.
              BUFFER-COPY xxpnpgroup EXCEPT xxPnpgroup.PnpSeq                            xxPnpgroup.pnpgroup 
             TO PnpGroup.
              ASSIGN
                 PNPGroup.pnpseq    = NEXT-VALUE(pnpseq)
                 PNPGroup.Brand     = Syst.CUICommon:gcBrand
                 PNPGroup.groupType = 2
                 Pnpgroup.PnpGroup  = icCLI
                 Pnpgroup.dFrom     = today
                 Pnpgroup.dTo       = 12/31/2054.

              FIND FIRST Mobsub WHERE 
                         Mobsub.Cli = icCli NO-LOCK NO-ERROR.
              IF AVAIL Customer THEN 
              FIND FIRST Customer WHERE 
                         Customer.Custnum = Mobsub.CustNum NO-LOCK NO-ERROR. 
              IF Avail Customer THEN 
              ASSIGN   
              PNPGroup.name      = Func.Common:mDispCustName(BUFFER Customer) .
           END.
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
              
              RUN Mc/creasfee.p (MobSub.CustNum,
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
        Syst.CUICommon:ufk[1]= 0  Syst.CUICommon:ufk[2]= 0 Syst.CUICommon:ufk[3]= 0 Syst.CUICommon:ufk[4]= 1764
        Syst.CUICommon:ufk[5]= 5  Syst.CUICommon:ufk[6]= 4 Syst.CUICommon:ufk[7]= 0 Syst.CUICommon:ufk[8]= 8 Syst.CUICommon:ufk[9]= 1
        Syst.CUICommon:ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW PNPGroup.PNPGroup {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(Syst.CUICommon:ccc) PNPGroup.PNPGroup WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.CUICommon:nap = keylabel(LASTKEY).

      if lookup(Syst.CUICommon:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(Syst.CUICommon:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND PNPGroup where recid(PNPGroup) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
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

      ASSIGN Syst.CUICommon:nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
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
      else if lookup(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO
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
      else if lookup(Syst.CUICommon:nap,"prev-page,page-up,-") > 0 THEN DO:
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
     else if lookup(Syst.CUICommon:nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
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

     if lookup(Syst.CUICommon:nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(Syst.CUICommon:nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND PNPGroup where recid(PNPGroup) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(Syst.CUICommon:ctc)
          PNPGroup.PNPGroup 
          PNPGroup.PNPGroup.

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
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(Syst.CUICommon:ccc)
          PNPGroup.PNPGroup 
          PNPGroup.PNPGroup.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPNPGroup).

           DELETE PNPGroup.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST PNPGroup
              WHERE PNPGroup.Brand = Syst.CUICommon:gcBrand ) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     ELSE IF LOOKUP(Syst.CUICommon:nap,"4,F4") > 0 THEN DO:
       FIND FIRST PNPGroup WHERE 
            RECID(PNPGroup) = rtab[FRAME-LINE] 
       NO-LOCK NO-ERROR.
       FIND FIRST PNPGroup WHERE
            RECID(PNPGroup) = rtab[FRAME-LINE]
       NO-LOCK NO-ERROR.
                        
       IF PNPGroup.GroupType = 0 THEN
          RUN Mc/pnplist.p(PNPGroup.pnpSeq).
        ELSE  RUN Mc/matepnplist.p(PNPGroup.pnpseq,PNPGroup.PNPGroup).

       ufkey = true.
       RUN Syst/ufkey.p.
       PAUSE 0.
     END.

     else if lookup(Syst.CUICommon:nap,"7,f7") > 0 THEN DO:  /* hinnasto */
     END.

     else if lookup(Syst.CUICommon:nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST PNPGroup where 
            recid(PNPGroup) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE Syst.CUICommon:ehto = 9.
       RUN Syst/ufkey.p.

       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPNPGroup).

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPNPGroup).

       xrecid = recid(PNPGroup).

     END.

     else if lookup(Syst.CUICommon:nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(PNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(Syst.CUICommon:nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(PNPGroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(Syst.CUICommon:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 

   FIND FIRST CCN WHERE
              CCN.Brand  = Syst.CUICommon:gcBrand AND 
              CCN.CCN    = PNPGroup.ccn
   NO-LOCK NO-ERROR.

   DISPLAY
      PNPGroup.PNPGroup  
      PNPGroup.name
      PNPGroup.ccn     CCN.CCNName
      PNPGroup.dFrom
      PNPGroup.dTo
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT PNPGroup WHERE
                PNPGroup.Brand     = Syst.CUICommon:gcBrand AND
                PNPGroup.GroupType = 2       AND
                PNPGroup.PNPGroup  = icCLi
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV PNPGroup WHERE
                PNPGroup.Brand     = Syst.CUICommon:gcBrand AND
                PNPGroup.GroupType = 2       AND
                PNPGroup.PNPGroup  = icCLi
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.
   IF order = 1 THEN 
      FIND FIRST PNPGroup WHERE
                 PNPGroup.Brand     = Syst.CUICommon:gcBrand AND
                 PNPGroup.GroupType = 2       AND
                 PNPGroup.PNPGroup  = icCLi
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST PNPGroup WHERE
                PNPGroup.Brand     = Syst.CUICommon:gcBrand AND
                PNPGroup.GroupType = 2       AND
                PNPGroup.PNPGroup  = icCLi
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.

   DEF VAR ldtChgDate AS DATE NO-UNDO. 
   DEF VAR ldAmt      AS DEC  NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO. 


   IF NOT bNew THEN DO:

      FIND FIRST CCN WHERE
                 CCN.Brand = Syst.CUICommon:gcBrand AND 
                 CCN.CCN   = PNPGroup.ccn
      NO-LOCK NO-ERROR.

      DISP 
         PNPGroup.PNPGroup 
         BillItem.BIName   WHEN AVAIL BillItem
         CCN.CCNName       WHEN AVAIL CCN
      WITH FRAME lis.

   END.

   DISP PNPGroup.PNPSeq pnpgroup.ccn pnpgroup.name WITH FRAME lis.

   ldtChgDate = PNPGroup.dTo.
   
   UPDATE 
      PNPGroup.PNPGroup WHEN bNew
      PNPGroup.dfrom
      PNPGroup.dto
   WITH FRAME lis EDITING: 

      READKEY. 
      Syst.CUICommon:nap = KEYLABEL(LASTKEY). 
      IF lookup(Syst.CUICommon:nap,Syst.CUICommon:poisnap) > 0 THEN DO:
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
                              xxPNPGroup.Brand    = Syst.CUICommon:gcBrand                 AND
                              xxPNPGroup.PNPGroup = input PNPGroup.PNPGroup AND
                              xxPNPGroup.dto      = input PNPGroup.dto      AND
                              RECID(xxPNPGroup)   ne RECID(PNPGroup))
            THEN DO:
               BELL.
               MESSAGE "PNP Group allready exist!".
               NEXT-PROMPT PNPGroup.dto.
               NEXT.               
            END.                  
         
            FOR EACH PnpList OF PnpGroup WHERE 
                     PnpList.toDate > INPUT pnpgroup.dto EXCLUSIVE-LOCK.
                ASSIGN
                     pnplist.toDate = INPUT Pnpgroup.DTo.
                     
            ENd.         
         END.

      END.

      APPLY LASTKEY. 

   END.
   
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
                          TRUE,  /* credit singlefee for billed items */
                          katun,
                          "",
                          OUTPUT ldAmt,
                          OUTPUT liCnt).
      END.
         
   END.
    
   HIDE FRAME lis.

END.

