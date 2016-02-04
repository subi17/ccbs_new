~/* -----------------------------------------------
  MODULE .......: mservicelimit
  FUNCTION .....: Maintain rating mservicelimit
  APPLICATION ..: TMS
  AUTHOR .......: jp
  CREATED ......: 25-02-01
  MODIFIED .....: 10.06.05/aam quicker find routines
  VERSION ......: SCRUNKO3, (23.10.96)
  ------------------------------------------------------ */

{Syst/commali.i}                    
{Func/timestamp.i}
{Syst/tmsconst.i}
{Mc/lib/tokenlib.i}

DEF  INPUT PARAMETER  iiMSSeq    AS INT            NO-UNDO.
DEF  INPUT PARAMETER  iiDialType AS INT            NO-UNDO.
DEF  INPUT PARAMETER  iiSlSeq    AS INT            NO-UNDO.

{Syst/eventval.i}
IF llDoEvent THEN DO :
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   
   {lib/eventlog.i}
      
   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   DEF VAR lhMServiceLimit AS HANDLE NO-UNDO. 
   lhFixedFee = BUFFER FixedFee:HANDLE.
   RUN StarEventInitialize(lhFixedfee).
   lhMServiceLimit = BUFFER mservicelimit:HANDLE.
   RUN StarEventInitialize(lhMServiceLimit).
                           
END.

DEF VAR haku-servicelimit    LIKE mservicelimit.MSSeq  NO-UNDO.
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
DEF VAR lcValidfrom AS C                     NO-UNDO format "X(20)" .
DEF VAR lcValidto   AS C                     NO-UNDO format "X(20)" .
DEF VAR lccli       AS C                     NO-UNDO FORMAT "x(9)" .
DEF VAR lcunit      AS C                     No-UNDO FORMAT "X(20)".
DEF VAR lctype      AS C                     No-UNDO FORMAT "X(20)".
DEF VAR lcCode      AS CHAR                  NO-UNDO.
DEF VAR lcvalid     AS CHAR                  NO-UNDO FORMAT "X(39)" .
DEF VAR lcGroup     AS CHAR                  NO-UNDO FORMAT "X(8)" .

DEF VAR lii         AS INT                   NO-UNDO.

DEF VAR llAdmin     AS LOG                   NO-UNDO INIT FALSE.

DEF BUFFER xxservicelimit FOR mservicelimit.

form
   lcGroup                    COLUMN-LABEL "ServG"
   lccli                      COLUMN-LABEL "CLI"
   mservicelimit.DialType     COLUMN-LABEL "CT"
   mservicelimit.InclUnit     COLUMN-LABEL "Unit"
   mservicelimit.InclAmt      COLUMN-LABEL "InclAmt" 
   lcvalid                    COLUMN-LABEL "Active Time"

WITH width 80 OVERLAY ROW 1 scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " "   + 
   " Servicelimits "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
   "Mobsub Seq :" mservicelimit.MSSeq                     SKIP
   "CLI .......:" lccli                                   SKIP
   "Call Type .:" mservicelimit.DialType    lctype  no-label      SKIP
   "Incl Unit..:" mservicelimit.InclUnit    lcunit  no-label      SKIP
   "Incl Amt...:" mservicelimit.InclAmt             SKIP
   "Valid From.:" mservicelimit.FromTS   lcvalidfrom    SKIP  
   "Valid To...:" mservicelimit.endTS     lcvalidto      SKIP
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc )
   fr-header WITH no-labels
   FRAME lis.


form
   ServiceLimit.GroupCode    COLON 20
   ServiceLimitGroup.GroupName NO-LABEL SKIP
   ServiceLimit.SLCode     COLON 20 FORMAT "x(16)"
   ServiceLimit.SLName     COLON 20 LABEL "Servicelimit name"
   ServiceLimit.DialType   COLON 20
   lctype  FORMAT  "x(30)" NO-LABEL         SKIP
   ServiceLimit.InclAmt      COLON 20 SKIP
   ServiceLimit.InclUnit     COLON 20
   lcUnit FORMAT "X(30)" NO-LABEL SKIP
   Servicelimit.web          COLON 20
   WITH  OVERLAY ROW 2 centered
    side-label  FRAME servicelimit.

FUNCTION fDispUnit2 RETURNS LOGICAL
   (iiUnit AS INT).
   
     IF iiUnit > 0 THEN
            lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
         "Tariff","DataType",STRING(iiUnit)).
     ELSE lcUnit = "".
    
     DISPLAY lcUnit WITH FRAME servicelimit.
       
     RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")).
          
END FUNCTION.
          



form /*  search WITH FIELD mservicelimit */
    haku-servicelimit
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT).
   IF iiUnit > 0 THEN
   lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
           
   "Servicelimit","InclUnit",STRING(iiUnit)).
   ELSE lcUnit = "".
   DISPLAY lcUnit WITH FRAME lis.
   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")).
END FUNCTION.
                           

IF getTMSRight("VENDOR,SYST") EQ "RW" THEN llAdmin = TRUE.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE mservicelimit THEN ASSIGN
   memory       = recid(mservicelimit)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = TRUE.

   MESSAGE
   "CANNOT FIND ANY SUBSCRIPTION LIMIT"
   VIEW-AS ALERT-BOX .
   LEAVE.
END.
LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by  ".
       if order = 2 then put screen row 19 col 30 " Order by  ".
    END.

   IF must-add THEN DO:  /* mservicelimit -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSACTION:
           LEAVE add-new.
        
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE mservicelimit THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.

        FIND mservicelimit where recid(mservicelimit) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           
           IF AVAILABLE mservicelimit THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(mservicelimit).
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
        ufk[1]= 2612
        ufk[2]= 0
        ufk[3]= 2610 
        ufk[4]= 2603
        ufk[5]= 5  ufk[6]= 4 
        ufk[7]= 0
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.
      


      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW lccli ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) lccli WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW mservicelimit.DialType ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) lccli WITH FRAME sel.
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
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND mservicelimit where recid(mservicelimit) = memory.
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

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND mservicelimit where recid(mservicelimit) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE mservicelimit THEN DO:
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
              rtab[1] = recid(mservicelimit)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-DOWN] 
           no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE mservicelimit THEN DO:
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
              rtab[FRAME-DOWN] = recid(mservicelimit).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND mservicelimit where recid(mservicelimit) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE mservicelimit THEN DO:
           memory = recid(mservicelimit).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE mservicelimit THEN memory = recid(mservicelimit).
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
           FIND mservicelimit where recid(mservicelimit) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     IF lookup(nap,"1,f1") > 0 THEN DO:  /* show callcounters */
        FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE]
        no-lock.
         
        FIND FIRST servicelimit WHERE 
                   ServiceLimit.slseq = mservicelimit.slseq NO-LOCK NO-ERROR.
        FIND FIRST servicelimitGroup WHERE 
                   ServiceLimitGroup.Brand = gcBrand AND
                   servicelimitgroup.groupcode = servicelimit.GroupCode
        NO-LOCK NO-ERROR.           
                   
        FIND dialtype WHERE
             dialtype.dialtype = servicelimit.dialtype NO-LOCK NO-ERROR.
           
        IF AVAIL dialtype THEN lctype = dialtype.dtname.
        ELSE lctype = "".
        PAUSE 0.         
        fDispUnit2(ServiceLimit.InclUnit).
        PAUSE 0.
        DISP
           ServiceLimitGroup.GroupName   WHEN AVAIL ServiceLimitGroup
           ServiceLimit.GroupCode
           ServiceLimit.slcode 
           ServiceLimit.slname
           ServiceLimit.DialType
           ServiceLimit.InclAmt
           ServiceLimit.InclUnit
           Servicelimit.web
           lctype
        WITH FRAME servicelimit.
        MESSAGE "PRESS ENTER" . PAUSE NO-MESSAGE.
       
        HIDE FRAME servicelimit.
        must-print = TRUE.
        NEXT LOOP.
                 
     END.
     
     ELSE IF lookup(nap,"3,f3") > 0 THEN DO:  /* show callcounters */
        FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE]
        no-lock.
        FIND FIRST servicelimit WHERE 
                   ServiceLimit.slseq = mservicelimit.slseq NO-LOCK NO-ERROR.
        FIND FIRST servicelimitGroup WHERE 
                   ServiceLimitGroup.Brand = gcBrand AND
                   servicelimitgroup.groupcode = servicelimit.GroupCode
        NO-LOCK NO-ERROR.

        RUN servicelcounter(INPUT (IF servicelimit.GroupCode BEGINS {&DSS}
                                   THEN 0 ELSE MserviceLimit.MsSeq),
                            INPUT MserviceLimit.CustNum,
                            INPUT MserviceLimit.SlSeq,
                            INPUT ServiceLimitGroup.GroupCode,
                            INPUT (YEAR(Today) * 100 + MONTH(Today)),
                            INPUT MserviceLimit.MSID).

        run ufkey.
        must-print = TRUE.
        NEXT LOOP.
     
     END.
     ELSE if lookup(nap,"4,f4") > 0 THEN DO:  /* Close service limit */
       
       FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE] 
       no-lock.
       
       IF mserviceLimit.endTS < fmakets() THEN DO:
          MESSAGE
          "Service Limit allready closed!"
          VIEW-AS ALERT-BOX.
          NEXT.       
       END.
       
       FIND FIRST ServiceLimit WHERE
                  ServiceLimit.slseq    = mServiceLimit.slseq AND 
                  ServiceLimit.dialtype = mServicelimit.dialtype 
       NO-LOCK NO-ERROR.

       IF avail servicelimit THEN lcgroup = servicelimit.groupcode .
       ELSE DO:
          MESSAGE 
          "Unknown Service limits"                            SKIP
          "Terminate service limits and possible fees manually!"
          VIEW-AS ALERT-BOX.
          NEXT.
       END.
       
       ASSIGN ok = FALSE.
       message 
       "Do you want terminate service limits of Mobile subscription?" 
       VIEW-AS ALERT-BOX BUTTONS YES-NO update ok. 

       IF ok THEN DO TRANS:
          lii  = 0.
          FOR EACH ServiceLimit WHERE 
                   ServiceLimit.GroupCode = lcGroup,
              EACH xxservicelimit WHERE 
                   xxservicelimit.slseq = servicelimit.slseq  AND 
                   xxservicelimit.msseq = mservicelimit.msseq AND 
                   xxservicelimit.endTS > fmakeTS() .

             ASSIGN 
                xxservicelimit.endTS = fmakeTS()
                lii = lii +  1.
          END.
       
          FOR EACH FixedFee WHERE 
                   FixedFee.Brand             = gcBrand                     AND
                   FixedFee.HostTable         = "mobsub"                    AND 
                   Fixedfee.keyvalue          = string(mservicelimit.msseq) AND                   Fixedfee.servicelimitgroup = lcgroup .
                    
             IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFixedFee).
                       
             FOR EACH FFItem OF FixedFee WHERE
                      FFItem.Billed = FALSE AND
                      FFItem.Concerns[1] > YEAR(Today) * 10000 +
                                           MONTH(Today) * 100   +
                                           DAY(Today):
                DELETE FFItem.
             END.
          
             ASSIGN
                FixedFee.EndPeriod = YEAR(Today) * 100 + MONTH(Today).
         
             IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFixedFee).
          END.
          
          MESSAGE
          "Totally " lii " servicelimits closed!"
          VIEW-aS ALERT-BOX.
       
          must-print = TRUE.
          NEXT LOOP.
                               
       END.
     END.
     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          lccli
          mservicelimit.inclamt.

       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE mservicelimit THEN memory = recid(mservicelimit).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE] 
no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE mservicelimit THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(mservicelimit).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND mservicelimit where recid(mservicelimit) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          lccli
          mservicelimit.inclunit.
       IF ok THEN DO:

           DELETE mservicelimit.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST mservicelimit
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */


     else if lookup(nap,"7,f7") > 0 THEN DO:  /* hinnasto */

     END.
     
     else if lookup(nap,"enter,return") > 0 AND NOT llAdmin THEN DO:
       FIND FIRST mservicelimit where 
            recid(mservicelimit) = rtab[frame-line(sel)] no-lock.
       assign fr-header = " VIEW " ufk = 0 ufk[8] = 8 ehto = 3.
       RUN ufkey.
       cfc = "lis". RUN ufcolor.

       IF AVAIL mservicelimit THEN DO:
         RUN LOCAL-FIND-OTHER.

         DISP
            mservicelimit.MSSeq
            lccli
            mservicelimit.DialType lcType
            mservicelimit.InclAmt
            mservicelimit.FromTS
            mservicelimit.endTS
            fTs2hms(mservicelimit.FromTS)  @ lcvalidfrom
            fTs2hms(mservicelimit.endTS)   @ lcvalidto
            WITH FRAME lis.
            
            fDispUnit(mServiceLimit.InclUnit).
       END.
       xrecid = recid(mservicelimit).
       must-print = true.
       NEXT LOOP.
       
     END.

     else if lookup(nap,"enter,return") > 0 AND llAdmin THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST mservicelimit where 
            recid(mservicelimit) = rtab[frame-line(sel)]
       no-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.

       cfc = "lis". RUN ufcolor.

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       xrecid = recid(mservicelimit).
       must-print = true.
       NEXT LOOP.

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(mservicelimit) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(mservicelimit) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   RUN LOCAL-FIND-OTHER.

   DISPLAY

      lccli 
      mservicelimit.DialType
      mservicelimit.InclUnit 
      mservicelimit.InclAmt  
      lcvalid
      lcGroup
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF iiSlseq > 0 THEN DO:
      IF iiDialType > 0 THEN 
      FIND NEXT mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq    = iislseq     AND 
                mservicelimit.dialtype = iidialtype  
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND NEXT mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq = iislseq 
                 
      NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiMsSeq > 0 THEN DO:           
      IF iiDialType > 0 THEN 
      FIND NEXT mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq AND
                mservicelimit.dialtype = iidialtype 
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND NEXT mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq  
                 NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF iiDialType > 0 THEN 
      FIND NEXT mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.dialtype = iidialtype
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND NEXT mservicelimit USE-INDEX MSSeq  
                 NO-LOCK NO-ERROR.
   END.
   
            
END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF iiSlseq > 0 THEN DO:
      IF iiDialType > 0 THEN 
      FIND PREV mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq    = iislseq  AND 
                mservicelimit.dialtype = iidialtype  
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND PREV mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq = iislseq  
                 
      NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiMsSeq > 0 THEN DO:           
      IF iiDialType > 0 THEN 
      FIND PREV mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq AND
                mservicelimit.dialtype = iidialtype 
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND PREV mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq 
                 NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF iiDialType > 0 THEN 
      FIND PREV mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.dialtype = iidialtype 
                 NO-LOCK NO-ERROR.
      ELSE          
      FIND PREV mservicelimit USE-INDEX MSSeq  
                       NO-LOCK NO-ERROR.
   END.
    
END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF iiSlseq > 0 THEN DO:
      IF iiDialType > 0 THEN 
      FIND FIRST mservicelimit USE-INDEX SLSeq  WHERE
                 mservicelimit.slseq    = iislseq  AND 
                 mservicelimit.dialtype = iidialtype 
                  NO-LOCK NO-ERROR.
      ELSE          
      FIND FIRST mservicelimit USE-INDEX SLSeq  WHERE
                 mservicelimit.slseq = iislseq  
                  
      NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiMsSeq > 0 THEN DO:           
      IF iiDialType > 0 THEN 
      FIND FIRST mservicelimit USE-INDEX MSSeq  WHERE
                 mservicelimit.msseq = iimsseq AND
                 mservicelimit.dialtype = iidialtype 
                  NO-LOCK NO-ERROR.
      ELSE          
      FIND FIRST mservicelimit USE-INDEX MSSeq  WHERE
                 mservicelimit.msseq = iimsseq  
                  NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF iiDialType > 0 THEN 
      FIND FIRST mservicelimit USE-INDEX MSSeq  WHERE
                 mservicelimit.dialtype = iidialtype 
                  NO-LOCK NO-ERROR.
      ELSE          
      FIND FIRST mservicelimit USE-INDEX MSSeq  
                  NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF iiSlseq > 0 THEN DO:
      IF iiDialType > 0 THEN 
      FIND LAST mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq    = iislseq  AND 
                mservicelimit.dialtype = iidialtype 
                                 NO-LOCK NO-ERROR.
      ELSE          
      FIND LAST mservicelimit USE-INDEX SLSeq  WHERE
                mservicelimit.slseq = iislseq  
                                  
      NO-LOCK NO-ERROR.
   END.
   
   ELSE IF iiMsSeq > 0 THEN DO:           
      IF iiDialType > 0 THEN 
      FIND LAST mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq AND
                mservicelimit.dialtype = iidialtype 
                                 NO-LOCK NO-ERROR.
      ELSE          
      FIND LAST mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.msseq = iimsseq  
                                 NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF iiDialType > 0 THEN 
      FIND LAST mservicelimit USE-INDEX MSSeq  WHERE
                mservicelimit.dialtype = iidialtype 
                                 NO-LOCK NO-ERROR.
      ELSE          
      FIND LAST mservicelimit USE-INDEX MSSeq NO-LOCK NO-ERROR.
   END.
   
            
END PROCEDURE.
 
PROCEDURE LOCAL-FIND-OTHER.

   lccli = "".
   
   FIND FIRST Mobsub WHERE 
              Mobsub.msseq = mServiceLimit.msseq NO-LOCK NO-ERROR.
   IF avail mobsub THEN lccli = mobsub.cli.
   ELSE DO:
      FIND FIRST MsOwner NO-LOCK WHERE
                  MsOwner.MsSeq = mServiceLimit.msseq NO-ERROR.
      IF AVAILABLE MsOwner THEN lcCLI = MsOwner.CLI + "*".
   END.

   FIND dialtype WHERE
        dialtype.dialtype = mservicelimit.dialtype NO-LOCK NO-ERROR.
           
   IF AVAIL dialtype THEN lctype = dialtype.dtname.
   ELSE lctype = "".

   lcvalid =  fTs2hms(mservicelimit.FromTS) + "-" .
   IF mservicelimit.EndTS < 99999999 THEN 
   lcvalid = lcvalid +  fTs2hms(mservicelimit.EndTS).

   FIND FIRST servicelimit WHERE
              ServiceLimit.slseq = Mservicelimit.slseq AND 
             ServiceLimit.dialtype = mservicelimit.dialtype NO-LOCK NO-error.

   IF avail servicelimit THEN lcGroup = servicelimit.groupcode.
   ELSE lcGroup  = "?".     

END PROCEDURE.

       
PROCEDURE LOCAL-UPDATE-RECORD. 
   run LOCAL-FIND-OTHER.
   DEF INPUT PARAMETE bNew AS LO NO-UNDO.
   
   DISP 
      mservicelimit.MSSeq 
      lccli
      mservicelimit.DialType lctype
      mservicelimit.InclAmt 
      mservicelimit.fromts
      mservicelimit.endts
      fTs2hms(mservicelimit.FromTS)  @ lcvalidfrom
      fTs2hms(mservicelimit.endTS)   @ lcvalidto
   with frame lis. 

   fDispUnit(mServiceLimit.InclUnit).
         
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
         
      PROMPT 
         mservicelimit.InclAmt 
         mservicelimit.FromTS
         mservicelimit.endTS
      WITH FRAME lis EDITING: 
         
         ehto = 9. RUN ufkey.p.
         
         READKEY.
         nap = KEYLABEL(LASTKEY). 
         
         IF nap = "F9" AND
            FRAME-FIELD = "InclUnit"
         THEN DO:
            RUN h-tmscodes(INPUT "Tariff",    /* TableName */
                                  "DataType", /* FieldName */
                                  "Tariff",     /* GroupCode */
                                  OUTPUT lcCode).
                                  
            IF lcCode ne "" AND lcCode NE ? THEN DO:
               fDispUnit(INTEGER(lcCode)).
               DISPLAY INTEGER(lcCode) ;& mServiceLimit.InclUnit
               WITH FRAME lis.
             END.
             NEXT.
         END.

         IF lookup(nap,poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "fromts" THEN DO:
               disp fTs2hms(input input mservicelimit.FromTS)  @ lcvalidfrom.
            END.

            ELSE IF FRAME-FIELD = "endts" THEN DO:
               disp fTs2hms(input mservicelimit.endts)  @ lcvalidto.
            END.

            ELSE IF FRAME-FIELD = "servicelimit" THEN DO:
               if input frame lis mservicelimit.MSSeq  = "" THEN DO:
                  LEAVE .
               END.
            END.
            
            ELSE IF FRAME-FIELD = "InclUnit" THEN DO:
               IF NOT fDispUnit(INPUT INPUT mServiceLimit.InclUnit)
               THEN DO:
                  BELL.
                  MESSAGE "Unknown unit".
                  NEXT.
               END.
            END.
         END.
         
         APPLY LASTKEY. 
      END.
      
      FIND CURRENT mServiceLimit NO-LOCK.

      IF CURRENT-CHANGED mServiceLimit THEN DO:
         MESSAGE {&MSG_RECORD_CHANGED}
         VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
      END.
      ELSE DO:
         
         FIND CURRENT mServiceLimit EXCLUSIVE-LOCK.
         
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMServiceLimit).
         
         ASSIGN
            mservicelimit.InclAmt 
            mservicelimit.FromTS
            mservicelimit.endTS.
         
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMServiceLimit).
         find current mservicelimit NO-LOCK.
      END.

      LEAVE.

   END.

   HIDE FRAME lis no-pause.
   
END.

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
END.
