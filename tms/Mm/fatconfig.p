/* ----------------------------------------------------------------------
  MODULE .......: FATConfig
  TASK .........: UPDATEs table FATConfig
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 02.05.06
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable FATConfig

{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FATConfig'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFATConfig AS HANDLE NO-UNDO.
   lhFATConfig = BUFFER FATConfig:HANDLE.
   RUN StarEventInitialize(lhFATConfig).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhFATConfig).
   END.

END.

DEF INPUT PARAMETER icFatGroup AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcConfType    AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
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
DEF VAR lcTypeName   AS CHAR                   NO-UNDO. 
DEF VAR lcTargName   AS CHAR                   NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO FORMAT "X(2)". 
DEF VAR ldtToDate    AS DATE                   NO-UNDO.

DEF BUFFER bFATConfig FOR FATConfig.
    
form
    FATConfig.ConfType   
    lcTypeName           FORMAT "X(12)" COLUMN-LABEL "Type Name"
    FATConfig.ConfTarget 
    lcTargName           FORMAT "X(12)" COLUMN-LABEL "Target Name"
    FATConfig.ValidFrom    
    FATConfig.ValidTo    
    FATConfig.ConfRule1  FORMAT "X(16)" 
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " " + icFatGroup + " CONFIGURATION "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    FATConfig.ConfType   COLON 22
      lcTypeName FORMAT "X(40)" NO-LABEL SKIP
    FATConfig.ValidFrom  COLON 22 
    FATConfig.ValidTo    COLON 22 
    FATConfig.ConfTarget COLON 22 
      lcTargName FORMAT "X(35)" NO-LABEL SKIP
    FATConfig.ConfRule1  COLON 22
    FATConfig.ConfRule2  COLON 22
    FATConfig.ConfRule3  COLON 22
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FUNCTION fTypeName RETURNS CHARACTER
   (iiConfType AS INT).

   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "FATConfig",
                           "ConfType",
                           STRING(iiConfType)).
END FUNCTION.

FUNCTION fTargName RETURNS CHARACTER
   (icConfTarg AS CHAR).

   IF icConfTarg = "" THEN RETURN "".
   
   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "FATConfig",
                           "ConfTarget",
                           icConfTarg).
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE FATConfig THEN ASSIGN
   Memory       = recid(FATConfig)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.
    
   IF must-add THEN DO:  /* Add a FATConfig  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR FATConfig.ConfType WITH FRAME lis EDITING:
           
               READKEY. 
               nap = KEYLABEL(LASTKEY).

               IF nap = "F9" THEN DO:

                  RUN Help/h-tmscodes.p(INPUT "FATConfig", /* TableName*/
                                       "ConfType",  /* FieldName */
                                       "FATime",    /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     DISPLAY lcCode @ FATConfig.ConfType WITH FRAME lis.
                  END.

                  ehto = 9.
                  RUN Syst/ufkey.p.
                  NEXT. 
               END.

               APPLY LASTKEY.
           END.

           IF INPUT FRAME lis FATConfig.ConfType = ""
           THEN LEAVE add-row.

           IF NOT CAN-FIND(FIRST TMSCodes WHERE
                           TMSCodes.TableName = "FATConfig" AND
                           TMSCodes.FieldName = "ConfType"  AND
                           TMSCodes.CodeValue = INPUT FATConfig.ConfType)
           THEN DO:
              MESSAGE "Unknown configuration type" 
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           
           CREATE FATConfig.
           ASSIGN
           FATConfig.Brand     = lcBrand
           FATConfig.FTGrp     = icFatGroup
           FATConfig.ConfType  = INPUT FATConfig.ConfType
           FATConfig.ValidFrom = TODAY
           FATConfig.ValidTo   = 12/31/2050.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFATConfig).

           ASSIGN
           Memory = recid(FATConfig)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE FATConfig THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FATConfig WHERE recid(FATConfig) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FATConfig THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FATConfig).
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
        ufk    = 0
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FATConfig.ConfType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATConfig.ConfType WITH FRAME sel.
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
        FIND FATConfig WHERE recid(FATConfig) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FATConfig THEN
              ASSIGN FIRSTrow = i Memory = recid(FATConfig).
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
           IF NOT AVAILABLE FATConfig THEN DO:
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
                rtab[1] = recid(FATConfig)
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
           IF NOT AVAILABLE FATConfig THEN DO:
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
              rtab[FRAME-DOWN] = recid(FATConfig).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FATConfig WHERE recid(FATConfig) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FATConfig THEN DO:
           Memory = recid(FATConfig).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FATConfig THEN Memory = recid(FATConfig).
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
           FIND FATConfig WHERE recid(FATConfig) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       FATConfig.ConfType FATConfig.ValidFrom FATConfig.ValidTo.

       RUN local-find-NEXT.
       IF AVAILABLE FATConfig THEN Memory = recid(FATConfig).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FATConfig THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FATConfig).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       FATConfig.ConfType FATConfig.ValidFrom FATConfig.ValidTo.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFATConfig).

           DELETE FATConfig.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE FATConfig THEN DO:
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
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATConfig).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATConfig).

       RUN local-disp-row.
       xrecid = recid(FATConfig).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FATConfig) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FATConfig) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FATConfig WHERE recid(FATConfig) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FATConfig WHERE recid(FATConfig) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST FATConfig 
          WHERE FATConfig.Brand = lcBrand AND
                FATConfig.FTGrp = icFatGroup
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST FATConfig 
          WHERE FATConfig.Brand = lcBrand AND
                FATConfig.FTGrp = icFatGroup
          NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT FATConfig 
          WHERE FATConfig.Brand = lcBrand AND
                FATConfig.FTGrp = icFatGroup
          NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV FATConfig 
          WHERE FATConfig.Brand = lcBrand AND
                FATConfig.FTGrp = icFatGroup
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       FATConfig.ConfType
       lcTypeName
       FATConfig.ConfTarget
       lcTargName
       FATConfig.ValidFrom
       FATConfig.ValidTo
       FATConfig.ConfRule1
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    lcTypeName = fTypeName(FATConfig.ConfType).
    lcTargName = fTargName(FATConfig.ConfTarget).
       
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      FATConfig.ConfType
      lcTypeName
      FATConfig.ConfTarget
      lcTargName
      FATConfig.ValidFrom
      FATConfig.ValidTo
      FATConfig.ConfRule1
      FATConfig.ConfRule2
      FATConfig.ConfRule3
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN Syst/ufkey.p.
      
         UPDATE
         FATConfig.ValidFrom    
         FATConfig.ValidTo     
         FATConfig.ConfTarget WHEN NEW FATConfig
         FATConfig.ConfRule1
         FATConfig.ConfRule2
         FATConfig.ConfRule3
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"ConfTarget,ConfRule1") > 0  
            THEN DO:

               IF FRAME-FIELD = "ConfTarget" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "FATConfig",   /* TableName*/
                                       "ConfTarget",  /* FieldName */
                                       STRING(FatConfig.ConfType),
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     FATConfig.ConfTarget = lcCode.
                     lcTargName = fTargName(FATConfig.ConfTarget).
                     DISPLAY FATConfig.ConfTarget lcTargName WITH FRAME lis.
                  END.
               END.

               ELSE IF FRAME-FIELD = "ConfRule1" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "FATConfig",   /* TableName*/
                                       "ConfRule1",   /* FieldName */
                                       STRING(FatConfig.ConfType),
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     FATConfig.ConfRule1 = lcCode.
                     DISPLAY FATConfig.ConfRule1 WITH FRAME lis.
                  END.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

          
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "ConfTarget" THEN DO:
                  lcTargName = fTargName(INPUT INPUT FRAME lis 
                                              FATConfig.ConfTarget).
                  IF lcTargName = "" THEN DO:
                     BELL.
                     MESSAGE "Unknown target".
                     NEXT.
                  END. 
                     
                  DISPLAY lcTargName WITH FRAME lis.
               END.

               ELSE IF FRAME-FIELD = "ValidTo" THEN DO:
                  ASSIGN FRAME lis FATConfig.ValidTo.
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */

         LEAVE. 
      END.
      
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.p.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
      
      LEAVE.
   END.
   
END PROCEDURE.

