/* ----------------------------------------------------------------------
  MODULE .......: tmrlimit.p
  TASK .........: UPDATEs table TMRLimit
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.05.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMRLimit'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTMRLimit AS HANDLE NO-UNDO.
   lhTMRLimit = BUFFER TMRLimit:HANDLE.
   RUN StarEventInitialize(lhTMRLimit).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhTMRLimit).
   END.

END.

DEF INPUT PARAMETER iiTMRuleSeq AS INT  NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 5.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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

DEF VAR lcStatus       AS CHAR NO-UNDO.
DEF VAR lcField        AS CHAR NO-UNDO. 
DEF VAR lcCode         AS CHAR NO-UNDO. 
DEF VAR lcValueType    AS CHAR NO-UNDO.
DEF VAR lcRequest      AS CHAR NO-UNDO.
DEF VAR lcSMSText      AS CHAR NO-UNDO.

FORM
    TMRLimit.FromDate
    TMRLimit.ToDate  
    TMRLimit.LimitID
    TMRLimit.LimitAmt
    TMRLimit.LimitPerc
    TMRLimit.Action
    TMRLimit.SMSText FORMAT "X(15)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " LIMITS FOR RULE " + STRING(iiTMRuleSeq) + " "
    FRAME sel.

FORM
    TMRLimit.TMRuleSeq       COLON 15
       TMRule.Name NO-LABEL SKIP(1)
    TMRLimit.FromDate        COLON 15 
    TMRLimit.ToDate          COLON 15
       SKIP(1)
    TMRLimit.LimitID         COLON 15
    TMRLimit.ValueType       COLON 15
       lcValueType NO-LABEL FORMAT "X(30)" 
       SKIP
    TMRLimit.LimitAmt        COLON 15
    TMRLimit.LimitPerc       COLON 15
    TMRLimit.MinValue        COLON 15
    TMRLimit.MaxValue        COLON 15
       VALIDATE(INPUT TMRLimit.MaxValue >= INPUT TMRLimit.MinValue,
                "Upper limit cannot be less than lower limit")
       SKIP(1)         
    TMRLimit.Action          COLON 15
       lcRequest NO-LABEL FORMAT "X(30)" 
       SKIP
    TMRLimit.ActionParam     COLON 15
       LABEL "Parameters"
    TMRLimit.SMSText         COLON 15 FORMAT "X(40)" SKIP
       lcSMSText NO-LABEL FORMAT "X(40)" AT 17
       SKIP
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fDispValueType RETURNS LOGIC
   (iiValueType AS INT):

   lcValueType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                  "TMRLimit",
                                  "ValueType",
                                  STRING(iiValueType)).
                                  
   DISP lcValueType WITH FRAME lis.
       
END FUNCTION.

FUNCTION fDispAction RETURNS LOGIC
   (iiAction AS INT):

   lcRequest = "".
   IF iiAction > 0 THEN DO:
      FIND FIRST RequestType WHERE
                 RequestType.Brand   = gcBrand AND
                 RequestType.ReqType = iiAction NO-LOCK NO-ERROR.
      IF AVAILABLE RequestType THEN lcRequest = RequestType.ReqName.
   END.
                                  
   DISP lcRequest WITH FRAME lis.
       
END FUNCTION.

FUNCTION fDispSMSText RETURNS LOGIC
   (icSMSText AS CHAR):

   lcSMSText = "".
   
   FOR FIRST InvText NO-LOCK WHERE
             InvText.Brand     = gcBrand   AND 
             InvText.Target    = "SMS"     AND
             InvText.KeyValue  = icSMSText AND
             InvText.FromDate <= TODAY     AND
             InvText.ToDate   >= TODAY     AND
             InvText.Language  = 1:
      lcSMSText = InvText.TxtTitle.
      IF lcSMSText = "" THEN lcSMSText = SUBSTRING(InvText.InvText,1,30).
   END.
                                  
   DISP lcSMSText WITH FRAME lis.
       
END FUNCTION.



cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST TMRule WHERE TMRule.TMRuleSeq = iiTMRuleSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE TMRule THEN DO:
   MESSAGE "Rule not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE TMRLimit THEN ASSIGN
   Memory       = recid(TMRLimit)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No limits available!" VIEW-AS ALERT-BOX INFORMATION.
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

   IF must-add THEN DO:  /* Add a TMRLimit  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY iiTMRuleSeq @ TMRLimit.TMRuleSeq.

           i = 1.
           FOR EACH TMRLimit NO-LOCK WHERE
                    TMRLimit.TMRuleSeq = iiTMRuleSeq:
              i = MAX(i,TMRLimit.LimitID + 1).      
           END.

           CREATE TMRLimit.
           ASSIGN 
              TMRLimit.TMRuleSeq = iiTMRuleSeq
              TMRLimit.LimitID   = i
              TMRLimit.FromDate  = TODAY
              TMRLimit.ToDate    = 12/31/2049.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMRLimit).

           ASSIGN
           Memory = recid(TMRLimit)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TMRLimit THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TMRLimit WHERE recid(TMRLimit) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMRLimit THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMRLimit).
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
        ufk   = 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMRLimit.FromDate ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMRLimit.FromDate WITH FRAME sel.
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
        FIND TMRLimit WHERE recid(TMRLimit) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMRLimit THEN
              ASSIGN FIRSTrow = i Memory = recid(TMRLimit).
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
           IF NOT AVAILABLE TMRLimit THEN DO:
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
                rtab[1] = recid(TMRLimit)
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
           IF NOT AVAILABLE TMRLimit THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMRLimit).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMRLimit WHERE recid(TMRLimit) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMRLimit THEN DO:
           Memory = recid(TMRLimit).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMRLimit THEN Memory = recid(TMRLimit).
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
           FIND TMRLimit WHERE recid(TMRLimit) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

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
          TMRLimit.FromDate
          TMRLimit.ToDate
          TMRLimit.LimitID
          TMRLimit.LimitAmt
          TMRLimit.LimitPerc.

       RUN local-find-NEXT.
       IF AVAILABLE TMRLimit THEN Memory = recid(TMRLimit).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TMRLimit THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TMRLimit).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          TMRLimit.FromDate
          TMRLimit.ToDate
          TMRLimit.LimitID
          TMRLimit.LimitAmt
          TMRLimit.LimitPerc.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMRLimit).

           DELETE TMRLimit.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TMRLimit THEN DO:
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
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMRLimit).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMRLimit).

       RUN local-disp-row.
       xrecid = recid(TMRLimit).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMRLimit) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMRLimit) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND TMRLimit WHERE recid(TMRLimit) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMRLimit WHERE recid(TMRLimit) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST TMRLimit WHERE 
      TMRLimit.TMRuleSeq = iiTMRuleSeq
      USE-INDEX LimitID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST TMRLimit WHERE 
      TMRLimit.TMRuleSeq = iiTMRuleSeq
      USE-INDEX LimitID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT TMRLimit WHERE 
      TMRLimit.TMRuleSeq = iiTMRuleSeq
      USE-INDEX LimitID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV TMRLimit WHERE 
      TMRLimit.TMRuleSeq = iiTMRuleSeq
      USE-INDEX LimitID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TMRLimit.FromDate
       TMRLimit.ToDate
       TMRLimit.LimitID
       TMRLimit.LimitAmt
       TMRLimit.LimitPerc
       TMRLimit.Action
       TMRLimit.SMSText
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bLimit FOR TMRLimit.
   DEF VAR i AS INT NO-UNDO. 

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         TMRLimit.TMRuleSeq        
         TMRule.Name
         TMRLimit.FromDate
         TMRLimit.ToDate        
         TMRLimit.LimitID
         TMRLimit.LimitAmt
         TMRLimit.LimitPerc
         TMRLimit.ValueType
         TMRLimit.Action
         TMRLimit.ActionParam
         TMRLimit.SMSText
         TMRLimit.MinValue
         TMRLimit.MaxValue
      WITH FRAME lis.

      fDispValueType(TMRLimit.ValueType).
      fDispAction(TMRLimit.Action).
      fDispSMSText(TMRLimit.SMSText).
      
      IF NOT NEW TMRLimit THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateLimit:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT TMRLimit EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.
         
         UPDATE
            TMRLimit.FromDate WHEN NEW TMRLimit
            TMRLimit.ToDate        
            TMRLimit.LimitID  WHEN NEW TMRLimit
            TMRLimit.ValueType
            TMRLimit.LimitAmt
            TMRLimit.LimitPerc
            TMRLimit.MinValue
            TMRLimit.MaxValue
            TMRLimit.Action
            TMRLimit.ActionParam
            TMRLimit.SMSText
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"ValueType,Action,SMSText") > 0 
            THEN DO:

               IF FRAME-FIELD = "ValueType" THEN DO:
                  RUN Help/h-tmscodes("TMRLimit",
                                 "ValueType",
                                 "TMR",
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ?
                  THEN DISP INTEGER(lcCode) ;& TMRLimit.ValueType 
                  WITH FRAME lis.
               END.
             
               ELSE IF FRAME-FIELD = "Action" THEN DO:
                  ASSIGN
                     gcHelpParam = "tmrlimit"
                     si-recid    = ?.
               
                  RUN Syst/requesttype(0).
        
                  gcHelpParam = "".
                        
                  IF si-recid NE ? THEN DO:
                     FIND RequestType WHERE RECID(RequestType) = si-recid
                        NO-LOCK NO-ERROR.
                     IF AVAILABLE RequestType THEN 
                     DISP RequestType.ReqType @ TMRLimit.Action WITH FRAME lis.
                  END.   
               END.
 
               ELSE IF FRAME-FIELD = "SMSText" THEN DO:
                  ASSIGN
                     gcHelpParam = "tmrlimit"
                     si-recid    = ?.
               
                  RUN Mc/invotxt("SMS","").
        
                  gcHelpParam = "".
                        
                  IF si-recid NE ? THEN DO:
                     FIND InvText WHERE RECID(InvText) = si-recid
                        NO-LOCK NO-ERROR.
                     IF AVAILABLE InvText THEN 
                     DISP InvText.KeyValue @ TMRLimit.SMSText WITH FRAME lis.
                  END.   
               END.
             
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.

            ELSE IF FRAME-FIELD = "LimitPerc" THEN DO:
               IF INPUT TMRLimit.ValueType = 1 THEN DO:
                  NEXT-PROMPT TMRLimit.MinValue.
                  APPLY "RETURN".
                  NEXT.
               END.
            END.
         
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "LimitID" THEN DO:
                  IF CAN-FIND(FIRST bLimit WHERE
                           bLimit.TMRuleSeq = iiTMRuleSeq            AND
                           bLimit.LimitID   = INPUT TMRLimit.LimitID AND
                           bLimit.ToDate    = INPUT TMRLimit.ToDate  AND
                           RECID(bLimit)  NE RECID(TMRLimit))
                  THEN DO:
                     MESSAGE "A similar limit row already exists"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END. 
               END.
            
               ELSE IF FRAME-FIELD = "ValueType" THEN DO:
                  IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                          "TMRLimit",
                                          "ValueType",
                                          STRING(INPUT TMRLimit.ValueType))
                  THEN DO:
                     MESSAGE "Unknown value type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               
                  fDispValueType(INPUT INPUT TMRLimit.ValueType).
               END.

               ELSE IF FRAME-FIELD = "Action" THEN DO:
                
                  IF INPUT TMRLimit.Action > 0 AND 
                     NOT CAN-FIND(FIRST RequestType WHERE
                                  RequestType.Brand = gcBrand AND
                                  RequestType.ReqType = INPUT TMRLimit.Action)
                  THEN DO:
                     MESSAGE "Unknown action"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               
                  fDispAction(INPUT INPUT TMRLimit.Action).
               END.
 
               ELSE IF FRAME-FIELD = "SMSText" THEN DO:

                  IF INPUT TMRLimit.SMSText = "" THEN 
                      DISP "" @ lcSMSText WITH FRAME lis.
 
                  ELSE DO: 
                     
                     DO i = 1 TO NUM-ENTRIES(INPUT TMRLimit.SMSText):
                        
                        fDispSMSText(INPUT ENTRY(i,(INPUT TMRLimit.SMSText))).

                        IF lcSMSText = "" THEN DO:
                           MESSAGE "Unknown text"
                           VIEW-AS ALERT-BOX ERROR.
                           NEXT UpdateLimit.
                        END.
                     END.
                  END.
               
               END.

               ELSE IF FRAME-FIELD = "LimitAmt" THEN DO:
                  IF INPUT TMRLimit.ValueType = 1 THEN DO:
                     NEXT-PROMPT TMRLimit.MinValue.
                     NEXT.
                  END.
               END.   
          
            END.
            
            APPLY LASTKEY.
         END.

         IF TMRLimit.ValueType = 1 AND TMRLimit.MaxValue NE 0 AND
            (TMRLimit.MinValue > TMRLimit.LimitAmt OR
             TMRLimit.MaxValue < TMRLimit.LimitAmt)
         THEN DO:
            MESSAGE "Check limit and given minimum/maximum values"
            VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT UpdateLimit.
         END.
   
         IF CAN-FIND(FIRST bLimit WHERE
                           bLimit.TMRuleSeq = TMRLimit.TMRuleSeq AND
                           bLimit.LimitID   = TMRLimit.LimitID   AND
                           bLimit.ToDate   >= TMRLimit.FromDate  AND
                           bLimit.FromDate <= TMRLimit.ToDate    AND
                           RECID(bLimit)  NE RECID(TMRLimit))
         THEN DO:
            MESSAGE "A similar limit row already exists for given period"
            VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT UpdateLimit.
         END.

         FOR EACH bLimit NO-LOCK WHERE
                  bLimit.TMRuleSeq = TMRLimit.TMRuleSeq AND
                  bLimit.LimitID   NE TMRLimit.LimitID   AND
                  bLimit.ToDate   >= TMRLimit.FromDate  AND
                  bLimit.FromDate <= TMRLimit.ToDate:
                  
            IF bLimit.LimitID < TMRLimit.LimitID AND
               ((bLimit.ValueType = 1 AND
                 bLimit.LimitAmt >= TMRLimit.LimitAmt) OR
                (bLimit.ValueType = 2 AND
                 bLimit.LimitPerc >= TMRLimit.LimitPerc) OR
                (bLimit.ValueType = 2 AND
                 bLimit.LimitAmt NE TMRLimit.LimitAmt))
            THEN DO:
               MESSAGE "Given limit values are not consistent with values"
                       SKIP
                       "on limit rows with smaller ID"
               VIEW-AS ALERT-BOX ERROR.
               UNDO UpdateLimit, NEXT UpdateLimit.
            END.
 
            ELSE IF bLimit.LimitID > TMRLimit.LimitID AND
               ((bLimit.ValueType = 1 AND
                 bLimit.LimitAmt <= TMRLimit.LimitAmt) OR
                (bLimit.ValueType = 2 AND
                 bLimit.LimitPerc <= TMRLimit.LimitPerc) OR
                (bLimit.ValueType = 2 AND
                 bLimit.LimitAmt NE TMRLimit.LimitAmt))
            THEN DO:
               MESSAGE "Given limit values are not consistent with values"
                       SKIP
                       "on limit rows with greater ID"
               VIEW-AS ALERT-BOX ERROR.
               UNDO UpdateLimit, NEXT UpdateLimit.
            END.
          END.
                        
         LEAVE UpdateLimit.
      END.
      
      IF NEW TMRLimit THEN LEAVE.   
   END.
   
END PROCEDURE.

