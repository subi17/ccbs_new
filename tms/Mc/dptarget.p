/* ----------------------------------------------------------------------
  MODULE .......: DPTarget.p
  TASK .........: UPDATEs table DPTarget
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.04.11
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DPTarget'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDPTarget AS HANDLE NO-UNDO.
   lhDPTarget = BUFFER DPTarget:HANDLE.
   RUN StarEventInitialize(lhDPTarget).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDPTarget).
   END.

END.

DEF INPUT PARAMETER iiDPId AS INT  NO-UNDO.

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

DEF VAR lcStatus AS CHAR NO-UNDO.
DEF VAR lcField  AS CHAR NO-UNDO. 
DEF VAR lcCode   AS CHAR NO-UNDO. 
DEF VAR lcPlan   AS CHAR NO-UNDO.
DEF VAR lcName   AS CHAR NO-UNDO. 

FORM
    DPTarget.TargetTable FORMAT "X(12)"
    DPTarget.TargetKey  
    lcName FORMAT "X(20)" COLUMN-LABEL "Name"
    DPTarget.ValidTo    FORMAT "99-99-99"
    DPTarget.Included   COLUMN-LABEL "Incl."
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " TARGETS FOR PLAN " + lcPlan + " "
    FRAME sel.

FORM
    SKIP
    DPTarget.DPId       COLON 18
       DiscountPlan.DPName NO-LABEL FORMAT "X(35)" SKIP
    DPTarget.TargetTable   COLON 18
         FORMAT "X(16)"
    DPTarget.TargetKey     COLON 18      
       lcName NO-LABEL FORMAT "X(30)" SKIP
    DPTarget.Included   COLON 18   
    DPTarget.ValidFrom  COLON 18 
    DPTarget.ValidTo    COLON 18
       SKIP(1)
WITH  OVERLAY ROW 8 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FUNCTION fTargetName RETURNS CHAR
   (icTargetTable AS CHAR,
    icTargetKey AS CHAR):

   DEF VAR lcTarget AS CHAR NO-UNDO.
    
   lcTarget = "".
   
   CASE icTargetTable:
   WHEN "BillItem" THEN DO:
      FIND FIRST BillItem WHERE
                 BillItem.Brand = gcBrand AND
                 BillItem.BillCode = icTargetKey NO-LOCK NO-ERROR.
      IF AVAILABLE BillItem THEN lcTarget = BillItem.BIName.
   END.
   WHEN "BItemGroup" THEN DO:
      FIND FIRST BItemGroup WHERE
                 BItemGroup.Brand = gcBrand AND
                 BItemGroup.BIGroup = icTargetKey NO-LOCK NO-ERROR.
      IF AVAILABLE BItemGroup THEN lcTarget = BItemGroup.BIGName.
   END.
   END CASE. 

   RETURN lcTarget.
    
END FUNCTION.


FIND FIRST DiscountPlan WHERE DiscountPlan.DPId = iiDPId NO-LOCK NO-ERROR.
IF NOT AVAILABLE DiscountPlan THEN DO:
   MESSAGE "Discount plan not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.
lcPlan = DiscountPlan.DPRuleID.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE DPTarget THEN ASSIGN
   Memory       = recid(DPTarget)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No targets available!" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a DPTarget  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY iiDPId @ DPTarget.DPId.

           CREATE DPTarget.
           ASSIGN 
              DPTarget.DPId = iiDPId
              DPTarget.ValidFrom  = TODAY
              DPTarget.ValidTo    = 12/31/2049.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              DPTarget.TargetTable = "" OR 
              DPTarget.TargetKey = ""
           THEN UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDPTarget).

           ASSIGN
           Memory = recid(DPTarget)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE DPTarget THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DPTarget WHERE recid(DPTarget) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DPTarget THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DPTarget).
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
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DPTarget.TargetTable {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DPTarget.TargetTable WITH FRAME sel.
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
        FIND DPTarget WHERE recid(DPTarget) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DPTarget THEN
              ASSIGN FIRSTrow = i Memory = recid(DPTarget).
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
           IF NOT AVAILABLE DPTarget THEN DO:
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
                rtab[1] = recid(DPTarget)
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
           IF NOT AVAILABLE DPTarget THEN DO:
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
              rtab[FRAME-DOWN] = recid(DPTarget).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DPTarget WHERE recid(DPTarget) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DPTarget THEN DO:
           Memory = recid(DPTarget).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DPTarget THEN Memory = recid(DPTarget).
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
           FIND DPTarget WHERE recid(DPTarget) = Memory NO-LOCK.
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
          DPTarget.TargetTable
          DPTarget.TargetKey
          DPTarget.ValidTo.

       RUN local-find-NEXT.
       IF AVAILABLE DPTarget THEN Memory = recid(DPTarget).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DPTarget THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DPTarget).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO DELETE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DPTarget.TargetTable
          DPTarget.TargetKey
          DPTarget.ValidTo.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPTarget).

           DELETE DPTarget.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DPTarget THEN DO:
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
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPTarget).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPTarget).

       RUN local-disp-row.
       xrecid = recid(DPTarget).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DPTarget) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DPTarget) must-print = TRUE.
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
      FIND DPTarget WHERE recid(DPTarget) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DPTarget WHERE recid(DPTarget) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST DPTarget WHERE 
      DPTarget.DPId = iiDPId NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST DPTarget WHERE 
      DPTarget.DPId = iiDPId NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT DPTarget WHERE 
      DPTarget.DPId = iiDPId NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV DPTarget WHERE 
      DPTarget.DPId = iiDPId NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DPTarget.TargetTable
       DPTarget.TargetKey
       lcName
       DPTarget.Included
       DPTarget.ValidTo
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcName = fTargetName(DPTarget.TargetTable,
                        DPTarget.TargetKey).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bTarget FOR DPTarget.
   
   DEF VAR i AS INT NO-UNDO. 

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP 
         DPTarget.DPId        
         DiscountPlan.DPName
         DPTarget.TargetTable
         DPTarget.TargetKey
         lcName
         DPTarget.Included
         DPTarget.ValidFrom
         DPTarget.ValidTo        
         DPTarget.Included
      WITH FRAME lis.

      IF NOT NEW DPTarget THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateTarget:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT DPTarget EXCLUSIVE-LOCK.
         ehto = 9.
         RUN Syst/ufkey.p.
         
         UPDATE
            DPTarget.TargetTable WHEN NEW DPTarget
            DPTarget.TargetKey   WHEN NEW DPTarget
            DPTarget.Included
            DPTarget.ValidFrom
            DPTarget.ValidTo        
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"TargetTable,TargetKey") > 0 THEN DO:

               IF FRAME-FIELD = "TargetTable" THEN DO:
                  RUN Help/h-tmscodes(INPUT "DPTarget",
                                       "TargetTable",     
                                       ?,     
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN 
                     DISPLAY lcCode @ DPTarget.TargetTable WITH FRAME lis.
               END.
               
               ELSE IF FRAME-FIELD = "TargetKey" THEN DO:
               
                  CASE INPUT DPTarget.TargetTable:
                  WHEN "BillItem" THEN DO:
                     RUN Help/nntuse.p.
                     IF siirto NE ? THEN 
                        DISPLAY siirto @ DPTarget.TargetKey WITH FRAME lis.
                  END.
                  WHEN "BItemGroup" THEN DO:
                     RUN Help/nnpgse.p.
                     IF siirto NE ? THEN 
                        DISPLAY siirto @ DPTarget.TargetKey WITH FRAME lis.
                  END.
                  END CASE.
               END.
               
               ehto = 9.
               RUN Syst/ufkey.
               NEXT. 
            END.
 
            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "TargetTable" THEN DO:
                  IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                          "DPTarget",
                                          "TargetTable",
                                          INPUT INPUT DPTarget.TargetTable)
                  THEN DO:
                     MESSAGE "Unknown target table" 
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "TargetKey" THEN DO:
                  
                  lcName = fTargetName(INPUT INPUT DPTarget.TargetTable,
                                       INPUT INPUT DPTarget.TargetKey).

                  IF lcName = "" AND 
                     INDEX(INPUT DPTarget.TargetKey,"*") = 0 THEN DO:
                        MESSAGE "Unknown target"
                        VIEW-AS ALERT-BOX ERROR.
                        NEXT. 
                  END.

                  DISPLAY lcName WITH FRAME lis.
               END.

            END.
            
            APPLY LASTKEY.
         END.

         IF CAN-FIND(FIRST bTarget WHERE
                           bTarget.DPId = DPTarget.DPId AND
                           bTarget.TargetTable = DPTarget.TargetTable AND
                           bTarget.TargetKey = DPTarget.TargetKey AND
                           bTarget.ValidTo   >= DPTarget.ValidFrom  AND
                           bTarget.ValidFrom <= DPTarget.ValidTo    AND
                           RECID(bTarget)  NE RECID(DPTarget))
         THEN DO:
            MESSAGE "A similar row already exists for given period"
            VIEW-AS ALERT-BOX ERROR.
            UNDO, NEXT UpdateTarget.
         END.

         LEAVE UpdateTarget.
      END.
      
      IF NEW DPTarget THEN LEAVE.   
   END.
   
END PROCEDURE.

