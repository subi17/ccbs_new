/* ----------------------------------------------------------------------
  MODULE .......: alarm_tms
  TASK .........: Cause an alarm, if TMS does exception
  APPLICATION ..: tms
  AUTHOR .......: Janne Tourunen
  CREATED ......: 23.02.2017
  ---------------------------------------------------------------------- */
/*
&GLOBAL-DEFINE BrTable DiscountPlan
*/

{commali.i}
{eventval.i}
{timestamp.i}
/*
{brand.i}
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDiscountPlan AS HANDLE NO-UNDO.
   lhDiscountPlan = BUFFER DiscountPlan:HANDLE.
   RUN StarEventInitialize(lhDiscountPlan).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhDiscountPlan).
   END.

END.
*/
DEF SHARED VAR siirto AS CHAR.

DEF VAR lcRuleID      AS CHAR                   NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR ldDiscValue     AS DEC  NO-UNDO.
DEF VAR lcDPUnit        AS CHAR NO-UNDO.
DEF VAR lcSubject       AS CHAR NO-UNDO.
DEF VAR llSubjectType   AS LOG  NO-UNDO.
DEF VAR llTargetType    AS LOG  NO-UNDO.
DEF VAR llCCDisplay     AS LOG  NO-UNDO.
/*
Field-Name           Datatype Initial Format         Label
-------------------- -------- ------- -------------- -------------------------
ActionLog            char             x(50)          ActionLog
AlarmID              int64    0       ->,>>>,>>9     AlarmID
AlarmText            char             x(50)          AlarmText
CancelTime           dec      0       99999999.99999 CancelTS
ClearTime            dec      0       99999999.99999 ClearTS
CurrentStatus        int      0       >9             Status
Parameters           char             x(50)          Parameters
ProgramBlock         char             x(20)          Program
ResetTime            dec      0       99999999.99999 ResetTS
SettingTime          dec      0       99999999.99999 SettingTS
Severity             int      0       >9             Severity

*/
/* id, text, status, severity, time set */
FORM
    Alarm.AlarmID    FORMAT ">>>>>>>9"
    Alarm.CurrentStatus  FORMAT ">9"
    Alarm.Severity  FORMAT ">9"
    Alarm.SettingTime 
    Alarm.AlarmText    FORMAT "X(30)"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TMS ALARM  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.


/*
FORM
    DiscountPlan.Brand          COLON 25
    DiscountPlan.DPId           COLON 25 FORMAT ">>>>>>>9"
    DiscountPlan.DPRuleID       COLON 25 FORMAT "X(16)"
    DiscountPlan.DPName         COLON 25 FORMAT "X(50)"
    DiscountPlan.BillCode       COLON 25 FORMAT "X(16)"
       BillItem.BIName FORMAT "X(30)" NO-LABEL SKIP
    DiscountPlan.ValidFrom      COLON 25
       VALIDATE(INPUT DiscountPlan.ValidFrom NE ?,
                "Effective date is mandatory")
       LABEL "Valid"
       "-"
    DiscountPlan.ValidTo  
       NO-LABEL 
       VALIDATE(INPUT DiscountPlan.ValidTo NE ? AND
                INPUT DiscountPlan.ValidTo >= INPUT DiscountPlan.ValidFrom,
                "Invalid expiration date")
    DiscountPlan.Priority       COLON 25 
    DiscountPlan.Subject     COLON 25
    llSubjectType               COLON 25 FORMAT "All/List"
       LABEL "Subject Type"
       HELP "Subject selection, All or List"
    llTargetType              COLON 25 FORMAT "All/List"
       LABEL "Target Type"
       HELP "Target selection, All or List"
    DiscountPlan.ProcessStopper COLON 25
    ldDiscValue                 COLON 25 FORMAT "->>>>>9.99"
       LABEL "Default Value"
       HELP "Default discount value"
    DiscountPlan.DPUnit         COLON 25 FORMAT "X(12)"
    DiscountPlan.MaxAmount      COLON 25
    DiscountPlan.MinBaseAmount  COLON 25
    Discountplan.ValidPeriods   COLON 25 FORMAT ">>>"
       LABEL "Period Limit" 
    llCCDisplay                 COLON 25 FORMAT "Yes/No" 
       LABEL "Visible In CC" 
       HELP "Visible in CC tools"
WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FORM 
    "Brand:" lcBrand skip
    "Name :" lcRuleID FORMAT "X(20)" 
    HELP "Enter rule ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Rule ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.
*/

FUNCTION fBillItemName RETURNS LOGIC
  (icBillCode AS CHAR):
  
   FIND FIRST BillItem WHERE
              BillItem.Brand = gcBrand AND
              BillItem.BillCode = icBillCode NO-LOCK NO-ERROR.
   IF AVAILABLE BillItem THEN DISPLAY BillItem.BIName WITH FRAME lis.
   ELSE DISPLAY "" @ BillItem.BIName WITH FRAME lis.
  
   RETURN (AVAILABLE BillItem).

END FUNCTION.

/*
FUNCTION fSubject RETURNS LOGIC
   (icSubject AS CHAR):

   lcSubject = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "DiscountPlan",
                                   "Subject",
                                   icSubject).
   DISPLAY lcSubject WITH FRAME lis.
   
   RETURN (lcSubject > "").
      
END FUNCTION.

FUNCTION fDPUnit RETURNS LOGIC
   (icDPUnit AS CHAR):

   lcSubject = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                   "DiscountPlan",
                                   "DPUnit",
                                   icDPUnit).
   DISPLAY lcDPUnit WITH FRAME lis.
   
   RETURN (lcDPUnit > "").
      
END FUNCTION.
*/


IF gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

cfc = "sel". RUN ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE Alarm THEN ASSIGN
   Memory       = recid(Alarm)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   /*
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rules available!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   */
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.
/*
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND DiscountPlan WHERE recid(DiscountPlan) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DiscountPlan THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(DiscountPlan).
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
        ufk[1] = 816
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DiscountPlan.DPRuleID {uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DiscountPlan.DPRuleID WITH FRAME sel.
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
        FIND DiscountPlan WHERE recid(DiscountPlan) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE DiscountPlan THEN
              ASSIGN FIRSTrow = i Memory = recid(DiscountPlan).
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
           IF NOT AVAILABLE DiscountPlan THEN DO:
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
                rtab[1] = recid(DiscountPlan)
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
           IF NOT AVAILABLE DiscountPlan THEN DO:
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
              rtab[FRAME-DOWN] = recid(DiscountPlan).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND DiscountPlan WHERE recid(DiscountPlan) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DiscountPlan THEN DO:
           Memory = recid(DiscountPlan).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE DiscountPlan THEN Memory = recid(DiscountPlan).
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
           FIND DiscountPlan WHERE recid(DiscountPlan) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.p.
       ehto = 9. RUN ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcRuleID WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcRuleID > "" THEN DO:
          FIND FIRST DiscountPlan WHERE 
                     DiscountPlan.Brand = lcBrand AND
                     DiscountPlan.DPRuleID >= lcRuleID
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST DPMember WHERE 
            DPMember.DPId = DiscountPlan.DPId)
       THEN DO:
          MESSAGE "Members exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST DPTarget WHERE 
            DPTarget.DPId = DiscountPlan.DPId)
       THEN DO:
          MESSAGE "Targets exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       IF CAN-FIND(FIRST DPSubject WHERE 
                         DPSubject.DPId = DiscountPlan.DPId)
       THEN DO:
          MESSAGE "Subjects exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DiscountPlan.DPId DiscountPlan.DPName
       DiscountPlan.Priority.
        
       RUN local-find-NEXT.
       IF AVAILABLE DiscountPlan THEN Memory = recid(DiscountPlan).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE DiscountPlan THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(DiscountPlan).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       DiscountPlan.DPId DiscountPlan.DPName
       DiscountPlan.Priority.
       
       IF ok THEN DO:

           FOR EACH DPRate EXCLUSIVE-LOCK WHERE
                    DPRate.DPId = DiscountPlan.DPId:
              DELETE DPRate.
           END.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDiscountPlan).

           DELETE DiscountPlan.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DiscountPlan THEN DO:
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
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDiscountPlan).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.p.
       cfc = "lis". RUN ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY DiscountPlan.DPId.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDiscountPlan).

       RUN local-disp-row.
       xrecid = recid(DiscountPlan).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(DiscountPlan) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(DiscountPlan) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */
*/
HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.
/*
IF gcHelpParam > "" THEN DO:
   IF xRecid NE ? THEN DO:
      FIND FIRST DiscountPlan WHERE RECID(DiscountPlan) = xRecid NO-LOCK.
      siirto = STRING(DiscountPlan.DPRuleId).
   END.   
END.
*/   

ehto = 4.
RUN ufkey.p.
/*
fCleanEventObjects().
*/


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND DiscountPlan WHERE recid(DiscountPlan) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND DiscountPlan WHERE recid(DiscountPlan) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV Alarm USE-INDEX AlarmID NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       Alarm.AlarmId
       Alarm.CurrentStatus
       Alarm.Severity
       Alarm.SettingTime
       Alarm.AlarmText
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.
/*
PROCEDURE local-UPDATE-record:

   DEF VAR lcValue AS CHAR NO-UNDO.

   IF NEW DiscountPlan THEN toimi = -1.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "Subject" THEN DO:
                  IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "DiscountPlan",
                                      "Subject",
                                      INPUT INPUT DiscountPlan.Subject) = ""
                  THEN DO:
                     MESSAGE "Unknown type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
               
               ELSE IF FRAME-FIELD = "DPUnit" THEN DO:
                  IF DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                      "DiscountPlan",
                                      "DPUnit",
                                      INPUT INPUT DiscountPlan.DPUnit) = ""
                  THEN DO:
                     MESSAGE "Unknown unit"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "BillCode" THEN DO:
                  IF NOT CAN-FIND(FIRST BillItem WHERE
                        BillItem.Brand = gcBrand AND
                        BillItem.BillCode = INPUT DiscountPlan.BillCode)
                  THEN DO:
                     MESSAGE "Unknown billing item"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.   
            END.
            
            APPLY LASTKEY.
         END.
   
         /*
         IF NEW DiscountPlan THEN DO:
         
            IF DiscountPlan.Subject NE "Contract Target" THEN 
               llSubjectType = TRUE.
               
            ASSIGN
               DiscountPlan.SubjectType = STRING(llSubjectType,"All/List")
               DiscountPlan.TargetType = STRING(llTargetType,"All/List").
               
            FIND FIRST DPRate EXCLUSIVE-LOCK WHERE
                       DPRate.DPId = DiscountPlan.DPId AND
                       DPRate.ValidFrom = DiscountPlan.ValidFrom AND
                       DPRate.ValidTo = DiscountPlan.ValidTo NO-ERROR.
            IF NOT AVAILABLE DPRate THEN DO:
               CREATE DPRate.
               ASSIGN 
                  DPRate.DPId      = DiscountPlan.DPId
                  DPRate.ValidFrom = DiscountPlan.ValidFrom
                  DPRate.ValidTo   = DiscountPlan.ValidTo
                  DPRate.DiscValue = ldDiscValue.
            END.
         END.

         DiscountPlan.CCDisplay = INT(llCCDisplay). 
            
         LEAVE.
         */
      END.

      ELSE IF toimi = 2 THEN DO:
         RUN dpsubject.p (DiscountPlan.DPId).
      END.
      
      ELSE IF toimi = 3 THEN DO:
         RUN dptarget.p (DiscountPlan.DPId).
      END.
 
      ELSE IF toimi = 4 THEN DO:
         RUN dprate.p (DiscountPlan.DPId).
      END.

      ELSE IF toimi = 6 THEN DO:
         RUN dpmember.p (DiscountPlan.DPId,"","").
      END.
      
      /* translations */
      ELSE IF toimi = 7 THEN DO:  
         RUN invlang.p(31,STRING(DiscountPlan.DPId)).
      END.
       
      ELSE IF toimi = 8 THEN LEAVE.  

   END.
   
END PROCEDURE.
*/
