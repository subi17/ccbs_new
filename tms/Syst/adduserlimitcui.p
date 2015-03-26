/* ----------------------------------------------------------------------
  MODULE .......: adduserlimitcui.p
  TASK .........: update user/group limits in UserLimit  
  APPLICATION ..: 
  AUTHOR .......: rdv
  CREATED ......: 
  CHANGED ......:
  Version ......: 
  ---------------------------------------------------------------------- */



{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'UserLimit'}
{eventval.i}
{tmsconst.i}
{fuserright.i}

/* input parameters */

DEF INPUT  PARAMETER  icLimitTarget   LIKE UserLimit.LimitTarget NO-UNDO.
DEF INPUT  PARAMETER  icLimitTargetID LIKE UserLimit.LimitTargetID  NO-UNDO.



DEF VAR lcLimitAmt   AS CHAR                   NO-UNDO.
DEF VAR llIsAdmin    AS LOG                    NO-UNDO.
DEF VAR lilimitType  AS INT                    NO-UNDO.
def var ob-code     like TMSCodes.CodeValue    no-undo. 
DEF VAR llIsNew      AS LOG                    NO-UNDO.
/* cui variables */
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO INIT "LIMIT INFO".
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR endloop      AS I                    NO-UNDO. 


IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhUserLimit AS HANDLE NO-UNDO.
   lhUserLimit = BUFFER UserLimit:HANDLE.
   RUN StarEventInitialize(lhUserLimit).

   ON F12 ANYWHERE 
   DO:
      RUN eventview2.p(lhUserLimit).
   END.
END.


/* check admin user rights */
IF getTMSRight("CCSUPER,SYST") = "RW" THEN llIsAdmin = TRUE. ELSE llIsAdmin = FALSE.


/* frame to select limit type */
FORM
    TMSCodes.CodeValue    COLUMN-LABEL "Type"    FORMAT "X(5)"
    TMSCodes.CodeName     COLUMN-LABEL "Description"   FORMAT "X(30)"
    lcLimitAmt            COLUMN-LABEL "Value" FORMAT "X(20)"    
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
COLOR VALUE(cfc)
TITLE COLOR VALUE(ctc)  "Limits for " + icLimitTarget + " " + icLimitTargetID 
FRAME sel.


/* frame to list a limit type */
FORM    
    UserLimit.LimitTarget LABEL "Applied to ... "
    UserLimit.LimitTargetID NO-LABEL FORMAT "X(10)" SKIP
    
    UserLimit.LimitType LABEL "Type " 
    TMSCodes.CodeName SKIP

    UserLimit.LimitAmt LABEL "Amount " FORMAT "->>>9.99"

WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.




form /* SEEK Code */
    ob-code
    help "Enter Type "
    with row 4  col 2 title color value(ctc) " Find Type "
    color value(cfc) no-labels overlay frame hayr.


/* main */
cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first. 

IF AVAILABLE TMSCodes THEN ASSIGN
   memory       = recid(TMSCodes)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No user limits type defined !" VIEW-AS ALERT-BOX.
      HIDE FRAME sel NO-PAUSE.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

 IF must-add THEN DO:  /* Add / update /delete UserLimit  */
      ASSIGN cfc = "lis" ufkey = true  must-add = FALSE.
      RUN ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        
        REPEAT TRANSACTION WITH FRAME lis:
           PAUSE 0.
           CLEAR FRAME lis NO-PAUSE.
           
           IF llIsNew THEN DO:

              CREATE UserLimit.
              ASSIGN UserLimit.Brand      = gcBrand 
                     UserLimit.LimitType  = INT(TMSCodes.CodeValue)
                     UserLimit.LimitTarget = icLimitTarget
                     UserLimit.LimitTargetID = icLimitTargetID.
           END.
          
          
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0
           THEN UNDO add-row, LEAVE add-row.
           

           LEAVE ADD-ROW.
   
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TMSCodes THEN LEAVE LOOP.
      ASSIGN
        memory = recid(TMSCodes)
       xrecid = memory.

      NEXT LOOP.
   END. /* end if must-add */


   PrintPage:
   DO :
      IF must-print THEN DO:
        clear frame sel all no-pause.
        RUN local-find-first.  
        /* Print to screen */
         rtab = ?.
         do while frame-line<= frame-down and available TMSCodes:
            run local-disp-row.
            rtab[frame-line] = recid(TMSCodes).
            down with frame sel.
            run local-find-next.
         end.
         must-print = false.
         up frame-line(sel) - 1 with frame sel.
      END. /* must-print = TRUE */    

      if ufkey then do:
         assign
         ufk = 0 ufk[1] = 35 ufk[5] = 11
         ufk[6] = 0 ufk[8] = 8  ufk[9] = 1
         ehto = 3 ufkey = false.
         run ufkey.p.
      end.

   END. /* PrintPage */

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

 
     HIDE MESSAGE NO-PAUSE.     
     CHOOSE ROW TMSCodes.CodeValue ;(uchoose.i;) NO-ERROR WITH FRAME sel.
     COLOR DISPLAY VALUE(ccc) TMSCodes.CodeValue WITH FRAME sel.

      /* clean variable */
      llIsNew = FALSE.

      nap = keylabel(lastkey).
      if frame-value = "" and rtab[frame-line] = ? and
            lookup(nap,"8,f8") = 0
      then next.

         /* previous line */
         if lookup(nap,"cursor-up") > 0 then do
         with frame sel:
            if frame-line = 1 then do:
               RUN local-find-this.
               RUN local-find-PREV.
               if not available TMSCodes then do:
                  bell.
                  message "You are on 1st row !".              
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* a previous one was found */
                  scroll down.
                  do i = 11 to 2 by -1:
                     rtab[i] = rtab[i - 1].
                  end.
                  RUN local-disp-row.
                  rtab[frame-line] = recid(TMSCodes).
                  memory = recid(TMSCodes).
               end.
            end.
            else up 1.
         end. /* previous line */

         /* next line */
         if lookup(nap,"cursor-down") > 0 then do with frame sel:
            if frame-line = frame-down then do:
                RUN local-find-this.
                 RUN local-find-NEXT.
               if not available TMSCodes then do:
                  bell.
                  message "You are on last row !".
                  pause 1 no-message.
                  next BROWSE.
               end.
               else do:
                  /* yet another record was found */
                  scroll up.
                  do i = 1 to 10:
                     rtab[i] = rtab[i + 1].
                  end. 
                  RUN local-disp-row.
                  rtab[frame-line] = recid(TMSCodes).
                  /* finally last line's KeyValue is saved */
                  memory = rtab[1].
               end.
            end.
            else down 1 .
         end. /* next line */

         /* previous page */
         else if lookup(nap,"page-up,prev-page") > 0 then do with frame sel:
            find TMSCodes where recid(TMSCodes) = memory no-lock no-error.
            RUN local-find-prev.
            if available TMSCodes then do:

               do i = 1 to (frame-down - 1):
                  RUN local-find-prev.
                  if available TMSCodes then memory = recid(TMSCodes).
                  else i = frame-down.
               end.
               must-print = true.
               next LOOP.
            end.
            else do:
               /* this is the first data page */
               bell.
               message "This is the 1st page !".          
               pause 1 no-message.
            end.
        end. /* previous page */

        /* next page */
        else if lookup(nap,"page-down,next-page") > 0 then do with frame sel:
           if rtab[frame-down] = ? then do:
               bell.
               message "This is the last page !".
               pause 1 no-message.
           end.
           else do: /* the downmost line wasn't empty */
               memory = rtab[frame-down].
               must-print = true.
               next LOOP.
           end.
        end. /* next page */

        /* Seek */
        if lookup(nap,"1,f1") > 0 then do:  /* ob-code */
           cfc = "puyr". run ufcolor.
           ehto = 9. run ufkey. ufkey = true.
           update ob-code with frame hayr.
           hide frame hayr no-pause.
           if ob-code ENTERED then do: 
              FIND TMSCodes  USE-INDEX TableName  WHERE  
               TMSCodes.TableName = "UserLimit" AND
               TMSCodes.FieldName = "LimitType" AND
               TMSCodes.CodeValue = ob-code AND
               TMSCodes.InUse = 1 NO-LOCK NO-ERROR.  

              if not available TMSCodes then do:
                       bell.
                       message "None found !".    
                       pause 1 no-message.
                       next BROWSE.
              end.
              /*  ob-code was found */
              assign
                memory = recid(TMSCodes)
                must-print = true.
           end.
           next LOOP.
        end. /* Seek */

        /* Choose */
        else if lookup(nap,"return,enter,5,f5") > 0  and lcRight = "RW" then do:
            /* change or add new */ 
            RUN local-find-this.
            RUN find-this-limit(FALSE).
            IF NOT AVAIL UserLimit THEN /* add */
               llIsNew = TRUE.
            ELSE  /* change */
               llIsNew = FALSE.
            must-add = TRUE.
            NEXT LOOP.

        end. /* Choose */
 
        /* First record */
        else if lookup(nap,"home,h") > 0 then do:
           RUN local-find-FIRST.
           memory = recid(TMSCodes).
           must-print = true.
           next LOOP.
        end. /* First record */

        /* last record */
        else if lookup(nap,"end,e") > 0 then do : 
           RUN local-find-LAST.
           memory = recid(TMSCodes).
           must-print = true.
           next LOOP.
        end. /* last record */

        else if nap = "8" or nap = "f8" then leave LOOP. /* Return */

  END.  /* BROWSE */

  
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.


PROCEDURE find-this-limit:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
      FIND TMSCodes  WHERE recid(TMSCodes) = rtab[frame-line(sel)].   
      FIND UserLimit WHERE UserLimit.Brand = gcBrand AND
                     UserLimit.LimitType = INT(TMSCodes.CodeValue) AND
                     UserLimit.LimitTarget = icLimitTarget AND
                     UserLimit.LimitTargetID = icLimitTargetID
      EXCLUSIVE-LOCK.
    END.
    ELSE DO:
      FIND TMSCodes  WHERE recid(TMSCodes) = rtab[frame-line(sel)] .
      FIND UserLimit WHERE UserLimit.Brand = gcBrand AND
                     UserLimit.LimitType = INT(TMSCodes.CodeValue) AND
                     UserLimit.LimitTarget = icLimitTarget AND
                     UserLimit.LimitTargetID = icLimitTargetID
      NO-LOCK.
    END.
END PROCEDURE.


PROCEDURE findlimitAmt:

   lclimitAmt = "not defined".

   FIND UserLimit WHERE UserLimit.Brand = gcBrand AND
                  UserLimit.LimitType = INT(TMSCodes.CodeValue) AND
                  UserLimit.LimitTarget = icLimitTarget AND
                  UserLimit.LimitTargetID = icLimitTargetID NO-LOCK NO-ERROR.

    IF AVAIL UserLimit THEN 
       lclimitAmt = STRING(UserLimit.LimitAmt).
    
    ELSE IF icLimitTarget = "TMSUser" THEN DO:

       FIND TMSUser WHERE TMSUser.UserCode = icLimitTargetID NO-LOCK NO-ERROR.

       FIND UserLimit WHERE UserLimit.Brand = gcBrand AND
                      UserLimit.LimitType = INT(TMSCodes.CodeValue) AND
                      UserLimit.LimitTarget = "UserGroup" AND
                      UserLimit.LimitTargetID = TMSUser.UserGroup NO-LOCK NO-ERROR.

        IF AVAIL UserLimit THEN  lclimitAmt = STRING(UserLimit.LimitAmt).
    END.
    
END PROCEDURE.

PROCEDURE local-find-this:

    FIND TMSCodes  WHERE recid(TMSCodes) = rtab[frame-line(sel)].   

    IF AVAIL TMSCodes THEN RUN findlimitAmt.
END PROCEDURE.


PROCEDURE local-find-FIRST:
     
    FIND FIRST TMSCodes  USE-INDEX TableName  WHERE  
               TMSCodes.TableName = "UserLimit" AND              
               TMSCodes.FieldName = "LimitType" AND
               TMSCodes.InUse = 1 NO-LOCK NO-ERROR.  

    IF AVAIL TMSCodes THEN RUN findlimitAmt. 

END PROCEDURE.


PROCEDURE local-find-LAST:

    FIND LAST  TMSCodes  USE-INDEX TableName  WHERE  
               TMSCodes.TableName = "UserLimit" AND
               TMSCodes.FieldName = "LimitType" AND
               TMSCodes.InUse = 1 NO-LOCK NO-ERROR.  

    IF AVAIL TMSCodes THEN RUN findlimitAmt. 
    
END PROCEDURE.

PROCEDURE local-find-NEXT:

    FIND NEXT  TMSCodes  USE-INDEX TableName  WHERE  
               TMSCodes.TableName = "UserLimit" AND
               TMSCodes.FieldName = "LimitType" AND
               TMSCodes.InUse = 1 NO-LOCK NO-ERROR.  

    IF AVAIL TMSCodes THEN RUN findlimitAmt. 

END PROCEDURE.

PROCEDURE local-find-PREV:

    FIND PREV  TMSCodes  USE-INDEX TableName  WHERE  
               TMSCodes.TableName = "UserLimit" AND
               TMSCodes.FieldName = "LimitType" AND
               TMSCodes.InUse = 1 NO-LOCK NO-ERROR.

    IF AVAIL TMSCodes THEN RUN findlimitAmt. 

END PROCEDURE.



PROCEDURE local-disp-row:

   CLEAR FRAME sel NO-PAUSE.        
   DISPLAY
       TMSCodes.CodeValue
       TMSCodes.CodeName
       lcLimitAmt 
   WITH FRAME sel. 

END PROCEDURE.


PROCEDURE local-update-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      DISP
         UserLimit.LimitTarget
         UserLimit.LimitTargetID
         UserLimit.LimitType
         TMSCodes.CodeName
         UserLimit.LimitAmt WHEN llIsAdmin
      WITH FRAME lis.

      IF NOT llIsNew THEN DO:
         ASSIGN ehto   = 0
                ufk    = 0            
                ufk[1] = 7 WHEN lcRight = "RW" AND llIsAdmin 
                ufk[3] = 4 WHEN lcRight = "RW" AND UserLimit.LimitTarget = icLimitTarget
                ufk[8] = 8.
             
         RUN ufkey.
      END.
      ELSE toimi = 1.
      
      IF toimi = 1 AND lcRight = "RW" THEN DO:
       
        IF UserLimit.LimitTarget <> icLimitTarget THEN DO:
             ASSIGN ok = FALSE.
             MESSAGE " User is already using the value defined for the "
                     + UserLimit.LimitTarget + " " + UserLimit.LimitTargetID  + 
                     ". Do you want to define own values for the user ? " VIEW-AS ALERT-BOX 
                     BUTTONS YES-NO TITLE "WARNING" UPDATE ok .
             IF ok THEN DO:

                 /* create a new user limit */
                 CREATE UserLimit.
                 ASSIGN UserLimit.Brand      = gcBrand 
                        UserLimit.LimitType  = INT(TMSCodes.CodeValue)
                        UserLimit.LimitTarget = icLimitTarget
                        UserLimit.LimitTargetID = icLimitTargetID
                        llIsNew = TRUE.
                 NEXT.

             END.
             ELSE LEAVE.

         END.

         ehto = 9.
         RUN ufkey.

         IF NOT llIsNew AND llDoEvent THEN RUN StarEventSetOldBuffer(lhUserLimit).

         FIND CURRENT UserLimit EXCLUSIVE-LOCK.
         
         UPDATE
            UserLimit.LimitAmt WHEN llIsAdmin
         WITH FRAME lis EDITING:
            READKEY.
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
               PAUSE 0. 
               IF INT(FRAME-VALUE) < 0 THEN DO:
                   MESSAGE " Give only positive values !"
                   VIEW-AS ALERT-BOX ERROR.
                   NEXT.
               END.

            END.

            APPLY LASTKEY.
                  
         END. /* EDITING */
        IF NOT llIsNew AND llDoEvent THEN RUN StarEventMakeModifyEvent(lhUserLimit).
         
      END.

      IF toimi = 3 AND lcRight = "RW" THEN DO:
        
         /* we should check the the user limit correspond to same limit target */
         IF UserLimit.LimitTarget <> icLimitTarget THEN DO:
            MESSAGE "You can not delete a limit defined at "
                     + UserLimit.LimitTarget + " level in this dialog" 
                     VIEW-AS ALERT-BOX ERROR.
                     LEAVE.
         END.
         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
         IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhUserLimit).
           FIND CURRENT UserLimit EXCLUSIVE-LOCK.
           DELETE UserLimit.
           LEAVE.
          END. 
      END.
      
      LEAVE.
   END.
END PROCEDURE.


