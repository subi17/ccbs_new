
/* ----------------------------------------------------------------------
  MODULE .......: provmaint.p 
  TASK .........: Set maintenance break on (HLR-actions) -> sogrequest.p
                  will be "stopped".. 
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 14.01.08
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/tmsparam4.i}
{Func/cparam2.i}
{Mc/provmaint.i}

session:system-alert-boxes = true.

DEF VAR lcAskPwd   AS CHARACTER NO-UNDO.
DEF VAR lcStatus   AS CHARACTER NO-UNDO EXTENT 2.
DEF VAR liMaintB   AS INTEGER   NO-UNDO.
DEF VAR lcPassWord AS CHARACTER NO-UNDO FORMAT "x(15)".
DEF VAR liMinutes  AS INTEGER   NO-UNDO FORMAT "zzz9".
DEF VAR lcBeg      AS CHARACTER NO-UNDO.
DEF VAR lcEnd      AS CHARACTER NO-UNDO.
DEF VAR ldFrom     AS DEC       NO-UNDO.
DEF VAR ldTo       AS DEC       NO-UNDO.
DEF VAR lcMessage  AS CHARACTER NO-UNDO.

DEF VAR lcOnOff    AS CHARACTER NO-UNDO EXTENT 5.

ASSIGN lcStatus[1] = "On"
       lcStatus[2] = "Off"
       ldFrom   = fCParamDe4("1","sogrequest","MaintBreakFrom")
       ldTo     = fCparamDe4("1","sogrequest","MaintBreakTo")
       lcBeg    = STRING(INT(ldFrom),"HH:MM:SS")
       lcEnd    = STRING(INT(ldTo),"HH:MM:SS")
       lcAskPwd = fCParamC("AdminUser").

FORM
    "  Solog handling is currently.:" lcStatus FORMAT "x(3)"
    WITH ROW 4 OVERLAY
    CENTERED
    COLOR VALUE(ccc)
    TITLE "   PROVISIONING   "
    NO-LABEL
FRAME frProvision.

FORM 
    SKIP(1)
    "   Enter duration of maintenance break in minutes:" liMinutes "   " 
    SKIP(1)
    "   Current time frame:" lcBeg "TO" 
                             lcEnd SKIP(1)
    WITH ROW 10 OVERLAY
    CENTERED
    NO-LABEL

FRAME frDuration.

FORM
    "Shutting down solog handling requires password!" SKIP(1)
    "Enter password:" lcPassWord
    WITH ROW 4 OVERLAY
    CENTERED
    COLOR VALUE(ccc)
    TITLE " PASSWORD "
    NO-LABEL
FRAME frPassWord.

UPDATE lcPassWord BLANK WITH FRAME frPassWord.
IF lcPassWord NE lcAskPwd THEN DO:
   MESSAGE "Incorrect password, cannot continue!" VIEW-AS ALERT-BOX.
   PAUSE 0.
   HIDE FRAME frPassWord.
   LEAVE.
END.

HIDE FRAME frPassWord.
liMaintB = INT(fCParamC4(gcBrand,"ServiceBreak","Activation")).
/* Display current status of solog handling */
DISP lcStatus[liMaintB + 1]  WITH FRAME frProvision.

PAUSE 0.

/* Show selection box */

DO WHILE TRUE:

   ASSIGN ufk    = 0 
          ufk[8] = 8 
          ehto = 3. 
   
   RUN ufkey.
   
   DISP "      On        " @ lcOnOff[1]
        "      Off       " @ lcOnOff[2]
        "    Duration    " @ lcOnOff[3]
        "----------------" @ lcOnOff[4]
        "    Cancel      " @ lcOnOff[5]
   WITH OVERLAY FRAME choices NO-LABELS.

   CHOOSE FIELD lcOnOff AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " SET HANDLING " CENTERED ROW 7 WITH 1 COL.
   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.
   
   IF FRAME-INDEX EQ 1 THEN DO:
      /* Solog handler still is running (0 = maint break = off) */
      IF liMaintB = 0 THEN DO:
         MESSAGE "Solog handling is already on!" VIEW-AS ALERT-BOX.
         NEXT.
      END.
      ELSE DO:
         
         fUpdateMaintBreak(0,0,0). /* Timeframe to zero, service break
                                          parameter to zero (start sog_request)
                                          (servicebreak parameter to 0)
                                       */
         liMaintB = 0.
         lcMessage = "Solog handling was restarted at: "       + 
                     STRING(TIME,"HH:MM:SS")                  + 
                     CHR(10)                                   + 
                     "User responsible of this action: "       + 
                     katun.
         
         MESSAGE "Solog handling has been restarted." 
         VIEW-AS ALERT-BOX.

         RUN pMailMaintBreak(lcMessage).
         
         HIDE FRAME Choices.
         HIDE FRAME frProvision.
         LEAVE.
         
      END.
      
   END.
   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      /* Maint break is on, solog handlin is stopped (1 = maint break = on) */
      IF liMaintB = 1 THEN DO:
         MESSAGE "Solog handling is already off!" VIEW-AS ALERT-BOX.
         NEXT.
      END.
      ELSE DO:
         DISP lcBeg lcEnd WITH FRAME frDuration.
         UPDATE liMinutes WITH FRAME frDuration.
         liMinutes = INPUT FRAME frDuration liMinutes.
         IF liMinutes > 130 THEN DO:
         
            MESSAGE "Maximum duration is 130 minutes, action cancelled" 
            VIEW-AS ALERT-BOX.
            HIDE FRAME frDuration.
            NEXT.
         END.
         
         /* Update tmsparam so that sog_request stops handling sologs */
         
         ASSIGN ldFrom = TIME
                ldTo   = TIME + (liMinutes * 60).
         
         fUpdateMaintBreak(ldFrom,ldTo,1).
         liMaintB = 1.
         
         ASSIGN lcBeg = STRING(INT(ldFrom),"HH:MM:SS")
                lcEnd = STRING(INT(ldTo),"HH:MM:SS").
         
         lcMessage = "Notification on solog handling "      +
                     "maintenance break: From "             +
                     lcBeg                                  + 
                     " to "                                 + 
                     lcEnd                                  + 
                     CHR(10)                                + 
                     "Solog handling is currently stopped!" + 
                     CHR(10)                                +
                     "User responsible of this action: "    + katun.
         
         RUN pMailMaintBreak(lcMessage).

         MESSAGE "SNS backend has been notified of this action." 
         VIEW-AS ALERT-BOX.
         
         HIDE FRAME frDuration.
         LEAVE.
      END.
   END.
   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      /* Check if maint break is on. If so, then send parameters to issue
         cretor to extend time window of nagios and comment on AIC */
      
      /* Duplicate code, could be removed later ;) */   
      
      IF liMaintB = 1 THEN DO:
         /* Duration change allowed, break is on */
         DISP lcBeg lcEnd WITH FRAME frDuration.
         UPDATE liMinutes WITH FRAME frDuration.
         liMinutes = INPUT FRAME frDuration liMinutes.
         IF liMinutes > 130 THEN DO:
            
            MESSAGE "Maximum duration is 130 minutes" VIEW-AS ALERT-BOX.
            HIDE FRAME frDuration.
            NEXT.
         END.
         
         HIDE FRAME frDuration.
         
         ASSIGN ldFrom = TIME
                ldTo   = TIME + (liMinutes * 60).

         fUpdateMaintBreak(ldFrom,ldTo,1).
         liMaintB = 1.
        

         ASSIGN lcBeg = STRING(INT(ldFrom),"HH:MM:SS")
                lcEnd = STRING(INT(ldTo),"HH:MM:SS").
         

         lcMessage = "Notification on solog handling "          +
                     "maintenance break duration change: From " +
                     lcBeg                                      +
                     " to "                                     +
                     lcEnd                                      +
                     CHR(10)                                    +
                     "Solog handling is currently stopped!"     +
                     CHR(10)                                    +
                     "User responsible of this action: "        + katun.         
         RUN pMailMaintBreak(lcMessage).
         
         MESSAGE "SNS backend has been notified of this action"
         VIEW-AS ALERT-BOX.
         
         LEAVE.
      END.
      
      
      ELSE DO:
         /* Nothing to extend, no maint break on */
         MESSAGE "Provisioning is still running,"
                 "no duration cannot be set!" VIEW-AS ALERT-BOX.
         NEXT.
      END.
   END.
   ELSE IF FRAME-INDEX EQ 5 THEN DO:
      HIDE FRAME Choices.
      HIDE FRAME frProvision.
      LEAVE.
   END.
   
END.

HIDE FRAME choices.
HIDE FRAME frProvision.
