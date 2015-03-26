/* ----------------------------------------------------------------------
  MODULE .......: checkmsbarring.p
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 01.11.07
  CHANGED ......: 02.06.08 Completely new version
  Version ......: xfera
----------------------------------------------------------------------- */

/* ---------------------------------------------------------------------
   Input.:  MsSeq for MsRequest
   Output:  ocCmdList for list of possible commands for barring 
   Output:  ocStatus for displaying mobsub barring status in mobsub.p
----------------------------------------------------------------------- */
{commali.i}
{barrfunc.i}


DEFINE INPUT PARAMETER  iiMsSeq     AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER  icKatun     AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER ocCmdList   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocStatus    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcReturn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCliType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lrCLB     AS RECID     NO-UNDO.
DEFINE VARIABLE lcLevel   AS CHARACTER NO-UNDO.

FIND MobSub NO-LOCK WHERE 
     MobSub.MsSeq = iiMsSeq
NO-ERROR.

IF NOT AVAIL MobSub THEN LEAVE.
lcCliType = MobSub.CliType.


/* Get barrings allowed depending on user group */
lcLevel   = fUserGroup(icKatun).   /* user rights (C_ or C_|Y_ or C_|Y_|D_)  */
lcReturn  = fCheckStatus(iiMsSeq). /* Last barring package applied */
lrCLB     = fCheckRestore(iiMsSeq,"CLB").    /* Check if CLB exists          */

/* This case will determine what barrings will be shown to barr browser */

/*
   CustCare level will only see actions available to CC level, packages
   begining with C_ when admin level barring is not applied:
   
   - If subscription has operator level barring, no actions are allowed to
     CustCare level

   Admin level will see all barring packages, with few exeptions:

   - If subscription has previously set CLB, and current barring is OLB 
     (ReqCparam2 has value for CLB)
     no CLB changes are allowed, CLB must be unbarred until further CLB
     handling is allowed. UN-barring will be shown last at the barring
     packages list

   - If CLB is currently the main barring, full list will be shown
     to admin level
*/

CASE lcReturn:
   
  /* NO current barring, get list of allowed barring components */
   WHEN "OK" THEN DO:
      
      ASSIGN ocCmdList = fGetSpac(lcCliType,lcReturn,lcLevel) /* Packet range */
             ocStatus  = "NONE".                     
   
   END.
   
   /* Barring denied, show in mobsub.p status display */
   WHEN "91" THEN ASSIGN ocCmdList = fGetSpac(lcCliType,lcReturn,lcLevel) /* Packet range */
                         ocStatus  = "ONC".
    
   /* Return all other barrings + counter action for current barring */
   OTHERWISE DO:

      /* Check for last_gen barring */
      IF INDEX(lcReturn,"_") > 0 THEN DO: /* This is next-gen */

         /* Check permissions */
         IF (LOOKUP("C_",lcLevel,"|") = 0 AND lcReturn BEGINS "C_") OR
            (LOOKUP("Y_",lcLevel,"|") = 0 AND lcReturn BEGINS "Y_") OR
            (LOOKUP("D_",lcLevel,"|") = 0 AND lcReturn BEGINS "D_") THEN DO:
            
            ASSIGN ocCmdList = ""  
                   ocStatus  = "NAD".
         END.
         
         /* DLB is set, only removal of DLB or DLB change is possible*/
         ELSE IF lcReturn BEGINS "D_" THEN DO:
            ASSIGN ocCmdList = fGetSpac(lcCliType,lcReturn,"D_") +
                                        "|UN" + lcReturn
                                        ocStatus = lcReturn.
         END.

         /* Pending CLB, only ADMIN level barrings allowed + unbarring of 
            current */
         ELSE IF (lrCLB NE ? AND lrCLB NE 0) AND lcReturn BEGINS "Y_" THEN DO:
            ASSIGN ocCmdList = fGetSpac(lcCliType,lcReturn,"Y_" + 
                   (IF LOOKUP("D_",lcLevel,"|") > 0 THEN "|D_" ELSE "")) +
                                        "|UN" + lcReturn
                                        ocStatus = lcReturn.
         END.
        
         /* Current barring is customer level or operator level, all barring 
            packages allowed + unbarring of current CLB */
         ELSE DO:
           ASSIGN ocCmdList = fGetSpac(lcClitype,lcReturn,lcLevel) +
                              "|UN" + lcReturn 
                              ocStatus  = lcReturn.
         END.
            
      END. /* next_gen */
      
      /* Last-gen handling */
      ELSE DO:
         ASSIGN ocCmdList = fGetSpac(lcCliType,lcReturn,lcLevel) +
                            "|UN" + lcReturn
                            ocStatus  = lcReturn.
      END. 
   END. /* Otherwise */
   
END CASE. 
                                                   
IF ocCmdList BEGINS "|" THEN
   ocCmdList = SUBSTRING(ocCmdList,2).
