{commali.i}
{barrfunc.i}
{timestamp.i}

DEFINE INPUT PARAMETER iiMsSeq    AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER icPackage  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icSource   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icCreator  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER idActStamp AS DECIMAL   NO-UNDO.
DEFINE INPUT PARAMETER icSMSText  AS CHARACTER NO-UNDO. 
DEFINE OUTPUT PARAMETER ocStatus  AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lrCLBReq AS RECID     NO-UNDO.
DEFINE VARIABLE liNewRequest  AS INTEGER NO-UNDO.
DEFINE VARIABLE liSubRequest  AS INTEGER NO-UNDO.
DEFINE VARIABLE ldtAction     AS DATE    NO-UNDO.
DEFINE VARIABLE liTime        AS INT     NO-UNDO.
DEFINE VARIABLE lcTag         AS CHAR    NO-UNDO.
DEFINE VARIABLE lrCurrentBarring AS ROWID NO-UNDO.
DEFINE VARIABLE lcOldBarring AS CHARACTER NO-UNDO. 

DEF BUFFER bRestoreReq FOR MsRequest.

FUNCTION fIsOLBMarked RETURNS LOGIC
   (icCLIType  AS CHAR,
    icPackage  AS CHAR,
    idtActDate AS DATE):
    
   IF NOT icPackage BEGINS "Y_" THEN RETURN FALSE.
   
   /* if this package is used in the activation, then it is marked as olb
      also when it is used manually */
   FOR FIRST RequestAction NO-LOCK USE-INDEX CLIType WHERE
             RequestAction.Brand      = gcBrand     AND
             RequestAction.CLIType    = icCLIType   AND
             RequestAction.ReqType    = 13          AND
             RequestAction.ValidTo   >= idtActDate  AND
             RequestAction.ValidFrom <= idtActDate  AND
             RequestAction.ActionType = "CTServPac" AND
             RequestAction.ActionKey  = icPackage:
      RETURN TRUE.
   END.

   RETURN FALSE.
    
END FUNCTION.


FIND MobSub WHERE
     MobSub.MsSeq = iiMsSeq
NO-LOCK NO-ERROR.

IF NOT AVAIL MobSub THEN RETURN. 

fSplitTS(idActStamp,
         OUTPUT ldtAction,
         OUTPUT liTime).
         
/* Check current barring status, to determine flow of packages */

lcReturn = fCheckBarrStatus(iiMsseq, OUTPUT lrCurrentBarring). 

CASE lcReturn:
   /* Error detected, ongoing nework commands */
   WHEN "91" THEN DO: 
      ocStatus = "ONC".
      RETURN.
   END.

   /* No previous barrings, proceed as normal */
   WHEN "OK" THEN DO:

      lcTag = "".
      
      /* If request is CLB */
      IF icPackage BEGINS "C_" THEN lcTag = "CLB".
      
      ELSE IF fIsOLBMarked(MobSub.CLIType,
                           icPackage,
                           ldtAction) THEN lcTag = "OLB".
      
      fCreateBP(icPackage,
                MobSub.InvCust,
                MobSub.Cli,
                MobSub.MsSeq,
                lcTag,
                0,
                icSource,
                icCreator,
                idActStamp,
                icSMSText,
                "",
                OUTPUT liNewRequest).
   
   END. /* END "OK" */

   /* Special handling, previous barrings exist */
   OTHERWISE DO:
     
      IF lcReturn BEGINS "D_" AND
         (NOT icPackage BEGINS "D_" AND
          NOT icPackage BEGINS "UND_") THEN DO:
         ocStatus = "NAD".
         RETURN.
      END.
       
      /* Next-Gen handling (_)*/
      IF INDEX(icPackage,"_") > 0 THEN DO:
         
         /* Removal requests "UN" */
         
         IF icPackage BEGINS "UNC_" THEN DO:
            /* Customer level package, can be creation or removal */
            fCreateBP(icPackage,      /* barring package */        
                      MobSub.InvCust,
                      MobSub.CLI,
                      MobSub.MsSeq,
                      "",             /* CLB */
                      0,              /* Set reqid for requester */
                      "5",            /* source from automation */
                      icCreator,
                      idActStamp,
                      icSMSText,
                      "",
                      OUTPUT liNewRequest). /* Get reqid for main req */

            /* GET RECID and Remove CLB tag from pending */
            lrCLBReq = fCheckRestore(iiMsSeq,"CLB").       
            IF lrCLBReq NE ? THEN DO:
               FIND bRestoreReq EXCLUSIVE-LOCK WHERE
                    RECID(bRestoreReq) = lrCLBReq.
            
               ASSIGN bRestoreReq.ReqCParam2 = "".
               RELEASE bRestoreReq.
            END.
            
            /* is there an olb barring that should be restored */
            RUN pRestoreOLB(icPackage,liNewRequest).
            
         END. /* End CLB removal */
         
         /* When OLB is removed, CLB must be checked if 
            it must be put back on */
         
         ELSE IF icPackage BEGINS "UNY_" OR
                 icPackage BEGINS "UND_" THEN DO:   

            /* first create removal package */
            fCreateBP(icPackage,
                      MobSub.InvCust,
                      MobSub.Cli,
                      MobSub.MsSeq,
                      "",
                      0,
                      icSource,
                      icCreator,
                      idActStamp,
                      icSMSText,
                      "",
                      OUTPUT liNewRequest).

            /* Restore barring that Debt barring has overwritten */
            IF icPackage BEGINS "UND_" AND
               lrCurrentBarring NE ? THEN DO:

               FIND bRestoreReq NO-LOCK WHERE
                  ROWID(bRestoreReq) = lrCurrentBarring.

               IF bRestoreReq.ReqCParam4 NE "" THEN DO:
                  fCreateBP(bRestoreReq.ReqCParam4,
                            MobSub.InvCust,
                            MobSub.Cli,
                            MobSub.MsSeq,
                            "",
                            liNewRequest,
                            "5",
                            icCreator,
                            idActStamp,
                            "",
                            "",
                            OUTPUT liSubRequest).
                  ocStatus = STRING(liNewRequest).
                  LEAVE.
               END.
            END.
            
            /* check if there is a clb package that should be restored */ 
            lrCLBReq = fCheckRestore(iiMsSeq,"CLB").

            IF lrCLBReq NE ? THEN DO:
               
               /* Remove old CLB */
               FIND bRestoreReq EXCLUSIVE-LOCK WHERE
                    RECID(bRestoreReq) = lrCLBReq.
               ASSIGN bRestoreReq.ReqCParam2 = "".
               
               fCreateBP(bRestoreReq.ReqCparam1, /* From transferring clb */
                         MobSub.InvCust,
                         MobSub.Cli,
                         MobSub.MsSeq,
                         "CLB",
                         liNewRequest, /* assign origreq, will be mandatory */
                         "5",
                         icCreator,
                         idActStamp,
                         "",
                         "",
                         OUTPUT liSubRequest).
               RELEASE bRestoreReq.
            END. /* CLB handling */

            /* otherwise restore possible olb barring */
            ELSE RUN pRestoreOLB(icPackage,liNewRequest).
 
         END.

          
         /* Set CLB, OLB or DLB barring on */ 
         ELSE DO:
            
            /* CLB change from one to another, CLB tag must be moved to
               new CLB */
         
            IF icPackage BEGINS "C_" THEN DO:
               
               /* Remove CLB from old CLB IF THERE IS ANY */
               lrCLBReq = fCheckRestore(iiMsSeq,"CLB").
               
               IF lrCLBReq NE ? THEN DO:
                  
                  FIND bRestoreReq EXCLUSIVE-LOCK WHERE 
                       RECID(bRestoreReq) = lrCLBReq.
                  ASSIGN bRestoreReq.ReqCParam2 = "".
                  RELEASE bRestoreReq.
               END.
                
               /* And "move" it to new */
               fCreateBP(icPackage,
                         MobSub.InvCust,
                         MobSub.Cli,
                         MobSub.MsSeq,
                         "CLB",  /* <- move */
                         0, /* sub-request id */
                         icSource,
                         icCreator,
                         idActStamp,
                         icSMSText,
                         "",
                         OUTPUT liNewRequest).
                        
            END.
            /* DLB Change */
            ELSE IF icPackage BEGINS "D_" THEN DO:
            
            /* Create removal of current barring (could be last-gen )
               and set new barring package pending */
               
               /* Remember previous barring */
               IF lcReturn BEGINS "D_" THEN DO:
                  FIND bRestoreReq NO-LOCK WHERE
                     ROWID(bRestoreReq) = lrCurrentBarring.
                  lcOldBarring = bRestoreReq.ReqCParam4.
               END.
               ELSE lcOldBarring = lcReturn.

               IF lcReturn EQ "D_HOTL" OR
                  icPackage EQ "D_HOTL" THEN
               fCreateBP("UN" + lcReturn,
                         MobSub.InvCust,
                         MobSub.Cli,
                         MobSub.MsSeq,
                         "",
                         0,
                         "5", /* source from automation */
                         icCreator,
                         idActStamp,
                         "",
                         "",
                         OUTPUT liSubRequest).
               ELSE liSubRequest = 0.
 
               /* Create new DLB */
               fCreateBP(icPackage,
                         MobSub.InvCust,
                         MobSub.Cli,
                         MobSub.MsSeq,
                         "",
                         liSubRequest, /* sub-request id */
                         icSource,
                         icCreator,
                         idActStamp,
                         icSMSText,
                         lcOldBarring, /* remember old barring */
                         OUTPUT liNewRequest).
            END.
            /* Create removal of current barring (could be last-gen )
               and set new barring package pending */
            ELSE DO: 

               IF fIsOLBMarked(MobSub.CLIType,
                               icPackage,
                               ldtAction) 
               THEN lcTag = "OLB".
               ELSE lcTag = "".
                
               /* Create new OLB */
               fCreateBP(icPackage,
                         MobSub.InvCust,
                         MobSub.Cli,
                         MobSub.MsSeq,
                         lcTag,
                         0, /* sub-request id */
                         icSource,
                         icCreator,
                         idActStamp,
                         icSMSText,
                         "",
                         OUTPUT liNewRequest).
                           
            END. /* Change OLB/LGB */
         END. /* Change */
      END. /* Next-gen handling */
      
      ELSE DO:
         /* Last-Gen barring change / removal */
         fCreateBP(icPackage,
                   MobSub.InvCust,
                   MobSub.Cli,
                   MobSub.MsSeq,
                   "",
                   0, /* sub-request id */
                   icSource,
                   icCreator,
                   idActStamp,
                   icSMSText,
                   "",
                   OUTPUT liNewRequest). 
      END.   
   END.
END.

ocStatus = STRING(liNewRequest).


PROCEDURE pRestoreOLB:

   DEF INPUT PARAMETER icNewPackage AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiWaitForReq AS INT  NO-UNDO.
    
   DEFINE VARIABLE lrOLBReq AS RECID NO-UNDO.
   
   /* is there an olb barring that should be restored */
   lrOLBReq = fCheckRestore(iiMsSeq,"OLB").

   IF lrOLBReq NE ? THEN DO:
      
      FIND bRestoreReq WHERE
           RECID(bRestoreReq) = lrOLBReq NO-LOCK.
      
      /* Check if restored olb barring is supported for Mobsub.CLIType */
      FIND FIRST CTServPac NO-LOCK WHERE
                 CTServPac.Brand     = gcBrand   AND
                 CTServPac.CLIType   = MobSub.CLIType AND
                 CTServPac.ServPac   = bRestoreReq.ReqCParam1 AND
                 CTServPac.FromDate <= ldtAction AND
                 CTServPac.ToDate   >= ldtAction NO-ERROR.
      
      /* don't restore if this an unbarring of the same package 
         or barring is not compatible with CLIType */
      IF icNewPackage = "UN" + bRestoreReq.ReqCparam1 OR
         NOT AVAIL CTServPac THEN DO:

         /* barring knowingly removed -> remove olb tag */
         FIND CURRENT bRestoreReq EXCLUSIVE-LOCK.
         bRestoreReq.ReqCparam2 = "".
         RELEASE bRestoreReq.
      END.

      /* restore olb barring */
      ELSE DO:
         fCreateBP(bRestoreReq.ReqCparam1, 
                   MobSub.InvCust,
                   MobSub.Cli,
                   MobSub.MsSeq,
                   "OLB",           /* request handler removes old olb tag */
                   iiWaitForReq,    /* request that must be completed first */
                   "5",
                   icCreator,
                   idActStamp,
                   "",             /* sms */
                   "",
                   OUTPUT liSubRequest).
      END.
   END.

END PROCEDURE.


