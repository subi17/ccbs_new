/* ----------------------------------------------------------------------
  MODULE .......: COPYSP.P
  TASK .........: COPY and/or UPDATE a set of Services
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 02-05-99
  CHANGED ......: 06.07.99 pt PARAM batch
                  09.11.03 jp brand
                  05.11.04 jp setfees
                  10.12.04/aam copy elements from clitype (CTServEl),
                               input idtDate,
                               use pDefaultServices
                  24.08.05/aam solog-parameter to pDefaultServices             
                  30.11.06/aam input ilNewSubs
  Version ......: M15
  ---------------------------------------------------------------------- */
{commali.i}
{timestamp.i}
{ffeecont.i}
{service.i}

DEF INPUT  PARAMETER iiMsSeq   LIKE MobSub.MsSeq     NO-UNDO. 
DEF INPUT  PARAMETER idtDate   AS DATE               NO-UNDO. 
DEF INPUT  PARAMETER ilNewSubs AS LOG                NO-UNDO. 
DEF INPUT  PARAMETER batch     AS log                NO-UNDO.

DEF VAR i   AS I    NO-UNDO.   
DEF VAR j   AS I    NO-UNDO.   

FIND MobSub WHERE MobSub.MSSeq = iiMSSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN. 

/* copy packages from clitype */ 

/* new subscription */
IF ilNewSubs THEN DO:
   /* first the profile */ 
   RUN pCopyPackage(MobSub.CLIType,
                    "*",
                    "",
                    MobSub.MSSeq,
                    idtDate,
                    TRUE,    /* only those that don't already exist */
                    FALSE,   /* create fees */
                    FALSE,   /* solog (this is used for new mobsubs only) */
                    0,
                    FALSE,
                    OUTPUT i).
   
   RUN pCopyPackage(MobSub.CLIType,
                    "TMSService",
                    "",
                    MobSub.MSSeq,
                    idtDate,
                    TRUE,    /* only those that don't already exist */
                    FALSE,   /* create fees */
                    FALSE,   /* solog (this is used for new mobsubs only) */
                    0,
                    FALSE,
                    OUTPUT j).
   i = i + j.

   /* then the elements that have been defined as basic */
   fProfileExtention(MobSub.CLIType,
                     MobSub.MsSeq,
                     idtDate,
                     FALSE).  /* create fees */
END.

ELSE RUN pDefaultServices (MobSub.CLIType,
                           MobSub.MSSeq,
                           idtDate,
                           FALSE,   /* all changed ones */
                           FALSE,   /* create fees */
                           FALSE,   /* solog */
                           OUTPUT i).


IF NOT batch THEN MESSAGE 
      "Services have been copied onto subscription."    SKIP
      i "components were copied."  skip(1)
VIEW-AS ALERT-BOX TITLE " Service COMPONENTS COPIED ".


