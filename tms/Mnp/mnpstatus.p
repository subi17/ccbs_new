/* ----------------------------------------------------------------------
  MODULE .......: mnpstatus.p 
  TASK .........: Choose mnp process status 
                  and list corresponding processes
  APPLICATIO ...: TMS
  AUTHOR .......: anttis 
  CREATED ......: 06.03.08
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

DEFINE INPUT PARAM piMNPType AS INTEGER NO-UNDO.

&SCOPED-DEFINE MNP_ST_TXT_NEW "NEW"
&SCOPED-DEFINE MNP_ST_TXT_AENV "AENV"
&SCOPED-DEFINE MNP_ST_TXT_ASOL "ASOL"
&SCOPED-DEFINE MNP_ST_TXT_ACON "ACON"
&SCOPED-DEFINE MNP_ST_TXT_APOR "APOR"
&SCOPED-DEFINE MNP_ST_TXT_AREC "AREC"
&SCOPED-DEFINE MNP_ST_TXT_AREC_CLOSED "AREC_CLOSED"
&SCOPED-DEFINE MNP_ST_TXT_ACAN "ACAN"
&SCOPED-DEFINE MNP_ST_TXT_ERROR "ERROR"
&SCOPED-DEFINE MNP_ST_TXT_QUIT "QUIT"
&SCOPED-DEFINE MNP_ST_TXT_DONE "DONE"
      
&SCOPED-DEFINE MNP_ST_TXT_MENV "MENV"
&SCOPED-DEFINE MNP_ST_TXT_MPRC "MPRC"
&SCOPED-DEFINE MNP_ST_TXT_MFIN "MFIN"

&SCOPED-DEFINE MNP_ST_TXT_NENV "NENV"
&SCOPED-DEFINE MNP_ST_TXT_NCON "NCON"
&SCOPED-DEFINE MNP_ST_TXT_NMIG "NMIG"

&SCOPED-DEFINE MNP_ST_TXT_BNOT "BNOT"
&SCOPED-DEFINE MNP_ST_TXT_BCAN "BCAN"
&SCOPED-DEFINE MNP_ST_TXT_BDEF "BDEF"
&SCOPED-DEFINE MNP_ST_TXT_BDET "BDET"

DEFINE VARIABLE lcOptions AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSelected AS CHAR NO-UNDO. 

CHOISES:
DO WHILE TRUE:
   CASE piMNPType:
      WHEN {&MNP_TYPE_TERMINATION} THEN lcOptions = 
         {&MNP_ST_TXT_NEW} + "|" +
         {&MNP_ST_TXT_BNOT} + "|" +
         {&MNP_ST_TXT_BCAN} + "|" +
         {&MNP_ST_TXT_BDEF} + "|" +
         {&MNP_ST_TXT_BDET}.
      WHEN 4 THEN lcOptions = 
         {&MNP_ST_TXT_NEW} + "|" +
         {&MNP_ST_TXT_QUIT}.
      WHEN 5 THEN lcOptions = 
         {&MNP_ST_TXT_NEW} + "|" +
         {&MNP_ST_TXT_MENV} + "|" +
         {&MNP_ST_TXT_MPRC} + "|" +
         {&MNP_ST_TXT_MFIN}.
      WHEN 6 THEN lcOptions = 
         {&MNP_ST_TXT_NEW} + "|" +
         {&MNP_ST_TXT_NENV} + "|" +
         {&MNP_ST_TXT_NCON} + "|" +
         {&MNP_ST_TXT_NMIG}.
      OTHERWISE lcOptions = 
         {&MNP_ST_TXT_NEW} + "|" +
         (IF piMNPType EQ 0 THEN ({&MNP_ST_TXT_AENV} + "|") ELSE "") +
         {&MNP_ST_TXT_ASOL} + "|" +
         {&MNP_ST_TXT_ACON} + "|" +
         {&MNP_ST_TXT_APOR} + "|" +
         {&MNP_ST_TXT_AREC} + "|" +
         {&MNP_ST_TXT_ACAN}.
   END.

   IF lcOptions = "" THEN DO:
      MESSAGE "MNP process statuses are not defined" VIEW-AS ALERT-BOX ERROR.
      RETURN "".
   END.
   
   IF piMNPType = {&MNP_TYPE_OLD} THEN
      lcOptions = lcOptions + "|" + {&MNP_ST_TXT_ERROR}.
   IF piMNPType = {&MNP_TYPE_IN} THEN
      lcOptions = lcOptions + "|" + {&MNP_ST_TXT_AREC_CLOSED}.
   lcOptions = lcOptions + "|" + {&MNP_ST_TXT_QUIT}.
         
   RUN Syst/selectbox.p(
     "MNP Status",
     lcOptions,
     OUTPUT lcSelected).

   CASE lcSelected:
      WHEN {&MNP_ST_TXT_NEW} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_NEW},piMNPType).
      WHEN {&MNP_ST_TXT_AENV} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_AENV},piMNPType).
      WHEN {&MNP_ST_TXT_ASOL} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_ASOL},piMNPType).
      WHEN {&MNP_ST_TXT_ACON} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_ACON},piMNPType).
      WHEN {&MNP_ST_TXT_APOR} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_APOR},piMNPType).
      WHEN {&MNP_ST_TXT_AREC} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_AREC},piMNPType).
      WHEN {&MNP_ST_TXT_ACAN} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_ACAN},piMNPType).
      WHEN {&MNP_ST_TXT_MENV} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_MENV},piMNPType).
      WHEN {&MNP_ST_TXT_MPRC} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_MPRC},piMNPType).
      wheN {&MNP_ST_TXT_MFIN} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_MFIN},piMNPType).
      WHEN {&MNP_ST_TXT_NENV} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_NENV},piMNPType).
      WHEN {&MNP_ST_TXT_NCON} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_NCON},piMNPType).
      WHEN {&MNP_ST_TXT_NMIG} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_NMIG},piMNPType).
      WHEN {&MNP_ST_TXT_BNOT} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_BNOT},piMNPType).
      WHEN {&MNP_ST_TXT_BCAN} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_BCAN},piMNPType).
      WHEN {&MNP_ST_TXT_BDEF} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_BDEF},piMNPType).
      WHEN {&MNP_ST_TXT_BDET} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_BDET},piMNPType).
      WHEN {&MNP_ST_TXT_ERROR} THEN RUN Mnp/mnpbr.p(0,{&MNP_ST_ERROR},piMNPType).
      WHEN {&MNP_ST_TXT_AREC_CLOSED} THEN 
         RUN Mnp/mnpbr.p(0,{&MNP_ST_AREC_CLOSED},piMNPType).
      WHEN {&MNP_ST_TXT_QUIT} THEN leave CHOISES. 
   END.
   IF lcSelected = "" THEN LEAVE.
END.
