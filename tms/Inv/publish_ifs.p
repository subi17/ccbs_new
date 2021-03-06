/* ----------------------------------------------------------------------
  MODULE .......: publish_ifs.p
  TASK .........: Publish invoices to IFS  
  APPLICATION ..: tms
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 19.08.2015
  CHANGED ......:
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/email.i}
{Func/multitenantfunc.i}
{Func/msreqfunc.i}

DEFINE INPUT PARAMETER iiMsRequest AS INT NO-UNDO.

DEF VAR ldaInvDate     AS DATE NO-UNDO.
DEF VAR liCount        AS INT  NO-UNDO.
DEF VAR liDumped       AS INT  NO-UNDO. 
DEF VAR lcToday        AS CHAR NO-UNDO.
DEF VAR llgError       AS LOG  NO-UNDO.
DEF VAR lcContent      AS CHAR NO-UNDO. 
DEF VAR lcAddrConfDir  AS CHAR NO-UNDO.
DEF VAR llInterrupt    AS LOG  NO-UNDO. 
DEF VAR lcContLogDir   AS CHAR NO-UNDO. 
DEF VAR lcLogFile      AS CHAR NO-UNDO. 
DEF VAR lcTenant       AS CHAR NO-UNDO. 

DEFINE STREAM strout.
   
   FIND MsRequest WHERE
        MsRequest.Brand     = Syst.Var:gcBrand     AND
        MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.

   IF NOT AVAIL MsRequest OR
                MsRequest.ReqType NE ({&REQTYPE_PUBLISH_IFS}) THEN
      RETURN "ERROR".

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   ASSIGN lcContent      = ""
          llgError       = NO
          liCount        = 0 
          liDumped       = 0
          llInterrupt    = NO
          ldaInvDate     = ?
          lcAddrConfDir  = ""
          lcContLogDir   = ""
          lcToday        = ""
          lcLogFile      = ""
          ldaInvDate     = DATE(MONTH(TODAY),1,YEAR(TODAY)) 
          lcTenant       = fConvertTenantToBrand(BUFFER-TENANT-NAME(MsRequest))
          lcAddrConfDir  = fCParamC("RepConfDir")
          lcContLogDir   = fCParam("PublishInvoice","ContentLogDir")
          lcToday        = STRING(YEAR(TODAY),"9999") +
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99")
          lcLogFile      = lcContLogDir + lcTenant + 
                           "_delstate_" + lcToday + STRING(TIME) + ".log".
 
   OUTPUT STREAM strout to VALUE(lcLogFile) APPEND.

   IF lcAddrConfDir > "" THEN
      lcAddrConfDir = lcAddrConfDir + "publishinvoice.email".

   IF ldaInvDate EQ ? THEN DO:
      fReqStatus(3,"Error: Invalid Invoice date").
      LEAVE.
   END.

   RUN Inv/invoice_deliverystate.p(ldaInvDate,
                             1,  /* inv.type */
                             1,  /* state */
                             0,  /* FRProcessID */
                             0,  /* UpdateInterval */
                             ?, /* due date if entered */
                             OUTPUT liCount).

   IF RETURN-VALUE BEGINS "ERROR" THEN DO:
      fReqStatus(3,RETURN-VALUE). 

      PUT STREAM strout UNFORMATTED
         "ERROR: " + RETURN-VALUE SKIP.
      
      llgError   = YES.
   END.
   ELSE 
      PUT STREAM strout UNFORMATTED 
         "Delivery state was set to " + STRING(liCount) + " Invoices" SKIP.   

   OUTPUT STREAM strout CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   xMailSubj = lcTenant + " " + xMailSubj.
   /* Send via mail */
   SendMail(lcLogFile,"").
   
   IF llgError THEN LEAVE.
  
   lcLogFile = lcContLogDir + lcTenant + 
               "_publishifs_" + lcToday + STRING(TIME) + ".log".
 
   OUTPUT STREAM strout TO VALUE(lcLogFile) APPEND.
 
   FIND FIRST DumpFile NO-LOCK WHERE
              DumpFile.Brand    EQ Syst.Var:gcBrand             AND
              DumpFile.DumpName EQ {&DUMP_IFS_INVOICE} NO-ERROR.

   IF AVAIL DumpFile THEN DO:
      RUN Syst/dumpfile_run.p(DumpFile.DumpID,
                       "Modified",
                       "",
                       FALSE,
                       OUTPUT liDumped).
   
      IF liDumped >= 0 THEN 
         PUT STREAM strout UNFORMATTED 
            "Generated IFS for " + STRING(liDumped) + " Service Invoices" SKIP.
      ELSE DO: 
         PUT STREAM strout UNFORMATTED
            "Error in creating IFS dump event !" SKIP.
         llgError = YES.
      END.   
   END.
    
   OUTPUT STREAM strout CLOSE.

   /* Mail recipients */
   GetRecipients(lcAddrConfDir).
   xMailSubj = lcTenant + " " +  xMailSubj.
   /* Send via mail */
   SendMail(lcLogFile,"").

   IF llgError THEN LEAVE.

   lcContent = "Delivery state was set to " + STRING(liCount) + " Invoices" + "  " +
               "Generated IFS for " + STRING(liDumped) + " Service Invoices".

   fReqStatus(2,lcContent). /* request handled succesfully */

