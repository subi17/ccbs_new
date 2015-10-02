/* ----------------------------------------------------------------------
  MODULE .......: publish_ifs.p
  TASK .........: Publish invoices to IFS  
  APPLICATION ..: tms
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 19.08.2015
  CHANGED ......:
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{fmakemsreq.i}
{email.i}

DEFINE INPUT PARAMETER iiMsRequest AS INT NO-UNDO.

DEF VAR ldaInvDate     AS DATE NO-UNDO.
DEF VAR liCount        AS INT  NO-UNDO.
DEF VAR lcToday        AS CHAR NO-UNDO.
DEF VAR llgError       AS LOG  NO-UNDO.
DEF VAR lcContent      AS CHAR NO-UNDO. 
DEF VAR lcContent1     AS CHAR NO-UNDO. 
DEF VAR lcContent2     AS CHAR NO-UNDO. 
DEF VAR lcAddrConfDir  AS CHAR NO-UNDO.
DEF VAR lcFileName     AS CHAR NO-UNDO. 
DEF VAR llInterrupt    AS LOG  NO-UNDO. 

   FIND MsRequest WHERE
        MsRequest.Brand     = gcBrand     AND
        MsRequest.MsRequest = iiMsRequest NO-LOCK NO-ERROR.

   IF NOT AVAIL MsRequest OR
                MsRequest.ReqType NE ({&REQTYPE_PUBLISH_IFS}) THEN
      RETURN "ERROR".

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   ASSIGN lcContent      = ""
          llgError       = NO
          liCount        = 0 
          llInterrupt    = NO
          ldaInvDate     = DATE(MONTH(TODAY),1,YEAR(TODAY)) 
          lcFileName     = fCparam("IFS","IFSMonthlyFile") 
          lcAddrConfDir  = fCParamC("RepConfDir")
          lcToday        = STRING(YEAR(TODAY),"9999") +
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99")
          lcFileName     = REPLACE(lcFileName,"#DATE",lcToday).

   IF ldaInvDate EQ ? THEN DO:
      fReqStatus(3,"Error: Invalid Invoice date").
      LEAVE.
   END.

   RUN invoice_deliverystate(ldaInvDate,
                             1,  /* inv.type */
                             1,  /* state */
                             0,  /* FRProcessID */
                             0,  /* UpdateInterval */
                             ?, /* due date if entered */
                             OUTPUT liCount) NO-ERROR. 
   IF ERROR-STATUS:ERROR THEN DO:
      fReqStatus(3,"ERROR: Delivery was not been set.").      
      ASSIGN lcContent1 = "ERROR: Delivery was not been set."
             llgError   = YES.
   END.
   ELSE 
      lcContent1 = "Delivery state was set to " + STRING(liCount) + " Invoices".   

   /* Mail recipients */
      GetRecipients(lcAddrConfDir).
   /* Send via mail */
      SendMail(lcContent,"").
   
   IF llgError THEN LEAVE.
  
   liCount = 0.

   RUN ifs_invoice(32, 
                   lcFileName,
                   "Modified",
                   0,
                   "",
                   "",
                   "",
                   OUTPUT liCount,
                   OUTPUT llInterrupt) NO-ERROR. 
   IF llInterrupt THEN DO:
      fReqStatus(3,"ERROR: Interruption in generating IFS file."). 
      ASSIGN lcContent2 = "ERROR: Interruption in generating IFS file."
             llgError  = YES.
   END.
   ELSE 
      lcContent2 = "Generated IFS for " + STRING(liCount) + " Service Invoices".   

   /* Mail recipients */
      GetRecipients(lcAddrConfDir).
   /* Send via mail */
      SendMail(lcContent,"").
   
   IF llgError THEN LEAVE.

   lcContent = lcContent1 + " " + lcContent2.

   fReqStatus(2,lcContent). /* request handled succesfully */
