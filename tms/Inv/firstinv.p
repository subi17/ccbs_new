/* ----------------------------------------------------------------------
  MODULE .......: firstinv.p
  TASK .........: Create first invoice to new customers
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.06.04
  CHANGED ......: 23.08.04/aam orderid to nnlamu5
  Version ......: Shark
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Func/cparam2.i}

DEF OUTPUT PARAMETER oiQty AS INT NO-UNDO.

DEF VAR llFound       AS LOG  NO-UNDO.
DEF VAR liQty         AS INT  NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcEPLFile     AS CHAR NO-UNDO.
DEF VAR liLetterClass AS INT  NO-UNDO. 
DEF VAR lcItem        AS CHAR NO-UNDO. 
DEF VAR lcCLILst      AS CHAR NO-UNDO. 

ASSIGN liLetterClass = fCParamI("EPLInvLClass") 
       lcEPLFile     = fCParamC("EPLFile")
       lcItem        = fCParamC("FirstInvItem").

/* only on weekdays */
IF WEEKDAY(TODAY) = 7 OR WEEKDAY(TODAY) = 1 THEN RETURN.

/* item(s) to be billed */
IF lcItem = ? OR lcItem = "" THEN RETURN.

FOR EACH Customer NO-LOCK WHERE
         Customer.Brand = "1":

   /* first invoice of new subscriptions -> mobsub must exist */
   IF NOT CAN-FIND(FIRST MobSub OF Customer) THEN NEXT.

   llFound = FALSE.

   CLILoop:
   FOR EACH MobSub OF Customer NO-LOCK WHERE    
            MobSub.ActivationDate NE ?          AND
            /* 10 days must have been passed from activation */
            TODAY - MobSub.ActivationDate >= 10 AND
            /* 10 days should be enough to handle each new subscr. */
            TODAY - MobSub.ActivationDate <= 20:
           
      /* unbilled fees */
      FOR EACH FixedFee NO-LOCK WHERE
               FixedFee.Brand     = Customer.Brand    AND
               FixedFee.CustNum   = Customer.CustNum  AND
               FixedFee.HostTable = "MobSub"          AND
               FixedFee.KeyValue  = STRING(MobSub.MSSeq):
              
         /* if even one fee has been billed from this cli -> pass */  
         IF CAN-FIND(FIRST FFItem OF FixedFee WHERE FFItem.Billed = TRUE)
         THEN NEXT CLILoop.
               
         IF LOOKUP(FixedFee.BillCode,lcItem) > 0 THEN DO:
            ASSIGN llFound  = TRUE
                   lcCLILst = lcCLILst + 
                              (IF lcCLILst > "" THEN "," ELSE "") + 
                              FixedFee.KeyValue.
         END.                     
      END.

      IF llFound THEN LEAVE.
      
   END.
   
   IF llFound THEN DO:

      /* create invoice */
      RUN Inv/nnlamu5 (Customer.InvCust,
                   0,
                   lcCLILst,
                   -1,
                   TRUE,
                   OUTPUT liQty).
  
      IF liQty > 0 THEN DO:
                  
         oiQty = oiQTy + liQty.

         /* print new invoice to EPL */
         FIND FIRST Invoice NO-LOCK WHERE
                    Invoice.CustNum    = Customer.InvCust AND
                    Invoice.InvDate    = TODAY            AND
                    Invoice.PrintState = 0                AND
                    Invoice.CrInvNum   = 0 NO-ERROR.
                   
         IF AVAILABLE Invoice THEN 
         RUN Inv/eletterinv(Invoice.InvNum,
                        Invoice.InvNum,
                        Invoice.InvDate,
                        "",
                        Invoice.CustNum,
                        FALSE,
                        TRUE,
                        TRUE,
                        Invoice.InvType,
                        liLetterClass,
                        lcEPLFile,
                        OUTPUT liQty,
                        OUTPUT lcError).
      END.
      
   END.

END.

