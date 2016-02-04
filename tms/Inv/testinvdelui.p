/* ---------------------------------------------------------------------------
  MODULE .......: TESTINVDELUI.P
  FUNCTION .....: Delete test invoices
  APPLICATION ..: TMS
  AUTHOR .......: mvi 
  CREATED ......: 07.03.07
  MODIFIED .....: 03.10.07/ vk Added the possibility to give low and high
                               limits for test invoices to be deleted
                 
  Version ......: XFERA
  TODO..........: add process logging
  -------------------------------------------------------------------------- */

{Syst/commali.i} 
{Func/tmsparam2.i}
{Inv/billrund.i NEW}
{Func/faccper.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}
{Func/finvnum.i}
{Func/timestamp.i}

IF lcRight NE "RW" THEN DO:
   MESSAGE " You cannot delete invoices ! " VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE VARIABLE liInvCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE llProceed  AS LOGICAL NO-UNDO INIT FALSE. 
DEFINE VARIABLE liTestInvCount AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcFloor    AS CHARACTER   NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE lcRoof     AS CHARACTER   NO-UNDO FORMAT "x(12)".
DEFINE VARIABLE liNewCount AS INTEGER     NO-UNDO.
DEFINE VARIABLE liRemoved  AS INTEGER     NO-UNDO.

FORM
   SKIP(2)
"        This program deletes all test invoices and releases all calls." 
SKIP(1)
"           Inv. nbr's low limit :" lcFloor SKIP
"           Inv nbr's high limit :" lcRoof SKIP(1)    
"           Proceed with deletion:" llProceed FORMAT "Yes/No"
HELP "Confirm test data deletion"
SKIP (2)
"           Test invoices in DB..:" liTestInvCount SKIP
"           Invoices deleted ....:" liInvCount    SKIP 
SKIP(5)
WITH
   OVERLAY TITLE COLOR value(ctc)
   " " + ynimi + " DELETE TEST INVOICES " + string(pvm,"99-99-99") + " "
   COLOR value(cfc) width 80 ROW 1 NO-LABELS
   FRAME mainFrame.

ASSIGN lcFloor        = ""
       lcRoof         = ""
       liTestInvCount = 0
       liNewCount     = 0.

      
FOR EACH Invoice USE-INDEX InvType WHERE Invoice.Brand   = "1"  AND
                                         Invoice.InvType = 99 NO-LOCK:
    liTestInvCount = liTestInvCount + 1.
END.                                         
  
/* nothing to do */
IF liTestInvCount <= 0 THEN DO:
   MESSAGE "No test invoices to delete."
      VIEW-AS ALERT-BOX.
   RETURN.
END.

DISP liTestInvCount WITH FRAME mainFrame.

mainLoop:
REPEAT WITH FRAME mainFrame:
   ehto = 9.
   RUN Syst/ufkey.

   UPDATE 
     lcFloor
     lcRoof
     llProceed 
     WITH FRAME mainFrame 
   editLoop:
   EDITING:
      READKEY.
      nap = KEYLABEL(LASTKEY).
      
      IF LOOKUP(nap,poisnap) > 0 THEN DO: 

         IF LOOKUP(nap,"f1") > 0 THEN DO:
            IF INPUT llProceed THEN LEAVE editLoop.
         END.
         
      END.
      
      APPLY LASTKEY.
   END.

   IF NOT INPUT llProceed THEN RETURN.

   ASSIGN
      ufk = 0      /* clear all */
      ufk[1] = 7   /* change    */
      ufk[5] = 795 /* start     */
      ufk[8] = 8   /* return    */
      ehto = 1.
   RUN Syst/ufkey.
   
   IF toimi = 1 THEN NEXT mainLoop.
   IF toimi = 5 THEN DO:
      RUN pDeleteTestInvoices.
      RETURN.
   END.
   IF toimi = 8 THEN RETURN.

END.


PROCEDURE pDeleteTestInvoices:

   /* delete test invoices */
   RUN Inv/delete_test_invoice.p (lcFloor,
                              lcRoof,
                              0,
                              0,
                              "",
                              OUTPUT liInvCount).
   PAUSE 0.
   DISP liInvCount WITH FRAME MainFrame. 
                              
   FOR EACH Invoice USE-INDEX InvType WHERE Invoice.Brand   = "1"  AND
                                            Invoice.InvType = 99 NO-LOCK:
       liNewCount = liNewCount + 1.
   END. 
   liRemoved = liTestInvCount - liNewCount.
   MESSAGE liRemoved " test invoices deleted." VIEW-AS ALERT-BOX.  
   RETURN.


END PROCEDURE. 


