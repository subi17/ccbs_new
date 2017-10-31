/* ----------------------------------------------------------------------
  MODULE .......: printdoc1_multi.p
  TASK .........: Print invoices to a doc1 file in multiple runs (screens)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 02.09.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR llOnlyNew     AS LOG  NO-UNDO.
DEF VAR llInvType     AS LOG  NO-UNDO.
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR lcPrintHouse  AS CHAR NO-UNDO.
DEF VAR liScreenQty   AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR liPreQty      AS INT  NO-UNDO. 
DEF VAR lcFileList    AS CHAR NO-UNDO.
DEF VAR liDelType     AS INT  NO-UNDO.

FORM 
   SKIP(2)
   "Print invoices to a file using DOC1 format. Invoices are divided" AT 8 
   "into multiple runs (screens), according to traffic amounts." AT 8 SKIP
   "Make sure that You have started this in a screen session." AT 8 
   SKIP(1)
                   
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      VALIDATE(INPUT ldtInvDate NE ?,
               "Invoice date is mandatory")
      HELP "Invoice date"
      SKIP

   llOnlyNew COLON 20
      LABEL "Printing Status" 
      FORMAT "New/All"
      HELP "Print (A)ll or only (N)ew unprinted invoices" 
      SKIP
        
   llInvType COLON 20
      LABEL "Type Of Invoices"
      FORMAT "Normal/Test"
      HELP "(N)ormal or (T)est invoices"
      SKIP
      
   lcPrintHouse COLON 20
      LABEL "Printhouse"
      FORMAT "X(20)"
      HELP "Printhouse, empty = ALL"
      SKIP
      
   liScreenQty COLON 20
      LABEL "Screens"
      FORMAT ">9"
      HELP "How many screens (runs) are started"
      SKIP(1)
   
   "File Name:" TO 20 SKIP
   lcFile AT 4 
      NO-LABEL 
      HELP "Name of the output file"
      FORMAT "X(70)"
   SKIP(3)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.CUICommon:ynimi + "  INVOICES TO DOC1  " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


ASSIGN ufkey         = FALSE
       ldtInvDate    = TODAY
       llOnlyNew     = TRUE
       llInvType     = TRUE
       lcPrintHouse  = ""
       liScreenQty   = 10
       lcFile        = fCParamC("SplitDoc1Print").

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY ldtInvDate
           llOnlyNew
           llInvType
           lcPrintHouse
           liScreenQty
           lcFile
   WITH FRAME fCrit.

   IF ufkey THEN DO:
      ASSIGN
         Syst.CUICommon:ufk    = 0
         Syst.CUICommon:ufk[1] = 132 
         Syst.CUICommon:ufk[3] = 1128
         Syst.CUICommon:ufk[5] = 63  
         Syst.CUICommon:ufk[8] = 8 
         Syst.CUICommon:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN Syst.CUICommon:toimi = 1
               ufkey = TRUE.

   IF Syst.CUICommon:toimi = 1 THEN DO:

      Syst.CUICommon:ehto = 9. 
      RUN Syst/ufkey.p.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE ldtInvDate
                llOnlyNew
                llInvType
                lcPrintHouse
                liScreenQty
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.CUICommon:nap = KEYLABEL(LASTKEY).

            IF LOOKUP(Syst.CUICommon:nap,Syst.CUICommon:poisnap) > 0 THEN DO:
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   /* quantity of invoices */ 
   ELSE IF Syst.CUICommon:toimi = 3 THEN DO:
      
      liPreQty = 0.
      
      DO liDelType = 1 TO 11 BY 1:
         RUN Inv/printdoc1co.p ("",
                          0,
                          999999999,
                          "",
                          "ZZZZ",
                          ldtInvDate,
                          llOnlyNew,
                          FALSE,
                          IF llInvType 
                          THEN 1
                          ELSE 99,
                          liDelType,
                          "",
                          "Qty",
                          FALSE,
                          OUTPUT liCount,
                          OUTPUT lcError).

         liPreQty = liPreQty + liCount.                 
      END.
      
      MESSAGE liPreQty "invoices will be printed with given criteria."
      VIEW-AS ALERT-BOX TITLE " QUANTITY ".
   END.


   /* printing */
   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF ldtInvDate = ? THEN DO:
         MESSAGE "Invoice date has not been chosen"
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Inv/printdoc1_split.p (ldtInvDate,
                           llOnlyNew,
                           llInvType,
                           lcPrintHouse,
                           liScreenQty,
                           "Doc1",
                           TRUE,
                           0,
                           0,
                           0,
                           TRUE,
                           OUTPUT lcFileList,
                           OUTPUT liCount).
                     
      /* create new screens for actual printing */
      IF lcFileList > "" THEN DO:

         RUN Inv/printdoc1_start_screens.p (ldtInvDate,
                                      lcFileList,
                                      "Doc1",
                                      TRUE).

         IF RETURN-VALUE BEGINS "ERROR:" THEN DO TRANS:
            CREATE ErrorLog.
            ASSIGN ErrorLog.Brand     = Syst.CUICommon:gcBrand
                   ErrorLog.ActionID  = "PRINTDOC1" 
                   ErrorLog.TableName = "Invoice"
                   ErrorLog.KeyValue  = STRING(ldtInvDate,"99-99-99")
                   ErrorLog.ErrorMsg  = RETURN-VALUE
                   ErrorLog.UserCode  = katun.
                   ErrorLog.ActionTS  = Func.Common:mMakeTS().
            RETURN RETURN-VALUE.       
        END.

         IF RETURN-VALUE = "" THEN 
            MESSAGE "Screens have been started"
            VIEW-AS ALERT-BOX.
         ELSE 
            MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX ERROR.
      END.
      
      ELSE MESSAGE "No invoices were found to be printed" SKIP
           RETURN-VALUE
           VIEW-AS ALERT-BOX INFORMATION.
       
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

