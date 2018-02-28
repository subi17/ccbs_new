/* ----------------------------------------------------------------------
  MODULE .......: printdoc1ui.p
  TASK .........: Print invoices to a doc1 file (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 19.12.06
  CHANGED ......: 19.01.07/mvi "Credit invoice" term -> "Credit note"
                  16.04.07/aam billrun to printdoc1co
                  21.05.07/aam order of criteria changed,
                               calculate qty
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/tmsconst.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Invoice'}

DEF VAR ufkey         AS LOG  NO-UNDO.
DEF VAR liCount       AS INT  NO-UNDO. 
DEF VAR lcInvGroup    AS CHAR NO-UNDO.
DEF VAR lcIgName      AS CHAR NO-UNDO. 
DEF VAR lcFile        AS CHAR NO-UNDO.
DEF VAR llOnlyNew     AS LOG  NO-UNDO.
DEF VAR llCredit      AS LOG  NO-UNDO.
DEF VAR liInvType     AS INT  NO-UNDO.
DEF VAR lcInvType     AS CHAR NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO EXTENT 2.
DEF VAR lcInvID       AS CHAR NO-UNDO EXTENT 2.
DEF VAR ldtInvDate    AS DATE NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcError       AS CHAR NO-UNDO. 
DEF VAR lcTransDir    AS CHAR NO-UNDO. 
DEF VAR liPreQty      AS INT  NO-UNDO.
DEF VAR liDelType     AS INT  NO-UNDO.
DEF VAR lcDelType     AS CHAR NO-UNDO.

FORM 
   SKIP(2)
   "Print invoices to a file using DOC1 format." AT 10 
   SKIP(1)
                   
   lcInvGroup COLON 20
      LABEL "Invoice Group"
      HELP  "Invoice group, EMPTY = all"
   lcIgName
      NO-LABEL
      FORMAT "X(30)" 
   SKIP

   liCustNum[1] COLON 20
      LABEL  "Customers"
      HELP   "Customers"
      FORMAT ">>>>>>>>9"
   "-"
   liCustNum[2]
      NO-LABEL
      HELP "Customers"
      VALIDATE(INPUT liCustNum[2] >= INPUT liCustNum[1],
               "Invalid definition")
      FORMAT ">>>>>>>>9"
      SKIP(1)                 

   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "One invoice type or 0 = ALL"
      FORMAT ">9"
   lcInvType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP

   liDelType COLON 20
      LABEL "Delivery Type"
      HELP "Invoice delivery type"
      FORMAT ">9"
   lcDelType 
      NO-LABEL
      FORMAT "X(30)"
      SKIP
      
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date, empty = ALL"
      SKIP

   lcInvID[1] COLON 20
      LABEL "Invoices"
      HELP  "Invoices"
      FORMAT "X(12)"
   "-"
   lcInvID[2]
      NO-LABEL
      HELP "Invoices"
      VALIDATE(INPUT lcInvID[2] >= INPUT lcInvID[1],
               "Invalid definition")
      FORMAT "X(12)"
      SKIP           
            
   llOnlyNew COLON 20
      LABEL "Printing Status" 
      FORMAT "New/All"
      HELP "Print (A)ll or only (N)ew unprinted invoices" SKIP
        
   llCredit COLON 20
      LABEL "Credit Notes"
      HELP "Print credited and credit notes" 
      FORMAT "Yes/No"
      SKIP(1)
   
   lcFile COLON 20
      LABEL "File Name"
      HELP "Name of the output file"
      FORMAT "X(50)"
      SKIP
   lcTransDir COLON 20
      LABEL "Transfer Directory"
      FORMAT "X(50)" 
   SKIP(1)
   
WITH ROW 1 SIDE-LABELS WIDTH 80
     TITLE " " + Syst.Var:ynimi + "  INVOICES TO DOC1  " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


FUNCTION fIGName RETURNS LOGIC
   (INPUT icInvGroup AS CHAR):
   
   lcIGName = "".
   
   IF icInvGroup = "" THEN lcIgName = "ALL".
               
   ELSE DO:
      FIND InvGroup WHERE 
           InvGroup.Brand    = Syst.Var:gcBrand AND
           InvGroup.InvGroup = icInvGroup
      NO-LOCK NO-ERROR.
      IF AVAILABLE InvGroup THEN lcIgName = InvGroup.IGName.
   END.
   
   DISPLAY lcIgName WITH FRAME fCrit.
   
END FUNCTION.

FUNCTION fInvTypeName RETURNS LOGIC
   (iiInvType AS INT):
   
   IF iiInvType = 0 
   THEN lcInvType = "ALL".
   ELSE lcInvType = Func.Common:mTMSCodeName("Invoice",
                                     "InvType",
                                     STRING(iiInvType)).
      
   DISPLAY lcInvType WITH FRAME fCrit.
   
END FUNCTION.

FUNCTION fDelTypeName RETURNS LOGIC
   (iiDelType AS INT):
   
   IF iiDelType = 0 
   THEN lcDelType = "ALL".
   ELSE lcDelType = Func.Common:mTMSCodeName("Invoice",
                                     "DelType",
                                     STRING(iiDelType)).
      
   DISPLAY lcDelType WITH FRAME fCrit.
   
END FUNCTION.


ASSIGN ufkey         = FALSE
       liCustNum[2]  = 999999999
       lcInvID[2]    = FILL("Z",12)
       ldtInvDate    = TODAY
       llOnlyNew     = TRUE
       llCredit      = FALSE
       liInvType     = 1 
       liDelType     = 1
       lcFile        = fCParamC("Doc1File").
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   IF liInvType = 99 
   THEN lcTransDir = fCParamC("Doc1TestTransDir").
   ELSE lcTransDir = fCParamC("Doc1TransDir").

   IF liDelType = {&INV_DEL_TYPE_EMAIL} OR
      liDelType = {&INV_DEL_TYPE_SMS} OR
      liDelType = {&INV_DEL_TYPE_ESI} OR
      liDelType = {&INV_DEL_TYPE_NO_DELIVERY} OR
      liDelType = {&INV_DEL_TYPE_EMAIL_PENDING} OR
      liDelType = {&INV_DEL_TYPE_NO_TRAFFIC} THEN
      lcTransDir = fCParamC("Doc1Del10TransDir").
   
   PAUSE 0.
   DISPLAY lcInvGroup 
           lcInvID
           liCustNum
           ldtInvDate
           llOnlyNew
           llCredit
           liInvType
           liDelType
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fIGName(lcInvGroup).
   fInvTypeName(liInvType).
   fDelTypeName(liDelType).

   IF ufkey THEN DO:
      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 132 
         Syst.Var:ufk[3] = 1128
         Syst.Var:ufk[5] = 63  
         Syst.Var:ufk[8] = 8 
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.
   END.
   ELSE ASSIGN Syst.Var:toimi = 1
               ufkey = TRUE.

   IF Syst.Var:toimi = 1 THEN DO:

      Syst.Var:ehto = 9. 
      RUN Syst/ufkey.p.
      
      liPreQty = 0.
      
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:

         UPDATE lcInvGroup
                liCustNum
                liInvType
                liDelType
                ldtInvDate
                lcInvID
                llOnlyNew
                llCredit
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.Var:nap = KEYLABEL(LASTKEY).

            IF Syst.Var:nap = "F9" AND LOOKUP(FRAME-FIELD,"liInvType,liDelType") > 0
            THEN DO:

               IF FRAME-FIELD = "liInvType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "InvType", /* FieldName */
                                       "Report", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) ;& liInvType.
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "liDelType" THEN DO:
                  RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                       "DelType", /* FieldName */
                                       "Billing", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO WITH FRAME fCrit:
                     DISPLAY INTEGER(lcCode) ;& liDelType.
                  END.
               END.
                
               Syst.Var:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(Syst.Var:nap,Syst.Var:poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcInvGroup" THEN DO:

                  IF INPUT lcInvGroup > "" AND
                     NOT CAN-FIND(InvGroup WHERE 
                                  InvGroup.Brand    = Syst.Var:gcBrand AND
                                  InvGroup.InvGroup = INPUT lcInvGroup)
                  THEN DO:
                     MESSAGE "Unknown invoicing group."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  fIGName(INPUT INPUT lcInvGroup).
               END.

               ELSE IF FRAME-FIELD = "liInvType" THEN DO:
                  fInvTypeName(INPUT INPUT liInvType).
                  IF lcInvType = "" THEN DO:
                     MESSAGE "Unknown invoice type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
 
               ELSE IF FRAME-FIELD = "liDelType" THEN DO:
                  fDelTypeName(INPUT INPUT liDelType).
                  IF lcDelType = "" THEN DO:
                     MESSAGE "Unknown invoice delivery type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
                
            END.

            APPLY LASTKEY.
         END. 

         LEAVE. 
      END.

   END.

   /* quantity of invoices */ 
   ELSE IF Syst.Var:toimi = 3 THEN DO:
      
      RUN Inv/printdoc1co.p (lcInvGroup,
                       liCustNum[1],
                       liCustNum[2],
                       lcInvID[1],
                       lcInvID[2],
                       ldtInvDate,
                       llOnlyNew,
                       llCredit,
                       liInvType,
                       liDelType,
                       "",
                       "Qty",
                       FALSE,
                       OUTPUT liPreQty,
                       OUTPUT lcError).
      
      MESSAGE liPreQty "invoices will be printed with given criteria."
      VIEW-AS ALERT-BOX TITLE " QUANTITY ".
   END.


   /* printing */
   ELSE IF Syst.Var:toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      lcFile = lcTransDir + "*" + lcFile.
         
      RUN Inv/printdoc1co.p (lcInvGroup,
                       liCustNum[1],
                       liCustNum[2],
                       lcInvID[1],
                       lcInvID[2],
                       ldtInvDate,
                       llOnlyNew,
                       llCredit,
                       liInvType,
                       liDelType,
                       "",
                       lcFile,
                       FALSE,
                       OUTPUT liCount,
                       OUTPUT lcError).
      
      MESSAGE liCount "invoices were printed" SKIP ""
              (IF liPreQty > 0 
               THEN "Precounted quantity was " + STRING(liPreQty) + CHR(10)
               ELSE "") 
              (IF lcError > "" 
               THEN "Error occurred: " + lcError
               ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.Var:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

