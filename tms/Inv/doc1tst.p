/* ----------------------------------------------------------------------
  MODULE .......: printdoc1ui.p
  TASK .........: Print invoices to a doc1 file (ui)
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 19.12.06
  CHANGED ......: 25.04.07/aam own trans dir for test invoices
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}

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
DEF VAR llOnlyFile    AS LOG  NO-UNDO.

FORM 
   SKIP(2)
   "Print invoices to a file using DOC1 format." AT 10 
   SKIP
   "VERSION FOR PICKING TEST INVOICES" AT 10 
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
            
   ldtInvDate COLON 20 
      LABEL "Invoice Date" 
      FORMAT "99-99-9999"
      HELP "Invoice date, empty = ALL"
      SKIP

   llOnlyNew COLON 20
      LABEL "Printing Status" 
      FORMAT "New/All"
      HELP "Print (A)ll or only (N)ew unprinted invoices" SKIP
        
   llCredit COLON 20
      LABEL "Credit Invoices"
      HELP "Print credited and credit invoices" 
      FORMAT "Yes/No"
      SKIP
   llOnlyFile COLON 20 
      LABEL "Only From File"
      HELP "Print only subscriptions listed in the file"
      FORMAT "Yes/No"
      SKIP

   liInvType COLON 20
      LABEL "Invoice Type"
      HELP "One invoice type or 0 = ALL"
      FORMAT ">9"
   lcInvType 
      NO-LABEL
      FORMAT "X(30)"
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
     TITLE " " + Syst.CUICommon:ynimi + "  INVOICES TO DOC1  " + STRING(TODAY,"99-99-99") + " "
     FRAME fCrit.


FUNCTION fIGName RETURNS LOGIC
   (INPUT icInvGroup AS CHAR):
   
   lcIGName = "".
   
   IF icInvGroup = "" THEN lcIgName = "ALL".
               
   ELSE DO:
      FIND InvGroup WHERE 
           InvGroup.Brand    = Syst.CUICommon:gcBrand AND
           InvGroup.InvGroup = icInvGroup
      NO-LOCK NO-ERROR.
      IF AVAILABLE InvGroup THEN lcIgName = InvGroup.IGName.
   END.
   
   DISPLAY lcIgName WITH FRAME fCrit.
   
END FUNCTION.

FUNCTION fTypeName RETURNS LOGIC
   (iiInvType AS INT):
   
   IF iiInvType = 0 
   THEN lcInvType = "ALL".
   ELSE lcInvType = Func.Common:mTMSCodeName("Invoice",
                                     "InvType",
                                     STRING(iiInvType)).
      
   DISPLAY lcInvType WITH FRAME fCrit.
   
END FUNCTION.


ASSIGN ufkey         = FALSE
       liCustNum[2]  = 999999999
       lcInvID[2]    = FILL("Z",12)
       ldtInvDate    = TODAY
       llOnlyNew     = TRUE
       llOnlyFile    = FALSE
       llCredit      = FALSE
       liInvType     = 1 
       lcFile        = fCParamC("Doc1File")
       lcTransDir    = fCParamC("Doc1TestTransDir").
       

CritLoop:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO CritLoop, NEXT CritLoop:

   PAUSE 0.
   DISPLAY lcInvGroup 
           lcInvID
           liCustNum
           ldtInvDate
           llOnlyNew
           llCredit
           llOnlyFile
           liInvType
           lcFile
           lcTransDir
   WITH FRAME fCrit.

   fIGName(lcInvGroup).
   fTypeName(liInvType).

   IF ufkey THEN DO:
      ASSIGN
         Syst.CUICommon:ufk    = 0
         Syst.CUICommon:ufk[1] = 132 
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

         UPDATE lcInvGroup
                liCustNum
                lcInvID
                ldtInvDate
                llOnlyNew
                llCredit
                llOnlyFile
                liInvType
                lcFile
         WITH FRAME fCrit EDITING:

            READKEY.
            Syst.CUICommon:nap = KEYLABEL(LASTKEY).

            IF Syst.CUICommon:nap = "F9" AND FRAME-FIELD = "liInvType" THEN DO:

               RUN Help/h-tmscodes.p(INPUT "Invoice",  /* TableName*/
                                    "InvType", /* FieldName */
                                    "Report", /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ?
               THEN DO WITH FRAME fCrit:
                  DISPLAY INTEGER(lcCode) ;& liInvType.
               END.

               Syst.CUICommon:ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.

            IF LOOKUP(Syst.CUICommon:nap,Syst.CUICommon:poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "lcInvGroup" THEN DO:

                  IF INPUT lcInvGroup > "" AND
                     NOT CAN-FIND(InvGroup WHERE 
                                  InvGroup.Brand    = Syst.CUICommon:gcBrand AND
                                  InvGroup.InvGroup = INPUT lcInvGroup)
                  THEN DO:
                     MESSAGE "Unknown invoicing group."
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.

                  fIGName(INPUT INPUT lcInvGroup).
               END.

               ELSE IF FRAME-FIELD = "liInvType" THEN DO:
                  fTypeName(INPUT INPUT liInvType).
                  IF lcInvType = "" THEN DO:
                     MESSAGE "Unknown invoice type"
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

   ELSE IF Syst.CUICommon:toimi = 5 THEN DO:
      
      IF lcFile = "" THEN DO:
         MESSAGE "File name has not been given."
         VIEW-AS ALERT-BOX ERROR.
         NEXT.
      END.
      
      RUN Inv/printdoc1co_tst.p (lcInvGroup,
                         liCustNum[1],
                         liCustNum[2],
                         lcInvID[1],
                         lcInvID[2],
                         ldtInvDate,
                         llOnlyNew,
                         llCredit,
                         llOnlyFile,
                         liInvType,
                         lcTransDir + "*" + lcFile,
                         OUTPUT liCount,
                         OUTPUT lcError).
      
      MESSAGE liCount "invoices were printed" SKIP ""
              (IF lcError > "" 
               THEN "Error occurred: " + lcError
               ELSE "") 
      VIEW-AS ALERT-BOX TITLE " DONE ".
      
      LEAVE CritLoop.
   END.

   ELSE IF Syst.CUICommon:toimi = 8 THEN DO:
      LEAVE CritLoop.
   END.

END. /* CritLoop */

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

