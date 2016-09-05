/* ----------------------------------------------------------------------------
  MODULI .......: PRINOINF.P
  TEHTAVA ......: EPL-file or local print for information texts from order
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 24.05.04
  MUUTOSPVM ....: 11.06.04/aam ilSilent, ocError
                  21.11.06/aam OrderCustomer
  VERSIO .......: yoigo
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Syst/utumaa.i new }
{Func/feplstart.i}
{Func/cparam2.i}
{Inv/edefine.i new}

DEF INPUT  PARAMETER iiITNum   AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiOrderID AS INT  NO-UNDO.
DEF INPUT  PARAMETER ilSilent  AS LOG  NO-UNDO.
DEF OUTPUT PARAMETER ocError   AS CHAR NO-UNDO. 

DEF VAR llEPL         AS LOG  NO-UNDO. 
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liLetterClass AS INT  NO-UNDO.
DEF VAR lcTestFlag    AS CHAR NO-UNDO. 
DEF VAR lcName        AS CHAR NO-UNDO.
DEF VAR lcAddr        AS CHAR NO-UNDO.
DEF VAR lcPost        AS CHAR NO-UNDO.
DEF VAR llUfkey       AS LOG  NO-UNDO.
DEF VAR lcTitle       AS CHAR NO-UNDO.
DEF VAR llOk          AS LOG  NO-UNDO. 
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCLI         AS CHAR NO-UNDO.
DEF VAR liPer         AS INT  NO-UNDO.
DEF VAR liMSSeq       AS INT  NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcTagLabel    AS CHAR NO-UNDO. 
DEF VAR lcTag         AS CHAR NO-UNDO. 
DEF VAR lcDefTxt      AS CHAR NO-UNDO. 


FORM
   skip(1)
   "Text will be printed either as an EPL-file or a local print." 
      AT 2 
      SKIP(1)

   lcTitle AT 2
      LABEL "Text ........."
      FORMAT "X(50)" 
      SKIP
      
   lcCLI AT 2
      label "CLI .........."
      help "A-number that letter is addressed to"
      FORMAT "X(16)"
      SKIP
 
   llEPL AT 2
      LABEL "Print to ....."
      HELP "Print to (E)PL or to (P)rinter"
      FORMAT "EPL/Printer"
      SKIP
      
   liLetterClass AT 2
      LABEL "Letter Class ."
      HELP "Letter class for EPL"
      FORMAT ">"
      VALIDATE(INPUT liLetterClass >= 1 AND INPUT liLetterClass <= 4,
               "Valid choices are 1-4")
      SKIP(1)

   iiOrderID AT 5
      LABEL "Order ."
      FORMAT ">>>>>>>9"
      SKIP   lcName AT 5 
      LABEL "Name .."
      FORMAT "X(30)"
      SKIP
   lcAddr AT 5
      LABEL "Address"
      FORMAT "X(30)"
      SKIP
   lcPost  AT 5
      LABEL "Post .."
      FORMAT "X(30)"
      SKIP(1)
   with title " PRINT AN INFORMATION TEXT " side-labels
   ROW 3 centered OVERLAY FRAME rajat.

FUNCTION fTxtTitle RETURNS LOGICAL.

   lcTitle = "".
   
   IF liITNum > 0 THEN DO:
      FIND InvText WHERE InvText.ITNum = liITNum NO-LOCK NO-ERROR.
      IF AVAILABLE InvText THEN lcTitle = '"' + InvText.TxtTitle + '"'.
   END.   
   
   DISPLAY lcTitle WITH FRAME rajat.

END FUNCTION.

FUNCTION fTargetAddr RETURNS LOGICAL.
   
   ASSIGN lcName = ""
          lcAddr = ""
          lcPost = "".
          
   /* receiver is always the owner */
   IF AVAILABLE OrderCustomer THEN ASSIGN 
      lcName = DYNAMIC-FUNCTION("fDispOrderName" IN ghFunc1,
                                BUFFER OrderCustomer)
      lcAddr = OrderCustomer.Address
      lcPost = OrderCustomer.ZipCode + " " + OrderCustomer.PostOffice.

   IF NOT ilSilent THEN 
   DISPLAY iiOrderID lcName lcAddr lcPost WITH FRAME rajat.
END.

IF iiITNum > 0 THEN DO:

   FIND InvText WHERE InvText.ITNum = iiITNum NO-LOCK NO-ERROR.

   IF NOT AVAILABLE InvText THEN DO:
      ocError = "Text was not found". 
      IF NOT ilSilent THEN
      MESSAGE ocError
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.

   liITNum = iiITNum.

END.
ELSE DO:
   lcDefTxt = fCParamC("EPLOrderTxt").

   FIND FIRST InvText NO-LOCK WHERE
              InvText.Brand    = gcBrand AND
              InvText.Target   = "General" AND
              InvText.KeyValue = lcDefTxt NO-ERROR.
   IF AVAILABLE InvText THEN liITNum = InvText.ITNum.
END.

IF iiOrderID > 0 THEN DO:

   FIND Order NO-LOCK WHERE
        Order.Brand   = gcBrand AND
        Order.OrderID = iiOrderID NO-ERROR.
   IF NOT AVAILABLE Order THEN DO:
      ocError = "Unknown order " + STRING(iiOrderID).
      IF NOT ilSilent THEN
      MESSAGE ocError
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

   ASSIGN lcCLI   = Order.CLI
          liMSSeq = Order.MSSeq.

   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = gcBrand       AND
              OrderCustomer.OrderID = Order.OrderID AND
              OrderCustomer.RowType = 1 NO-ERROR.
   
END. 

     
ASSIGN tuni1         = "InvText"
       tuni2         = ""
       liLetterClass = IF liITNum > 0 AND InvText.LetterClass > 0 
                       THEN InvText.LetterClass
                       ELSE fCParamI("EPLGenLClass")
       llUfkey       = FALSE
       nap           = "1"
       llEPL         = TRUE
       lcTestFlag    = fCParamC("EPLTest")
       liPer         = YEAR(TODAY) * 10000 +
                       MONTH(TODAY) * 100  +
                       DAY(TODAY)          +
                       0.86399.
toimi:
repeat WITH FRAME rajat ON ENDKEY UNDO toimi, NEXT toimi:

   IF NOT ilSilent THEN DO:
      PAUSE 0.
      DISPLAY llEPL
              liLetterClass
              lcCLI
              WITH FRAME rajat.
 
      fTxtTitle().
   END.
   ELSE nap = "5".

   fTargetAddr().
            
   IF llUfkey THEN DO:
      ASSIGN
      ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
      ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
      ehto = 3.
      RUN Syst/ufkey.p.

      READKEY.
      nap = keylabel(LASTKEY).
   END.
   ELSE llUfkey = TRUE.

   if lookup(nap,"1,f1") > 0 THEN DO:
   
      ASSIGN ehto = 9.
      RUN Syst/ufkey.p.

      REPEAT ON ENDKEY UNDO, LEAVE:
      
          UPDATE 
          lcTitle WHEN iiItNum = 0
          llEPL            
          liLetterClass
          WITH FRAME rajat EDITING:
          
             READKEY.
             
             IF FRAME-FIELD = "lcTitle" THEN DO:
             
                IF LOOKUP(KEYLABEL(LASTKEY),"ENTER,RETURN") > 0 THEN DO:
                   APPLY LASTKEY.
                   NEXT.
                END.
                
                ELSE IF KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
                    
                ELSE IF keylabel(lastkey) = "F9" THEN DO:
                   ASSIGN gcHelpParam = "prt"
                          si-recid    = 0.
                   RUN Mc/invotxt.p ("",
                                "").
                   gcHelpParam = "".
                   
                   ehto = 9.
                   RUN Syst/ufkey.p.
       
                   IF si-recid > 0 THEN DO:
                      FIND InvText WHERE RECID(InvText) = si-recid 
                      NO-LOCK NO-ERROR.
                      IF AVAILABLE InvText THEN DO:

                         liITNum = InvText.ITNum.
                         IF InvText.LetterClass > 0 THEN DO:
                            liLetterClass = InvText.LetterClass.
                            DISPLAY liLetterClass WITH FRAME rajat.
                         END.
                         
                         fTxtTitle().
                         APPLY "ENTER".
                         NEXT.
                      END.
                   END. 
                   
                END.
                ELSE NEXT. 
             END.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME rajat:
             
                PAUSE 0.

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.   
   
   else if lookup(nap,"5,f5") > 0 THEN DO:
      
      IF liITNum = 0 THEN DO:
         ocError = "Text has not been chosen".
         IF NOT ilSilent THEN
         MESSAGE ocError
         VIEW-AS ALERT-BOX
         ERROR.
         NEXT.
      END.
      
      IF lcName = "" OR
         lcPost = ""
      THEN DO:
         llOk = FALSE.
         ocError = "Target address is missing.".
         IF NOT ilSilent THEN DO:
            MESSAGE  ocError SKIP
                    "Continue with printing anyway ?"
            VIEW-AS ALERT-BOX
            QUESTION
            BUTTONS YES-NO
            SET llOk.
            IF NOT llOk THEN NEXT.
         END.
         ELSE RETURN.
      END.
      
      IF NOT ilSilent THEN DO:      
         ehto = 5. 
         RUN Syst/ufkey.p.
      END.
      
      IF llEPl THEN DO:
         IF NOT ilSilent THEN DO:
            IF NOT fEPLStart(lcTestFlag) THEN NEXT.
         END.   
      END.
       
      ELSE DO:
         tila = TRUE.
         {Syst/tmsreport.i RETURN}
         
      END.

      RUN Mc/printxt.p (iiOrderID,
                   liMSSeq, 
                   "",
                   1,                      /* 1=invtext */
                   6,                      /* order */
                   "",
                   "",
                   liITNum,
                   IF llEPL THEN 1 ELSE 2,
                   liLetterClass,
                   OUTPUT lcErrFile).
      
      IF NOT llEPL THEN DO:
         tila = FALSE.
         {Syst/utuloste.i}
      END.

      IF lcErrFile = "" THEN DO:
         IF NOT ilSilent THEN
         MESSAGE "Information text has been printed."
         VIEW-AS ALERT-BOX
         TITLE " DONE ".
      END.
      
      ELSE DO:
         ocError = "Error occurred during printout.".
         IF NOT ilSilent THEN
         MESSAGE ocError SKIP
                "File" lcErrFile "contains error messages."
         VIEW-AS ALERT-BOX
         TITLE " PRINTING FAILED ".
      END.
            
      LEAVE toimi.
   END.

   else if lookup(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.
      
END. /* toimi */

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

 
