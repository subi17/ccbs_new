/* ----------------------------------------------------------------------------
  MODULI .......: PRINMemo.P
  TEHTAVA ......: EPL-file or local print for Memos
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 11.02.04
  MUUTOSPVM ....: 
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Syst/utumaa.i new }
{Func/feplstart.i}
{Func/cparam2.i}
{Inv/edefine.i new}

DEF INPUT PARAMETER icHostTable LIKE Memo.HostTable  NO-UNDO.
DEF INPUT PARAMETER icKeyValue  LIKE Memo.KeyValue   NO-UNDO.
DEF INPUT PARAMETER iiMemoSeq   LIKE Memo.MemoSeq    NO-UNDO.

DEF VAR llEPL         AS LOG  NO-UNDO. 
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liLetterClass AS INT  NO-UNDO.
DEF VAR lcTestFlag    AS CHAR NO-UNDO. 
DEF VAR liAddress     AS INT  NO-UNDO. 
DEF VAR lcName        AS CHAR NO-UNDO.
DEF VAR lcAddr        AS CHAR NO-UNDO.
DEF VAR lcPost        AS CHAR NO-UNDO.
DEF VAR llUfkey       AS LOG  NO-UNDO.
DEF VAR lcTitle       AS CHAR NO-UNDO.
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 
DEF VAR lcTarget      AS CHAR NO-UNDO. 

lcTestFlag = fCParamC("EPLTest").

FORM
   skip(1)
   lcTitle
      NO-LABEL 
      FORMAT "X(50)" 
      SKIP

   "will be printed as either an EPL-file or a local print." 
      AT 2 
      SKIP(1)
   
   liAddress AT 2
      LABEL "Target Address"
      HELP "Letter receiver" 
      FORMAT "9"
      VALIDATE(INPUT liAddress > 0 AND INPUT liAddress < 3,
               "Valid values are 1-2")
   lcTarget 
      NO-LABEL
      FORMAT "X(30)"
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

   lcName AT 5 
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
   with title " PRINT A MEMO " side-labels
   ROW 5 centered OVERLAY FRAME rajat.

FUNCTION fTargetAddr RETURNS LOGICAL
   (iiAddress AS INT).
   
   ASSIGN lcName   = ""
          lcAddr   = ""
          lcPost   = ""
          lcTarget = "".
          
   lcTarget = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "InvText",
                               "AddrTarget",
                               STRING(iiAddress)).
           
   CASE iiAddress:
   WHEN 1 THEN ASSIGN lcName = Customer.CustName
                      lcAddr = Customer.Address
                      lcPost = Customer.ZipCode + " " + Customer.PostOffice.
   WHEN 2 THEN ASSIGN lcName = Customer.IDelName
                      lcAddr = Customer.IDelAddr
                      lcPost = Customer.IDelZipCode + " " +
                               Customer.IDelPost.
   END CASE.                      

   DISPLAY lcTarget lcName lcAddr lcPost WITH FRAME rajat.
END.

FIND FIRST Memo WHERE Memo.Brand     = gcBrand     AND
                      Memo.HostTable = icHostTable AND
                      Memo.KeyValue  = icKeyValue  AND 
                      Memo.MemoSeq   = iiMemoSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE Memo THEN DO:
   MESSAGE "Memo was not found"
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.

IF Memo.CustNum = 0 THEN DO:
   MESSAGE "Memo is not related to any customer"
   VIEW-AS ALERT-BOX 
   ERROR.
   RETURN.
END.

FIND Customer WHERE 
     Customer.Brand   = Memo.Brand AND
     Customer.CustNum = Memo.CustNum NO-LOCK NO-ERROR.
IF NOT AVAILABLE Customer THEN DO:
   MESSAGE "Unknown customer" Memo.CustNum
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.
     
ASSIGN tuni1         = "Memo"
       tuni2         = ""
       liLetterClass = fCParamI("EPLGenLClass")
       liAddress     = 1
       llUfkey       = FALSE
       nap           = "1"
       lcTitle       = '"' + Memo.MemoTitle + '"'
       llEPL         = FALSE.

toimi:
repeat WITH FRAME rajat ON ENDKEY UNDO toimi, NEXT toimi:

  
   PAUSE 0.
   DISPLAY lcTitle
           liAddress
           llEPL
           liLetterClass
           WITH FRAME rajat.
 
   fTargetAddr(liAddress).
            
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
          liAddress
          llEPL            
          liLetterClass
          WITH FRAME rajat EDITING:
          
             READKEY.
             
             IF FRAME-FIELD = "liAddress" AND keylabel(lastkey) = "F9" 
             THEN DO:
              
                RUN Help/h-tmscodes.p(INPUT "InvText",   /* TableName*/
                                     "AddrTarget", /* FieldName */
                                     "Printing",   /* GroupCode */
                               OUTPUT lcCode).

                IF lcCode NE "" AND lcCode NE ? THEN 
                DISPLAY INTEGER(lcCode) @ liAddress WITH FRAME rajat.
                
                ehto = 9.
                RUN Syst/ufkey.
                
                NEXT.
             END.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME rajat:
             
                PAUSE 0.

                IF FRAME-FIELD = "liAddress" THEN DO:
                   fTargetAddr(INPUT INPUT FRAME rajat liAddress).
                END.

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.   
   
   else if lookup(nap,"5,f5") > 0 THEN DO:
      
      IF lcName = "" OR
         lcPost = ""
      THEN DO:
         llOk = FALSE.
         MESSAGE "Target address is missing." SKIP
                 "Continue with printing anyway ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         SET llOk.
         IF NOT llOk THEN NEXT.
      END.
      
      ehto = 5. 
      RUN Syst/ufkey.
 
      IF llEPl THEN DO:
         IF NOT fEPLStart(lcTestFlag) THEN NEXT.
      END.
       
      ELSE DO:
         tila = TRUE.
         {Syst/tmsreport.i RETURN}

      END.

      RUN Mc/printxt (Memo.CustNum,
                   0,                      /* msseq  */
                   "",
                   2,                      /* 2=memo */
                   liAddress,
                   Memo.HostTable,
                   Memo.KeyValue,
                   Memo.MemoSeq,
                   IF llEPL THEN 1 ELSE 2,
                   liLetterClass,
                   OUTPUT lcErrFile).
      
      IF NOT llEPL THEN DO:
         tila = FALSE.
         {Syst/utuloste.i}
      END.

      IF lcErrFile = "" THEN 
      MESSAGE "Memo has been printed."
      VIEW-AS ALERT-BOX
      TITLE " DONE ".

      ELSE 
      MESSAGE "Error occurred during printout." SKIP
              "File" lcErrFile "contains error messages."
      VIEW-AS ALERT-BOX
      TITLE " PRINTING FAILED ".
      
      LEAVE toimi.
   END.

   else if lookup(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.
      
END. /* toimi */

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.

 
