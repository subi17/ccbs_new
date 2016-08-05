/* ----------------------------------------------------------------------------
  MODULI .......: PRININFO.P
  TEHTAVA ......: EPL-file or local print for information texts
  SOVELLUS .....: TMS
  TEKIJA .......: aam
  LUONTIPVM ....: 11.02.04
  MUUTOSPVM ....: 14.12.05/aam username from customer, not msowner
                  25.01.06/aam target address 5  
                  15.02.06/aam printing denied when InfoType = "SYS"        
                               (skip with password)
  VERSIO .......: M15
---------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Syst/utumaa.i new }
{Func/feplstart.i}
{Func/cparam2.i}
{Inv/edefine.i new}

DEF INPUT PARAMETER iiITNum   AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
DEF INPUT PARAMETER iiMSSeq   AS INT  NO-UNDO. 

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
DEF VAR liCustNum     AS INT  NO-UNDO.
DEF VAR lcCLI         AS CHAR NO-UNDO.
DEF VAR liPer         AS INT  NO-UNDO.
DEF VAR liMSSeq       AS INT  NO-UNDO. 
DEF VAR liITNum       AS INT  NO-UNDO. 
DEF VAR lcTagLabel    AS CHAR NO-UNDO. 
DEF VAR lcTag         AS CHAR NO-UNDO. 
DEF VAR lcCode        AS CHAR NO-UNDO.
DEF VAR lcTarget      AS CHAR NO-UNDO. 
DEF VAR lcPassword    AS CHAR NO-UNDO. 
DEF VAR lcAskPassWd   AS CHAR NO-UNDO.

DEF BUFFER bOwner FOR Customer.

FORM
   skip(1)
   "Text will be printed either as an EPL-file or a local print." 
      AT 2 
      SKIP(1)

   lcTitle AT 2
      LABEL "Text ........."
      FORMAT "X(50)" 
      SKIP
      
   liCustNum AT 2
      LABEL "Customer ....."
      HELP "Customer that letter is addressed to" 
      FORMAT ">>>>>>>9"
      SKIP
      
   lcCLI AT 2
      label "CLI .........."
      help "A-number that letter is addressed to"
      FORMAT "X(16)"
      SKIP
 
   liAddress AT 2
      LABEL "Target Address"
      HELP "Letter receiver"
      FORMAT "9"
      VALIDATE(INPUT liAddress > 0 AND INPUT liAddress < 6,
               "Valid values are 1-5")
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
          
   FIND Customer WHERE 
        Customer.Brand   = gcBrand AND
        Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
        
   IF iiAddress = 4 THEN      
   FIND bOwner WHERE
        bOwner.Brand   = gcBrand AND
        bOwner.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF iiAddress = 5 THEN      
   FIND bOwner WHERE
        bOwner.Brand   = gcBrand AND
        bOwner.CustNum = Customer.InvCust NO-LOCK NO-ERROR.
         
   IF iiMsSeq > 0 
   THEN FIND FIRST MSOwner NO-LOCK USE-INDEX MSSeq WHERE
                   MsOwner.MSSeq   = iiMSSeq AND
                   MsOwner.CustNum = iiCustNum NO-ERROR.
   ELSE DO:
      liMSSeq = 0.
      FIND FIRST MsOwner NO-LOCK USE-INDEX CLI WHERE
                 MsOwner.Brand   = gcBrand   AND
                 MsOwner.CLI     = lcCLI     AND
                 MsOwner.CustNum = liCustNum 
                 NO-ERROR.
      IF AVAILABLE MsOwner THEN liMSSeq = MsOwner.MSSeq. 
   END.
        
   CASE iiAddress:
   WHEN 1 OR WHEN 3 THEN IF AVAILABLE Customer THEN ASSIGN 
                      lcName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                BUFFER Customer)
                      lcAddr = Customer.Address
                      lcPost = Customer.ZipCode + " " + Customer.PostOffice.
   WHEN 2 THEN IF AVAILABLE Customer THEN ASSIGN 
                      lcName = Customer.IDelName
                      lcAddr = Customer.IDelAddr
                      lcPost = Customer.IDelZipCode + " " +
                               Customer.IDelPost.
   WHEN 4 OR WHEN 5
   THEN IF AVAILABLE bOwner THEN ASSIGN 
                      lcName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                BUFFER bOwner)
                      lcAddr = bOwner.Address
                      lcPost = bOwner.ZipCode + " " + bOwner.PostOffice.
   END CASE.                      

   DISPLAY lcTarget lcName lcAddr lcPost WITH FRAME rajat.
END.

IF iiITNum > 0 THEN DO:

   FIND InvText WHERE InvText.ITNum = iiITNum NO-LOCK NO-ERROR.

   IF NOT AVAILABLE InvText THEN DO:
      MESSAGE "Text was not found"
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.

   IF InvText.Target = "Customer" THEN DO:
      liCustNum = INTEGER(InvText.KeyValue).

      IF iiCustNum > 0 AND liCustNum NE iiCustNum THEN DO:
         MESSAGE "Information text belongs to another customer (" 
                 liCustNum ")."
         VIEW-AS ALERT-BOX 
         ERROR.
         RETURN.
      END.
      
      iiCustNum = liCustNum.
   END.
END.

IF iiMSSeq > 0 THEN DO:
   IF iiCustNum > 0 
   THEN FIND FIRST MSOwner NO-LOCK USE-INDEX MSSeq WHERE
                   MsOwner.MSSeq   = iiMSSeq AND
                   MsOwner.CustNum = iiCustNum NO-ERROR.
   ELSE FIND FIRST MSOwner NO-LOCK USE-INDEX MSSeq WHERE
                   MsOwner.MSSeq   = iiMSSeq AND
                   MsOwner.TsBeg  <= liPer   AND
                   MsOwner.TsEnd  >= liPer NO-ERROR.
   IF NOT AVAILABLE MSOwner THEN DO:
      MESSAGE "Unknown CLI"
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.
    
   IF iiCustNum = 0 THEN iiCustNum = MsOwner.CustNum.
   
   ASSIGN lcCLI     = MsOwner.CLI
          liMSSeq   = iiMSSeq.
END.

IF iiCustNum > 0 THEN DO:

   liCustNum = iiCustNum.
   
   FIND Customer WHERE 
        Customer.Brand   = gcBrand AND
        Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      MESSAGE "Unknown customer" liCustNum
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.

END.   


     
ASSIGN liITNum       = iiITNum
       tuni1         = "InvText"
       tuni2         = ""
       liLetterClass = IF iiITNum > 0 AND InvText.LetterClass > 0 
                       THEN InvText.LetterClass
                       ELSE fCParamI("EPLGenLClass")
       liAddress     = IF iiITNum > 0 AND InvText.AddrTarget > 0 
                       THEN InvText.AddrTarget
                       ELSE IF iiMSSeq > 0 THEN 3 ELSE 1
       llUfkey       = FALSE
       nap           = "1"
       llEPL         = TRUE
       lcTestFlag    = fCParamC("EPLTest")
       liPer         = YEAR(TODAY) * 10000 +
                       MONTH(TODAY) * 100  +
                       DAY(TODAY)          +
                       0.86399
       lcPassword    = fCParamC("MSAddressChg").

IF lcPassword = ? THEN lcPassword = "".
       
toimi:
repeat WITH FRAME rajat ON ENDKEY UNDO toimi, NEXT toimi:

   PAUSE 0.
   DISPLAY liAddress
           llEPL
           liLetterClass
           liCustnum
           lcCLI
           WITH FRAME rajat.
 
   fTargetAddr(liAddress).
   fTxtTitle().
            
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
          lcTitle   WHEN iiItNum   = 0
          liCustNum WHEN iiCustNum = 0
          lcCLI     WHEN iiMSSeq   = 0
          liAddress
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
                   RUN Syst/ufkey.
       
                   IF si-recid > 0 THEN DO:
                      FIND InvText WHERE RECID(InvText) = si-recid 
                      NO-LOCK NO-ERROR.
                      IF AVAILABLE InvText THEN DO:

                         IF InvText.Target = "Customer" AND 
                            INPUT liCustNum > 0         AND
                            INPUT liCustNum NE INTEGER(InvText.KeyValue)
                         THEN DO:
                            BELL.
                            MESSAGE "Text belongs to another customer".
                            NEXT.
                         END.
                         
                         liITNum = InvText.ITNum.
                         IF InvText.LetterClass > 0 THEN DO:
                            liLetterClass = InvText.LetterClass.
                            DISPLAY liLetterClass WITH FRAME rajat.
                         END.
                         IF InvText.AddrTarget > 0 THEN DO:
                            liAddress = InvText.AddrTarget.
                            DISPLAY liAddress WITH FRAME rajat.
                            fTargetAddr(liAddress).
                         END. 
                         
                         fTxtTitle().
                         APPLY "ENTER".
                         NEXT.
                      END.
                   END. 
                   
                END.
                ELSE NEXT. 
             END.

             ELSE IF FRAME-FIELD = "liAddress" AND KEYLABEL(LASTKEY) = "F9" 
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

                IF FRAME-FIELD = "liCustNum" THEN DO:
                   ASSIGN FRAME rajat liCustNum lcCLI.
                   
                   IF AVAILABLE InvText AND 
                      InvText.Target = "Customer" AND
                      INTEGER(InvText.KeyValue) NE INPUT liCustNum
                   THEN DO:
                      BELL.
                      MESSAGE "Text belongs to another customer".
                      NEXT. 
                   END.
                   
                   IF liCustNum > 0 THEN DO:
                      FIND Customer WHERE 
                           Customer.Brand = gcBrand AND
                           Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
                      IF NOT AVAILABLE Customer THEN DO:
                         BELL.
                         MESSAGE "Unknown customer".
                         NEXT.
                      END.
                   END.
                         
                   fTargetAddr(INPUT INPUT FRAME rajat liAddress).
                END.
                
                ELSE IF FRAME-FIELD = "lcCLI" THEN DO:
                   
                   ASSIGN FRAME rajat liCustNum lcCLI.

                   IF lcCli > "" THEN DO: 
                      IF liCustNum > 0 
                      THEN FIND FIRST MsOwner NO-LOCK USE-INDEX CLI WHERE
                                      MsOwner.Brand   = gcBrand AND
                                      MsOwner.CLI     = lcCLI   AND
                                      MsOwner.CustNum = liCustNum 
                           NO-ERROR.
                      ELSE FIND FIRST MsOwner NO-LOCK USE-INDEX CLI WHERE
                                      MsOwner.Brand   = gcBrand AND
                                      MsOwner.CLI     = lcCLI   AND
                                      MsOwner.TsBeg  <= liPer   AND
                                      MsOwner.TsEnd  >= liPer 
                           NO-ERROR.
        
                      IF NOT AVAILABLE MsOwner THEN DO:
                         BELL.
                         MESSAGE "Unknown CLI".
                         NEXT.
                      END.
                   
                      IF liCustNum = 0 THEN DO:
                         liCustNum = MsOwner.CustNum.
                         DISPLAY liCustNum WITH FRAME rajat.
                      END.
                   END.

                   fTargetAddr(INPUT INPUT FRAME rajat liAddress).
                   
                END.
                
                ELSE IF FRAME-FIELD = "liAddress" THEN DO:
                   ASSIGN FRAME rajat liCustNum lcCLI.
                   fTargetAddr(INPUT INPUT FRAME rajat liAddress).
                END.

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

   END.   
   
   else if lookup(nap,"5,f5") > 0 THEN DO:
      
      IF liITNum = 0 THEN DO:
         MESSAGE "Text has not been chosen"
         VIEW-AS ALERT-BOX
         ERROR.
         NEXT.
      END.

      FIND InvText WHERE InvText.ITNum = liItNum NO-LOCK NO-ERROR.
      IF AVAILABLE InvText AND InvText.InfoType = "SYS" THEN DO:
        
          IF lcPassword > "" THEN DO:
             
             lcAskPassWd = "".
            
             PAUSE 0.
             UPDATE lcAskPassWd 
                BLANK
                FORMAT "X(20)" 
                LABEL "Password"
                HELP "Password for printing a system letter"
                WITH OVERLAY ROW 10 CENTERED TITLE " SYSTEM LETTER "
                     SIDE-LABELS FRAME fPassword.
             
             IF lcAskPassWd NE lcPassword THEN DO:
                MESSAGE "Printing denied"
                VIEW-AS ALERT-BOX INFORMATION.
                NEXT. 
             END.
          END.
          ELSE DO:
             MESSAGE "This is a system letter, direct printing"
                    "is not allowed."
             VIEW-AS ALERT-BOX INFORMATION.
             NEXT.
          END.
      END.
       
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

      RUN Mc/printxt (liCustNum,
                   liMsSeq, 
                   "",
                   1,                      /* 1=invtext */
                   liAddress,
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

      IF lcErrFile = "" THEN 
      MESSAGE "Information text has been printed."
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

