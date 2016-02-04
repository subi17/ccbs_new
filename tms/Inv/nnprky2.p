/* ------------------------------------------------------
  MODULE .......: NNPRKY2.P
  FUNCTION .....: Erittely puheluittain raportin print-linerajat
  APPLICATION ..: NN
  AUTHOR .......: TT
  CREATED ......: 01.02.96
  MODIFIED .....: 24.09.96 /tt --> Ruotsinnettu, tulostukset NN-ohjelmalla
                  04.05.98 /kl --> 0 PARAM FOR nnpura2 InvNum 
                  19.10.01/aam     in English
                  12.12.01/aam     eLetter noted 
                  20.01.02  ht     PARAMETER InvNum alternative FOR ALL others
                  29.04.02  ht     PARAMETER lcAtil (A-subscriber)
                  12.09.02 jp      Invoice Number validation
                  19.03.03/aam     MSOwner checked,
                                   text changes etc.
                  29.01.04/aam     generate fee (creasfee)
                  08.04.04/aam     new parameters for creasfee
                  13.04.04/aam     epl, target address
                  06.06.05/aam     ItSendLog
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Syst/utumaa.i "new"}
{Func/feplstart.i}
{Inv/eplspec.i}
{Func/timestamp.i}

assign tuni1 = "nnpura2"
       tuni2 = "".

DEF VAR lcMacros AS CHAR                    NO-UNDO.
DEF VAR ufkey    AS LOG                     NO-UNDO.
def var CustNum1 as int format "zzzzzz9"    NO-UNDO.
def var CustNum2 as int format "zzzzzz9"    NO-UNDO.
def var pvm1     as date format "99-99-99"  NO-UNDO.
def var pvm2     as date format "99-99-99"  NO-UNDO.
def var tilak    as int format "9"          NO-UNDO.
DEF VAR llOk     AS LOGIC                   NO-UNDO. 
DEF VAR InvNum   LIKE InvSeq.InvNum         NO-UNDO.
DEF VAR lcAtil   LIKE CLI.CLI               NO-UNDO.
DEF VAR llUseInv AS LOGIC                   NO-UNDO. 
DEF VAR lleMail  AS LOGIC                   NO-UNDO.
DEF VAR liPDF    AS INT                     NO-UNDO. 
DEF VAR liMail   AS INT                     NO-UNDO. 
DEF VAR llM2Cust AS LOG                     NO-UNDO. 
DEF VAR liError  AS INT                     NO-UNDO. 
DEF VAR llCreFee AS LOG                     NO-UNDO INIT FALSE. 
DEF VAR llCover  AS LOG                     NO-UNDO INIT TRUE. 
DEF VAR lcInfo   AS CHAR                    NO-UNDO. 

DEF VAR liPrintTo     AS INT  NO-UNDO. 
DEF VAR liAddress     AS INT  NO-UNDO. 
DEF VAR lcPrintTarget AS CHAR NO-UNDO. 
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liPer1        AS INT  NO-UNDO.
DEF VAR liPer2        AS INT  NO-UNDO. 

form
   skip(1)
   "Report is printed on call level and sorted by"      AT 10 SKIP
   "Calling Customer / CLI / Billing Item / CCN."       AT 10 
   skip(14)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + 
        " CALL DETAILS (REPORT 2) " +
        string(pvm,"99-99-99") + " "
        FRAME valinta.

form
   skip(1)
   InvNum  
      label "Invoice number .." 
      help "Invoice number, 0 = all"                  
      format ">>>>>>>9"
      SKIP
   CustNum1  
      label "Customers ......." 
      help "If invoice nbr is 0, then these are calling customers"
      format ">>>>>>>9"
   " - " 
   CustNum2 
      no-label 
      help "If invoice nbr is 0, then these are calling customers"
      format ">>>>>>>9"
      SKIP
   pvm1   
      label "Dates ..........." 
      help "From "
   " - " 
   pvm2 
      no-label 
      help "Till "                        
      SKIP
   tilak  
      label "Status code ....."
      help "Status, 0 = not invoiced, 1 = invoiced, 2 = both" 
      SKIP
   lcAtil 
      label "A-subscriber ...."
      help "One A-subscriber; empty = ALL"
      SKIP(1)

   liPrintTo
      LABEL "Printing Target ."
      HELP "1=EPL, 2=Local printer, 3=PDF"
      FORMAT "9"
      VALIDATE(INPUT liPrintTo >= 1 AND INPUT liPrintTo <= 3,
               "Valid values are 1-3")
      lcPrintTarget 
         NO-LABEL
         FORMAT "X(10)"
   liLetterClass AT 40
      LABEL "Letter Class ........"
      HELP "Letter class for EPL"
      FORMAT ">"
      VALIDATE(INPUT liLetterClass >= 1 AND INPUT liLetterClass <= 4,
               "Valid values are 1-4")
      SKIP         

   llCover 
      LABEL "Cover Sheet ....."
      HELP "Print a cover sheet with name and address"
      FORMAT "Yes/No"
   lleMail AT 40 
      LABEL "Send PDF via eMail .." 
      HELP "Send formed PDF-files via eMail to invoicing customer"
      FORMAT "Yes/No"
      SKIP

  
   liAddress 
      LABEL "Target Address .."
      HELP "1=Customer, 2=Invoice delivery, 3=CLI user, 4=Owner"
      FORMAT "9"
      VALIDATE(INPUT liAddress > 0 AND INPUT liAddress < 5,
               "Valid values are 1-4")
   llM2Cust AT 40
      LABEL "Mail to customer/user"
      HELP "Send PDF to invoicing (C)ustomer or to TMS (U)ser (i.e. You)"
      FORMAT "Customer/User"
      SKIP

   llCreFee 
      LABEL "Create fee ......"
      HELP "Create fee for customer"
      FORMAT "Yes/No"
 
   with title " PRINTING CRITERIA " side-labels
   ROW 6 centered OVERLAY FRAME rajat.


FUNCTION fPrintTarget RETURNS LOGICAL
   (iiPrintTo AS INT).

   IF iiPrintTo >= 1 AND iiPrintTo <= 3 
   THEN DISPLAY ENTRY(iiPrintTo,"EPL,Printer,PDF") @ lcPrintTarget
        WITH FRAME rajat.
   
END FUNCTION.

lcMacros = fCParamC("MacroDir") + fCParamC("MacroSpec").

view FRAME valinta.
PAUSE 0 no-message.

ASSIGN pvm1          = DATE(MONTH(TODAY),1,YEAR(TODAY))
       pvm2          = IF MONTH(TODAY) = 12
                       THEN DATE(12,31,YEAR(TODAY))
                       ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1
       custnum2      = 99999999                
       liLetterClass = fCParamI("EPLGenLClass")
       liAddress     = 1
       liPrintTo     = 1
       ufkey         = FALSE
       nap           = "1".

toimi:
repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

      PAUSE 0.
      DISPLAY CustNum1 CustNum2 pvm1 pvm2 tilak InvNum lcAtil 
              lleMail llM2Cust llCreFee llCover
              liPrintTo liAddress liLetterClass
              WITH FRAME rajat.           

      fPrintTarget(liPrintTo).

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3. 
         RUN Syst/ufkey.

         READKEY.
         nap = keylabel(LASTKEY).
      END.
      ELSE ufkey = TRUE.

      if lookup(nap,"1,f1") > 0 THEN DO:
         
         ehto = 9.
         RUN Syst/ufkey.p.

         REPEAT ON ENDKEY UNDO, LEAVE:
         
            llUseInv = FALSE.
            
            UPDATE 
            InvNum
            VALIDATE(INPUT InvNum = 0 OR
                     CAN-FIND (FIRST Invoice WHERE
                                     Invoice.Brand  = gcBrand AND
                                     Invoice.InvNum = INPUT invnum),
            "Unknown Invoice Number!")                   
            WITH FRAME rajat.

            IF InvNum NE 0 THEN DO:
               FIND Invoice WHERE Invoice.InvNum = InvNum NO-LOCK NO-ERROR.
               IF AVAIL Invoice THEN DO: 
                  ASSIGN
                  CustNum1 = Invoice.CustNum
                  CustNum2 = Invoice.CustNum
                  pvm1     = Invoice.FromDate
                  pvm2     = Invoice.ToDate
                  liPer1   = fMake2DT(Invoice.FirstCall,1)
                  liPer2   = fMake2DT(Invoice.ToDate,86399)
                  tilak    = 1
                  llUseInv = TRUE.

                  DISPLAY CustNum1 CustNum2 pvm1 pvm2 tilak WITH FRAME rajat.
                  
               END.
            END.
        
            UPDATE
               CustNum1 WHEN NOT llUseInv
               CustNum2 WHEN NOT llUseInv
                  validate(input CustNum2 >= input CustNum1,
                                 "Check the number !")
               pvm1     WHEN NOT llUseInv
               pvm2     WHEN NOT llUseInv
                  validate(input pvm2 >= input pvm1, "Check the date !")
               tilak    WHEN NOT llUseInv
                  validate(INPUT tilak >= 0 AND INPUT tilak < 3,
                              "Status must be 0 - 2 !")
               lcAtil 
               VALIDATE(INPUT lcAtil = "" OR
                        CAN-FIND(FIRST CLI WHERE 
                                       CLI.CLI = INPUT lcAtil) OR
                        CAN-FIND(FIRST MSOwner WHERE 
                                       MSOwner.Brand = gcBrand AND
                                       MSOwner.CLI = INPUT lcAtil),
                       "Unknown CLI")
               liPrintTo        
            WITH FRAME rajat EDITING:
               
                  READKEY.
           
                  IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
                  THEN DO WITH FRAME rajat:
                  
                     PAUSE 0.
                     IF FRAME-FIELD = "CustNum1" THEN DO:
                        DISPLAY INPUT CustNum1 @ CustNum2.
                     END.
                     ELSE IF FRAME-FIELD = "CustNum2" THEN DO:
                        IF INPUT FRAME rajat CustNum2 = 0 THEN 
                        DISPLAY INPUT CustNum1 @ CustNum2.
                     END.

                     ELSE IF FRAME-FIELD = "liPrintTo" THEN DO:
                        fPrintTarget(INPUT INPUT liPrintTo).
                     END.
                     
                  END.
                  
                  APPLY LASTKEY.
            END.
         
            IF CustNum1 = CustNum2 THEN 
               UPDATE llCover WHEN liPrintTo NE 1 
                      liAddress llCreFee WITH FRAME rajat.
            ELSE ASSIGN llCreFee  = FALSE
                        llCover   = FALSE
                        liPrintTo = 2.
            
            IF liPrintTo = 1 THEN DO:
               llCover = TRUE.
               UPDATE liLetterClass WITH FRAME rajat.
            END.
            
            LEAVE.
         END.
          
         IF liPrintTo NE 3 THEN lleMail = FALSE.
         
      END.
      
      else if lookup(nap,"5,f5") > 0 THEN DO:

         IF liAddress = 3 AND lcAtil = "" THEN DO:
            MESSAGE "CLI must be chosen if it is wanted as a target address."
            VIEW-AS ALERT-BOX
            ERROR.
            NEXT.
         END.
         
         IF liPrintTo = 1 AND lcTestFlag > "" THEN DO:
            IF NOT fEPLStart(lcTestFlag) THEN NEXT.
         END.
         
         LEAVE toimi.
      END.
      
      else if lookup(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
END. /* toimi */

ehto = 5.
RUN Syst/ufkey.

ASSIGN llOk      = TRUE
       lcErrFile = "".

IF llCover THEN DO:
   IF lcAtil > "" AND liAddress = 3 THEN DO: 
   
      IF NOT llUseInv THEN DO:
         ASSIGN liPer1 = fMake2DT(pvm1,1)
                liPer2 = fMake2DT(pvm2,86399).
   
         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.Brand   = gcBrand AND
                    MsOwner.CustNum = CustNum1 AND
                    MsOwner.CLI     = lcAtil   AND
                    MsOwner.TsBeg  <= liPer2   AND
                    MsOwner.TsEnd  >= liPer1 NO-ERROR.
      END. 
                    
      ELSE 
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.Brand   = gcBrand AND
                 MsOwner.CLI     = lcAtil  AND
                 MsOwner.TsBeg  <= liPer2  AND
                 MsOwner.TsEnd  >= liPer1 NO-ERROR.
      
      IF NOT AVAILABLE MSOwner THEN DO:
         MESSAGE "CLI data was not found for" lcAtil
         VIEW-AS ALERT-BOX
         ERROR.
         RETURN.
      END.           
   END.
 
   /* set target address */
   llOk = fSetAddress(IF liAddress = 3 AND AVAILABLE MSOwner          
                      THEN MsOwner.CustNum
                      ELSE CustNum1,
                      IF AVAILABLE MSOwner 
                      THEN MSOwner.MSSeq 
                      ELSE 0,
                      liAddress).
   
   IF NOT llOk THEN DO:
      MESSAGE "Target address could not be set."
      VIEW-AS ALERT-BOX
      ERROR.
      RETURN.
   END.

   MESSAGE SKIP 
           lcEPLRName   SKIP
           lcEPLRCoName SKIP
           lcEPLRAddr   SKIP
           lcEPLRPost   SKIP(1)
           "Start printing the specification ?"
   VIEW-AS ALERT-BOX
   QUESTION
   BUTTONS YES-NO
   TITLE " TARGET ADDRESS WILL BE "
   SET llOk.

   IF NOT llOk THEN RETURN.

END.

/* epl */
IF liPrintTo = 1 THEN DO:
  
   llOk = fStartEPL().
END.

/* local print */
ELSE IF liPrintTo = 2 THEN DO:
  
   ASSIGN tila = TRUE.
   {Syst/utuloste.i "return"}

END.

/* cover sheet */
IF llCover THEN llCaSivu = -1.

IF llOk THEN 
RUN Inv/nnpura2 (INPUT CustNum1,
             INPUT CustNum2,
             INPUT pvm1,
             INPUT pvm2,
             INPUT tilak,
             INPUT InvNum,
             INPUT lcAtil,
             INPUT liPrintTo,
             INPUT (IF lleMail
                    THEN (IF llM2Cust
                          THEN 1
                          ELSE 2)
                    ELSE 0),
             OUTPUT liPDF,
             OUTPUT liMail,
             OUTPUT liError).

IF liPrintTo <= 2 THEN DO:

    IF liError = -1 THEN DO:
       MESSAGE "Nothing to print"
       VIEW-AS ALERT-BOX ERROR.
       llCreFee = FALSE.
    END.
    
    IF liPrintTo = 1 THEN DO:

      IF liError NE 0 THEN llErrors = TRUE.
      
      lcErrFile = fEndEPL().
 
      IF lcErrFile > "" THEN DO:
         MESSAGE "Error occurred while creating an EPL file." SKIP
                 "Messages are in file" lcErrFile "." 
         VIEW-AS ALERT-BOX
         ERROR.
         llCreFee = FALSE.
      END.
   END.
 
   ELSE IF liPrintTo = 2 THEN DO: 
      ASSIGN tila = FALSE.
      {Syst/utuloste.i}
   END.
   
   IF lcErrFile = "" AND liError NE -1 THEN DO:
   
      /* log from print */
      IF CustNum1 = CustNum2 THEN 
      DO FOR ITSendLog TRANS:
         CREATE ITSendLog.
         ASSIGN ITSendLog.Brand      = gcBrand 
                ITSendLog.TxtType    = 5
                ITSendLog.ITNum      = 0
                ITSendLog.CustNum    = CustNum1
                ITSendLog.InvNum     = InvNum
                ITSendLog.SendMethod = IF liPrintTo = 1 
                                       THEN 2
                                       ELSE 4
                ITSendLog.EMail      = ""
                ITSendLog.RepType    = "Spec2"
                ITSendLog.UserCode   = katun.
                ITSendLog.SendStamp  = fMakeTS().
      END.
       
      MESSAGE "Report 2 has been printed."
      VIEW-AS ALERT-BOX 
      TITLE " Done ".
   END.
   
END.
ELSE IF liPrintTo = 3 THEN DO:
   MESSAGE liPDF      "PDF-files were made from reports." SKIP
           liMail     "were sent via eMail." SKIP
           liError    "errors occurred during process."                    
   VIEW-AS ALERT-BOX
   INFORMATION.
END.

/* create fee */
IF llCreFee AND liError = 0 AND CustNum1 = CustNum2 THEN 
RUN Mc/creasfee (CustNum1,
              0,
              TODAY,
              "InvSpec",
              "2",
              2,
              ?,
              "",
              TRUE,
              katun,
              "",
              0,
              "",
              "",
              OUTPUT lcInfo).

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

