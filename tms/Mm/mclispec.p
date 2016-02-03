/* ------------------------------------------------------
  MODULE .......: MCLISPEC
  FUNCTION .....: Specification reports directly from cli maintenance
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 23.01.04 
  MODIFIED .....: 09.02.04/aam gcHelpParam
                  26.10.04/aam new parameter to nnpura4: full b-numbers
                  03.01.05/aam RepCode from SubSer
                  06.06.05/aam ItSendLog
                  17.01.06/aam RepCode can be only 4,
                               frame title from tmscodes
                  07.08.06/aam letterclass to nnpura3/4
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Syst/utumaa.i "new"}
{Inv/eplspec.i}
{Func/feplstart.i}
{Func/fsubser.i}
{Func/timestamp.i}

DEF INPUT PARAMETER icCLI AS CHAR NO-UNDO.

DEF VAR lcMacros AS CHAR                               NO-UNDO.
DEF VAR ufkey    AS LOG                                NO-UNDO.
def var pvm1     as date format "99-99-99" init ?      NO-UNDO.
def var pvm2     as date format "99-99-99" init TODAY  NO-UNDO.
def var tilak    as int format "9"                     NO-UNDO.
DEF VAR llStart  AS LOGIC                              NO-UNDO. 
DEF VAR InvNum   LIKE InvSeq.InvNum                    NO-UNDO.

DEF VAR liCustNum   AS INT  NO-UNDO. 
DEF VAR lcRepCodes  AS CHAR NO-UNDO init 4. 
DEF VAR lcCode      AS CHAR NO-UNDO. 
DEF VAR liSpec      AS INT  NO-UNDO. 
DEF VAR llPDF       AS LOG  NO-UNDO. 
DEF VAR lleMail     AS LOG  NO-UNDO.
DEF VAR liPDF       AS INT  NO-UNDO. 
DEF VAR liMail      AS INT  NO-UNDO. 
DEF VAR llM2Cust    AS LOG  NO-UNDO. 
DEF VAR liError     AS INT  NO-UNDO. 
DEF VAR llEPL       AS LOG  NO-UNDO. 
DEF VAR llOk        AS LOG  NO-UNDO.
DEF VAR lcErrFile   AS CHAR NO-UNDO. 
DEF VAR lcUser      AS CHAR NO-UNDO. 
DEF VAR llCreaFee   AS LOG  NO-UNDO. 
DEF VAR ldFeeAmt    AS DEC  NO-UNDO. 
DEF VAR lcMainTitle AS CHAR NO-UNDO.

DEF BUFFER bInvCust FOR Customer.

form
   skip(1)
   icCLI AT 3
      label "CLI .........."
      help "One A-subscriber; empty = ALL"
      lcUser NO-LABEL FORMAT "X(30)"
      SKIP
   lcRepCodes AT 3
      LABEL "Report Code .."
      HELP "Report to be printed"
      FORMAT "X" 
      SKIP
   InvNum AT 3 
      label "Invoice number" 
      help "Invoice number, 0 = all"                  
      format ">>>>>>>9"
      SKIP
   pvm1  AT 3 
      label "Dates ........" 
      help "From "
   " - " 
   pvm2  
      no-label 
      help "Till "                        
      SPACE(2)
      SKIP
   tilak AT 3 
      label "Status code .."
      help "Status, 0 = not invoiced, 1 = invoiced, 2 = both" 
      SKIP(1)
   llEPL AT 3
      LABEL "Print to ....."
      HELP "Print to (E)PL or to (P)rinter"
      FORMAT "EPL/Printer"
      SKIP
   liLetterClass AT 3
      LABEL "Letter Class ."
      HELP "Letter class for EPL"
      FORMAT ">"
      SKIP(1)
   llCreaFee AT 3 
      LABEL "Create Fee ..."
      HELP "Create fee for customer"
      FORMAT "Yes/No"
      SKIP(1)
   with title " PRINT " + lcMainTitle + " " side-labels
   ROW 5 centered OVERLAY FRAME rajat.

ASSIGN lcMacros      = fCParamC("MacroDir") + fCParamC("MacroSpec")
       liLetterClass = fCParamI("EPLSpecClass")
       lcMainTitle   = CAPS(DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                             "MobSub",
                                             "RepCodes",
                                             "4")).

FIND MobSub WHERE 
     MobSub.Brand = gcBrand AND
     MobSub.CLI   = icCLI NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   MESSAGE "Unknown CLI" icCLI
   VIEW-AS ALERT-BOX.
   RETURN.
END.

lcRepCodes = "4".


ASSIGN pvm1      = DATE(MONTH(TODAY),1,YEAR(TODAY))
       pvm2      = IF MONTH(TODAY) = 12
                   THEN DATE(12,31,YEAR(TODAY))
                   ELSE DATE(MONTH(TODAY) + 1,1,YEAR(TODAY)) - 1
       llStart   = TRUE
       liCustNum = MobSub.CustNum
       llPDF     = FALSE
       llEMail   = FALSE
       llEPL     = TRUE
       llCreaFee = TRUE
       gcHelpParam = "".
       
FOR FIRST Customer OF MobSub NO-LOCK:
   gcHelpParam = STRING(Customer.InvCust).
   lcUser      = DYNAMIC-FUNCTION("fDispCustName" IN
                 ghFunc1, BUFFER Customer).
END. 

PAUSE 0.
VIEW FRAME rajat.
ufkey = FALSE.

toimi:
repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

   PAUSE 0.
   DISPLAY lcRepCodes
           icCLI
           lcUser
           InvNum 
           pvm1 
           pvm2
           tilak
           llEPL
           liLetterClass
   WITH FRAME rajat. 

   IF ufkey THEN DO:
      ASSIGN
      ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
      ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
      ehto = 3 ufkey = FALSE.
      RUN ufkey.p.
   END.

   IF llStart THEN ASSIGN nap     = "1"
                          llStart = FALSE.
   ELSE DO:
      READKEY.
      nap = keylabel(LASTKEY).
   END.

   if lookup(nap,"1,f1") > 0 THEN DO:
   
      ASSIGN ehto = 9 
             ufkey = TRUE. 
      RUN ufkey.p.

      REPEAT ON ENDKEY UNDO, LEAVE:
      
          UPDATE 
          InvNum
             VALIDATE(CAN-FIND (FIRST Invoice WHERE
                                      Invoice.Brand  = gcBrand AND
                                      Invoice.InvNum = INPUT invnum),
                      "Unknown Invoice Number!")                   
          pvm1 
          pvm2
             VALIDATE(INPUT pvm2 >= INPUT pvm1,
                      "Upper limit cannot be less")
          tilak 
             validate(INPUT tilak >= 0 AND INPUT tilak < 3,
                      "Status must be 0 - 2 !")
          llEPL            
          liLetterClass
          llCreaFee
          WITH FRAME rajat EDITING:
          
             READKEY.
             
             IF FRAME-FIELD = "lcRepCodes" AND keylabel(lastkey) = "F9" 
             THEN DO:
              
                RUN h-tmscodes(INPUT "Mobsub",   /* TableName*/
                                     "RepCodes", /* FieldName */
                                     "Report",   /* GroupCode */
                               OUTPUT lcCode).

                IF lcCode NE "" AND lcCode NE ? THEN 
                DISPLAY lcCode @ lcRepCodes WITH FRAME rajat.
                
                ehto = 9.
                RUN ufkey.
                
                NEXT.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
             THEN DO WITH FRAME rajat:
             
                PAUSE 0.

                IF FRAME-FIELD = "InvNum" THEN DO:
               
                   IF INPUT InvNum NE 0 THEN DO:
                      FIND Invoice WHERE 
                           Invoice.InvNum = INPUT InvNum 
                      NO-LOCK NO-ERROR.
                      IF AVAIL Invoice AND Invoice.Brand = gcBrand THEN DO: 
                      
                         IF NOT CAN-FIND(FIRST SubInvoice OF Invoice WHERE
                                               SubInvoice.CLI = icCLI)
                         THEN DO:
                            BELL.
                            MESSAGE "Chosen CLI does not appear on this"
                                    "invoice.".
                            NEXT.
                         END.
                         
                         ASSIGN
                         pvm1  = Invoice.FromDate
                         pvm2  = Invoice.ToDate
                         tilak = 1.

                         DISPLAY pvm1 pvm2 tilak WITH FRAME rajat.
                      END.
                      ELSE DO:
                         BELL. 
                         MESSAGE "Unknown invoice".
                         NEXT.
                      END.
                   END.
                END.

             END.
             
             APPLY LASTKEY. 
             
          END.
          
          LEAVE.
      END.

      IF InvNum > 0 THEN tilak = 1. 
      
   END.
   
   else if lookup(nap,"5,f5") > 0 THEN DO:
      
      IF InvNum = 0 THEN DO:
         MESSAGE "Invoice number is mandatory."
         VIEW-AS ALERT-BOX.
         NEXT.
      END. 

      IF lcRepCodes NE "4" THEN DO:
         MESSAGE "Valid report code is 4."
         VIEW-AS ALERT-BOX.
         NEXT.
      END.
         
      ehto = 5. 
      RUN ufkey.
      
      llOk = FALSE.
      
      /* set address */
      fSetAddress(MobSub.CustNum,
                  MobSub.MSSeq,
                  3).  /* addressed to cli user */
                  
      /* epl -> create a new letter */
      IF llEPl THEN DO:
         IF fEPLStart(lcTestFlag) THEN 
         llOk = fStartEPL().
      END.
      
      ELSE DO:
         ASSIGN tila     = TRUE
                tuni1    = "nnpura" + lcRepCodes
                tuni2    = ""
                llCaSivu = -1.
       
         {Syst/utuloste.i "return"}

         llOk = TRUE.
      END.
      
      liError = 0.
      
      IF llOk THEN 
      CASE lcRepCodes:
      WHEN "3" THEN 
         RUN nnpura3 (INPUT liCustNum,
                      INPUT pvm1,
                      INPUT pvm2,
                      INPUT tilak,
                      INPUT InvNum,
                      INPUT icCLI,
                      INPUT (IF llPDF 
                             THEN 3
                             ELSE IF llEPL 
                                  THEN 1
                                  ELSE 2),
                      INPUT liLetterClass,            
                      OUTPUT liError).
                                  
      WHEN "4" THEN                
         RUN nnpura4 (INPUT liCustNum,
                      INPUT liCustNum,
                      INPUT pvm1,
                      INPUT pvm2,
                      INPUT tilak,
                      INPUT InvNum,
                      INPUT FALSE,             /* not full b-numbers */
                      INPUT icCLI,
                      INPUT (IF llPDF 
                             THEN 3
                             ELSE IF llEPL 
                                  THEN 1
                                  ELSE 2),
                      INPUT (IF lleMail
                             THEN (IF llM2Cust
                                   THEN 1
                                   ELSE 2)
                             ELSE 0),
                      INPUT "menu",
                      INPUT liLetterClass,
                      OUTPUT liPDF,
                      OUTPUT liMail,
                      OUTPUT liError).
      END CASE.
      
      IF liError = -1 THEN DO:
         MESSAGE "Nothing to print"
         VIEW-AS ALERT-BOX ERROR.
         llCreaFee = FALSE.
      END.
 
      IF llEPL THEN DO:
         
         IF liError NE 0 THEN llErrors = TRUE.
         
         lcErrFile = fEndEPL().
         
         IF lcErrFile > "" THEN DO:
            MESSAGE "Error occurred while creating an EPL file." SKIP
                    "Messages are in file" lcErrFile "." 
            VIEW-AS ALERT-BOX
            ERROR.
            llCreaFee = FALSE.
         END.
      END.
      
      ELSE DO:
         tila = FALSE.
         {Syst/utuloste.i}
      END.
      
      /* create usage fee */
      IF llOk AND llCreaFee THEN DO:
         
         RUN creasfee.p(MobSub.CustNum,
                       MobSub.MSSeq,
                       TODAY,
                       "CLISpec",
                       lcRepCodes,
                       2,
                       ?,
                       "",
                       TRUE,
                       katun,
                       "",
                       0,
                       "",
                       "",
                       OUTPUT lcCode).
      END.
      
      IF liError NE -1 THEN DO:

         /* log from print */
         DO FOR ITSendLog TRANS:
            CREATE ITSendLog.
            ASSIGN ITSendLog.Brand      = gcBrand 
                   ITSendLog.TxtType    = 5
                   ITSendLog.ITNum      = 0
                   ITSendLog.CustNum    = MobSub.CustNum
                   ITSendLog.InvNum     = InvNum
                   ITSendLog.SendMethod = IF llEPL
                                          THEN 2
                                          ELSE 4
                   ITSendLog.EMail      = ""
                   ITSendLog.RepType    = "Spec" + lcRepCodes
                   ITSendLog.UserCode   = katun.
                   ITSendLog.SendStamp  = fMakeTS().
         END.
         
         MESSAGE "Specification report has been printed."
         VIEW-AS ALERT-BOX
         TITLE " DONE ".
      END.
      
      LEAVE toimi.
   END.

   else if lookup(nap,"8,f8") > 0 THEN DO:
      LEAVE toimi.
   END.
      
END. /* toimi */

gcHelpParam = "".

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

