/* ------------------------------------------------------
  MODULE .......: NNPRKY3.P
  FUNCTION .....: Puheluraportin print-linerajat
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 26.03.03 (from nnprky2)
  MODIFIED .....: 12.09.03/aam brand
                  29.01.04/aam generate fee (creasfee)
                  08.04.04/aam new parameters for creasfee
                  13.04.04/aam epl, target address
                  06.06.05/aam ItSendLog
                  07.08.06/aam letterclass to nnpura3
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Syst/utumaa.i "new"}
{Func/feplstart.i}
{Inv/eplspec.i}
{Func/timestamp.i}

assign tuni1 = "nnpura3"
       tuni2 = "".

DEF VAR lcMacros AS CHAR                               NO-UNDO.
DEF VAR ufkey    AS LOG                                NO-UNDO.
def var CustNum as int format "zzzzzz9" init "0"      NO-UNDO.
def var pvm1     as date format "99-99-99" init ?      NO-UNDO.
def var pvm2     as date format "99-99-99" init TODAY  NO-UNDO.
def var tilak    as int format "9"                     NO-UNDO.
DEF VAR dkk      AS INT                                NO-UNDO.
DEF VAR dvv      AS INT                                NO-UNDO.
DEF VAR llOk     AS LOGIC                              NO-UNDO. 
DEF VAR InvNum   LIKE InvSeq.InvNum                    NO-UNDO.
DEF VAR lcAtil   LIKE CLI.CLI                          NO-UNDO.
DEF VAR llCreFee AS LOG                                NO-UNDO INIT FALSE. 
DEF VAR llCover  AS LOG                                NO-UNDO INIT TRUE. 
DEF VAR lcInfo   AS CHAR                               NO-UNDO. 

DEF VAR liPrintTo     AS INT  NO-UNDO. 
DEF VAR liAddress     AS INT  NO-UNDO. 
DEF VAR lcPrintTarget AS CHAR NO-UNDO. 
DEF VAR lcErrFile     AS CHAR NO-UNDO. 
DEF VAR liPer1        AS INT  NO-UNDO.
DEF VAR liPer2        AS INT  NO-UNDO. 
DEF VAR liError       AS INT  NO-UNDO. 


form
   skip(1)
   "Report is printed on summary level and sorted by"      AT 10 SKIP
   "CLI / CCN." AT 10 
   skip(14)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + 
        " CALL SUMMARY PER CLI (REPORT 3) " +
        string(pvm,"99-99-99") + " "
        FRAME valinta.

form
   skip(1)
   InvNum  
      label "Invoice Number " 
      help "Invoice number, 0 = all"                  
      format ">>>>>>>9"
      SKIP
   lcAtil 
      label "A-subscriber .."
      help "One A-subscriber; empty = ALL"
      SKIP
   pvm1   
      label "Dates ........." 
      help "From "
   " - " 
   pvm2 
      no-label 
      help "Till "                        
      SKIP(1)

   liPrintTo
      LABEL "Printing Target"
      HELP "1=EPL, 2=Local printer"
      FORMAT "9"
      VALIDATE(INPUT liPrintTo >= 1 AND INPUT liPrintTo <= 2,
               "Valid values are 1-2")
      lcPrintTarget 
         NO-LABEL
         FORMAT "X(10)"
      SKIP   
   liLetterClass 
      LABEL "Letter Class .."
      HELP "Letter class for EPL"
      FORMAT ">"
      VALIDATE(INPUT liLetterClass >= 1 AND INPUT liLetterClass <= 4,
               "Valid values are 1-4")
      SKIP         
   llCover 
      LABEL "Cover Sheet ..."
      HELP "Print a cover sheet with name and address"
      FORMAT "Yes/No"
      SKIP
   liAddress 
      LABEL "Target Address "
      HELP "1=Customer, 2=Invoice delivery, 3=CLI user, 4=Owner"
      FORMAT "9"
      VALIDATE(INPUT liAddress > 0 AND INPUT liAddress < 5,
               "Valid values are 1-4")
      SKIP         
   llCreFee 
      LABEL "Create Fee ...."
      HELP "Create fee for customer"
      FORMAT "Yes/No"
      SKIP(1)
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
       liLetterClass = fCParamI("EPLGenLClass")
       liAddress     = 1
       liPrintTo     = 1
       ufkey         = FALSE
       nap           = "1".

toimi:
repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
 
      PAUSE 0.
      DISPLAY pvm1 pvm2 InvNum llCreFee llCover
              liPrintTo liAddress liLetterClass
      WITH FRAME rajat.
        
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3.
         RUN Syst/ufkey.p.
         READKEY.
         nap = keylabel(LASTKEY).
      END.
      ELSE ufkey = TRUE.

      if lookup(nap,"1,f1") > 0 THEN DO:
         ASSIGN ehto = 9 ufkey = TRUE. 
         RUN Syst/ufkey.p.

         REPEAT ON ENDKEY UNDO, LEAVE:
            UPDATE 
            InvNum
            VALIDATE(CAN-FIND (FIRST Invoice WHERE
                               Invoice.Brand  = gcBrand AND
                               Invoice.InvNum = INPUT invnum),
            "Unknown Invoice Number!")                   
            lcAtil 
            VALIDATE(INPUT lcAtil = "" OR
                     CAN-FIND(FIRST CLI WHERE 
                                    CLI.CLI = INPUT lcAtil) OR
                     CAN-FIND(FIRST MSOwner WHERE 
                                    MSOwner.Brand = gcBrand AND
                                    MSOwner.CLI = INPUT lcAtil),
                     "Unknown CLI")
            liPrintTo
            liLetterClass
            llCover
            liAddress
            llCreFee         
            WITH FRAME rajat.

            IF InvNum NE 0 THEN DO:
               FIND Invoice WHERE Invoice.InvNum = InvNum NO-LOCK NO-ERROR.
               IF AVAIL Invoice THEN DO: 
                  ASSIGN
                  CustNum = Invoice.CustNum
                  pvm1    = Invoice.FromDate
                  pvm2    = Invoice.ToDate
                  liPer1  = fMake2DT(Invoice.FirstCall,1)
                  liPer2  = fMake2DT(Invoice.ToDate,86399)
                  tilak   = 1.

                  DISPLAY pvm1 pvm2 WITH FRAME rajat.
               END.
            END.

            ELSE DO:
               UPDATE
               pvm1
               pvm2  validate(input pvm2 >= input pvm1, "Check the date !")
               WITH FRAME rajat.
            END.

            LEAVE.
         END.

         IF liPrintTo = 1 THEN llCover = TRUE.

      END.

      else if lookup(nap,"5,f5") > 0 AND InvNum > 0 THEN DO:
        
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
RUN Syst/ufkey.p.

ASSIGN llOk      = TRUE
       lcErrFile = "".

IF llCover THEN DO:

   IF lcAtil > "" AND liAddress = 3 THEN DO: 
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
                      ELSE CustNum,
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

   RUN Syst/umakro.p (lcMacros).
END.

/* cover sheet */
IF llCover THEN llCaSivu = -1.

IF llOk THEN 
RUN Inv/nnpura3.p (INPUT CustNum,
             INPUT pvm1,
             INPUT pvm2,
             INPUT tilak,
             INPUT InvNum,
             INPUT lcAtil,
             INPUT liPrintTo,
             INPUT liLetterClass,
             OUTPUT liError).

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
   DO FOR ITSendLog TRANS:
      CREATE ITSendLog.
      ASSIGN ITSendLog.Brand      = gcBrand 
             ITSendLog.TxtType    = 5
             ITSendLog.ITNum      = 0
             ITSendLog.CustNum    = CustNum
             ITSendLog.InvNum     = InvNum
             ITSendLog.SendMethod = IF liPrintTo = 1 
                                    THEN 2
                                    ELSE 4
             ITSendLog.EMail      = ""
             ITSendLog.RepType    = "Spec3"
             ITSendLog.UserCode   = katun.
             ITSendLog.SendStamp  = fMakeTS().
   END.
    
   MESSAGE "Report 3 has been printed."
   VIEW-AS ALERT-BOX 
   TITLE " Done ".
END.

/* create fee */
IF llCreFee THEN
RUN Mc/creasfee.p (CustNum,
              0,
              TODAY,
              "InvSpec",
              "3",
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

