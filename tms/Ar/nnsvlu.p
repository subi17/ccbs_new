/* ===========================================================================
 MODULE ........: nnsvlu.p
 APPLICATION ...: nn
 TASK ..........: Reads Direct Debit Authorization file
 CREATED .......: 05.11.1999 
 CHANGED .......: 02.11.01 ht
                  21.11.01 ht  DELETE off from termination
                  27.11.01 ht  previous back
                               REMARK: Identification = CustNum
                  28.02.03 aam mark Customer.ChargeType,
                               wait for F5 before starting 
                  05.05.03 aam transfer to processed, 
                               use choosefile.p,
                               allow many days in one file
                  17.06.03/aam old bankaccount not mandatory on changes 
                  17.09.03/aam brand 
                  09.02.04/aam if customer not found try to find cli
                  16.07.04/aam directory and file name separately
                  03.08.04/aam remove leading letters from identification
                  10.08.04/aam AuthID
                  02.09.04/aam save failed lines to db with custnum 0
                  29.03.05/aam default date from header
                  12.04.05/aam skip empty rows and rows beginning with "$"
                  30.05.05/aam logic to ddauthin.p
                  26.05.06/aam use RefPrintDir
 VERSION .......: M15
============================================================================*/

{Syst/commali.i}
{Syst/utumaa.i "new"}
{Func/cparam2.i}
{Ar/ddtrans.i}

ASSIGN tuni1 = "nnsvlu"
       tuni2 = "".

DEF VAR lcInfile   AS CHAR  NO-UNDO FORMAT "x(55)".
DEF VAR lcDir      AS CHAR  NO-UNDO.
DEF VAR lcFile     AS CHAR  NO-UNDO. 
DEF VAR lcFromDir  AS CHAR  NO-UNDO. 
DEF VAR lcChoose   AS CHAR  NO-UNDO. 
DEF VAR llMove     AS LOG   NO-UNDO. 
DEF VAR liCount    AS INT   NO-UNDO. 

lcInfile  = fCparamC("DDebitAuthFile").

lcFromDir = "".
IF INDEX(lcInfile,"/") GT 0
THEN ASSIGN liCount   = R-INDEX(lcInfile,"/")
            lcFromDir = SUBSTRING(lcInfile,1,liCount - 1)
            lcInfile  = SUBSTRING(lcInfile,liCount + 1).


FORM  
   SKIP(4)
   "Direct debit authorizations will be read in." AT 10 SKIP
   SKIP(4)
   lcFromDir AT 10 
      LABEL "Directory" 
      FORMAT "x(55)" SKIP
   lcInfile  AT 10 LABEL "File Name" 
       help "Name of input file (F9 = list of files)"
   SKIP(6) 
   WITH WIDTH 80 ROW 1 SIDE-LABELS
    COLOR VALUE(cfc) TITLE COLOR VALUE(ctc) " " + ynimi +
       " IMPORT DD AUTHORIZATIONS "  + string(pvm,"99-99-99") + " "
    FRAME fStart.


PAUSE 0.
VIEW FRAME fStart.

MAIN:
REPEAT WITH FRAME fStart:

   ehto = 9. RUN ufkey.

   REPEAT WITH FRAME fStart ON ENDKEY UNDO, LEAVE:
   
    PAUSE 0.
    DISPLAY lcFromDir WITH FRAME fStart.
    
    UPDATE
    lcInfile
    WITH FRAME fStart EDITING:

        READKEY.

        IF KEYLABEL(LASTKEY) = "F9" THEN DO:

           lcDir = "".
           IF INDEX(INPUT lcInfile,"*") = 0 AND
              INDEX(INPUT lcInfile,"?") = 0 AND
              INDEX(INPUT lcInfile,"/") GT 0
           THEN ASSIGN liCount = R-INDEX(INPUT lcInfile,"/")
                       lcDir   = SUBSTRING(INPUT lcInfile,1,liCount - 1).

           IF lcDir > "" THEN lcChoose = lcDir.
           ELSE DO:
              lcChoose = INPUT lcInFile.
              IF lcFromDir > ""
              THEN lcChoose = lcFromDir + "/" + lcChoose + "¤yes".
           END.
           
           RUN choosefile (lcChoose,
                           OUTPUT lcFile).

           IF lcFile NE "" THEN DO:
              lcInfile = (IF lcDir NE ""
                          THEN lcDir + "/"
                          ELSE "") + lcFile.
              DISPLAY lcInfile.
           END. 

           ehto = 9.
           RUN ufkey.

        END. 

        ELSE APPLY LASTKEY. 
    END. 
    
    LEAVE.
    
   END.
   
   ASSIGN ufk = 0
          ufk[1] = 7
          ufk[5] = (IF lcInfile NE "" THEN 795 ELSE 0)
          ufk[8] = 8
          ehto = 0.
   run ufkey.p.

   IF TOIMI = 5 THEN LEAVE. 

   ELSE IF toimi = 8 THEN DO:
      HIDE FRAME fStart NO-PAUSE.
      RETURN.
   END. 

END.

IF lcFromDir > "" THEN lcInfile = lcFromDir + "/" + lcInFile.

IF SEARCH(lcInfile) NE lcInfile THEN DO:
   MESSAGE
   "Direct Debit Authorization File"   SKIP
   lcInfile                              SKIP
   "does not exist - reading is cancelled !"
   VIEW-AS ALERT-BOX ERROR
   TITLE " FILE DOES NOT EXIST ".
   RETURN.
END.   

/* Open STREAM */
ASSIGN tila = TRUE
       oso  = "-"  + 
              fCParamC("RefPrintDir") + "/" +
              "svval_"                   +
              STRING(YEAR(TODAY),"9999") +
              STRING(MONTH(TODAY),"99")  +
              STRING(DAY(TODAY),"99")    +
              "_" + STRING(TIME) + ".txt".

{Syst/utuloste.i "return"}

ehto = 5.
RUN ufkey.

RUN ddauthin (lcInFile, 
              TRUE,      /* show messages */
              TRUE,      /* send mail     */
              OUTPUT liCount).
              
 
llMove = (liCount >= 0).

IF liCount = 0 THEN 
MESSAGE "No authorizations were found. Can the file be moved to archive"
        "directory ?"
VIEW-AS ALERT-BOX
QUESTION
BUTTONS YES-NO
SET llMove.

ELSE IF liCount < 0 THEN 
MESSAGE "Authorization file was invalid. Can the file be moved to archive"
        "directory ?"
VIEW-AS ALERT-BOX
QUESTION
BUTTONS YES-NO
SET llMove.


/* move to archive */
IF llMove THEN RUN pDDAuthTrans(lcInFile).
   
HIDE FRAME fStart NO-PAUSE.



