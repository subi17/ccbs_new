/* ===========================================================================
 MODULE ........: intrumcl.p
 APPLICATION ...: TMS
 TASK ..........: ui for reading a File from Intrum Justitia with invoices 
                  whose claiming has been cancelled (some are posted as credit
                  loss)
 CREATED .......: 31.10.02/aam 
 CHANGED .......: 
 VERSION .......: M15
 ============================================================================*/

{commali.i}                      
{cparam2.i}
{ftransdir.i}
{utumaa.i NEW}
{intrumcr.i}

ASSIGN tuni1 = "intrumcl"
       tuni2 = "".

DEF VAR lcDefFile AS CHAR   NO-UNDO.
DEF VAR lcInfile  AS CHAR   NO-UNDO FORMAT "x(55)".
DEF VAR lcArchive AS CHAR   NO-UNDO.
DEF VAR liCount   AS INT    NO-UNDO.
DEF VAR liRead    AS INT    NO-UNDO. 
DEF VAR liFound   AS INT    NO-UNDO. 
DEF VAR llOk      AS LOGIC  NO-UNDO FORMAT "Yes/No".
DEF VAR lcDir     AS CHAR   NO-UNDO.
DEF VAR lcFile    AS CHAR   NO-UNDO. 

ASSIGN
   lcDefFile = fCParamC("IntrumCLossFile")
   lcArchive = fCParamC("IntrumArchive")
   lcInfile  = lcDefFile.


FORM 
skip(4)
"This program reads an ASCII file from Intrum Justitia, with"   AT 10 SKIP
"invoices for which claiming has been cancelled. Cancellation"  AT 10 SKIP
"codes 4,5,6,7 and 9 are booked as credit loss."                AT 10 
SKIP(4)
lcInfile AT 10 LABEL "File Name" 
    help "Name of Output File"
SKIP(5)
WITH  OVERLAY ROW 1 WIDTH 80
   COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) 
   " " + ynimi + " CREDIT LOSS POSTING  " + STRING(pvm,"99-99-99") + " "
   SIDE-LABELS FRAME main.

MAIN:
REPEAT WITH FRAME main:

   ehto = 9. RUN ufkey.

    PAUSE 0.
    UPDATE
    lcInfile
    WITH FRAME main EDITING:

        READKEY.

        IF KEYLABEL(LASTKEY) = "F9" THEN DO:

           lcDir = "".
           IF INDEX(INPUT lcInfile,"*") = 0 AND
              INDEX(INPUT lcInfile,"?") = 0 AND
              INDEX(INPUT lcInfile,"/") GT 0
           THEN ASSIGN liCount = R-INDEX(INPUT lcInfile,"/")
                       lcDir   = SUBSTRING(INPUT lcInfile,1,liCount - 1).

           RUN choosefile (IF lcDir NE "" 
                           THEN lcDir
                           ELSE INPUT lcInfile,
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

    ACTION:
    REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = (IF lcInfile ne "" THEN 795 ELSE 0).
      ufk[8] = 8.
      RUN ufkey.

      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:

         IF SEARCH(lcInfile) = ? THEN DO:
            MESSAGE "File cannot be found. Check the path and name."
            VIEW-AS ALERT-BOX.
            NEXT.
         END. 

         llOk = FALSE.
         MESSAGE "Do You want to start (Y/N) ?" UPDATE llOk.
         IF NOT llOk THEN NEXT action.

         LEAVE action.
      END.
   END. /* Action */      

   Message "Processing...".

   RUN intrumcr.p (OUTPUT TABLE ttError,
                   lcInfile,
                   OUTPUT liRead,
                   OUTPUT liFound,
                   OUTPUT liCount).

   /* move the file to processed */
   IF liFound > 0 AND lcArchive NE "" THEN 
   fTransDir(lcInfile,
             "",
             lcArchive). 

   /* print error list */
   IF CAN-FIND(FIRST ttError) THEN DO:

      ASSIGN tila = TRUE.
      {utuloste.i "return"}

      RUN intrumcrp (INPUT TABLE ttError,
                     lcInfile).

      ASSIGN tila = FALSE.
      {utuloste.i}

      MESSAGE "Error list was printed." 
      VIEW-AS ALERT-BOX. 

   END.    

   MESSAGE 
     liRead  "invoices were read from the file."           SKIP
     liFound "matching invoices were found from TMS"       SKIP
             "and their claiming was marked as cancelled." SKIP
     liCount "invoices were posted as credit loss."       
   VIEW-AS ALERT-BOX.   

   LEAVE main.

END. /* MAIN */

HIDE FRAME main NO-PAUSE.
HIDE MESSAGE.
