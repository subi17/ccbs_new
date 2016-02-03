/* ------------------------------------------------------
  MODULE .......: ddstatrel.p
  FUNCTION .....: ui for dd status report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 01.11.04 
  MODIFIED .....: 
  VERSION ......: SL
  ------------------------------------------------------ */
{Syst/commali.i}

{Syst/utumaa.i "new"}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ddauth'}

ASSIGN tuni1 = "ddstatre"
       tuni2 = "".

DEF VAR ufkey         AS LOG                     NO-UNDO.
DEF VAR ok            AS LOG   FORMAT "Yes/No"   NO-UNDO.
                
DEF VAR ldtAuthDate1  AS DATE                    NO-UNDO.
DEF VAR ldtAuthDate2  AS DATE                    NO-UNDO.
DEF VAR lcCode        AS CHAR                    NO-UNDO. 
DEF VAR lcFrameField  AS CHAR                    NO-UNDO. 
DEF VAR lcFile        AS CHAR  FORMAT "X(50)"    NO-UNDO.
DEF VAR llListUnsent  AS LOG                     NO-UNDO. 

FORM 
   SKIP(3)
   "Direct debit invoices sent to customers and changes in" AT 10 SKIP
   "authorizations are reported from given period."         AT 10 SKIP
   SKIP(3)
   
   ldtAuthDate1  AT 10
        LABEL "DD Period ....."
        HELP "Period of DD events"
        FORMAT "99-99-9999"
   "-"
   ldtAuthDate2    
        NO-LABEL
        HELP "Period of DD events"
        VALIDATE (INPUT ldtAuthDate2 ge INPUT ldtAuthDate1,
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"
   SKIP
   
   llListUnsent AT 10
        LABEL "List Unsent ..."
        HELP "List invoices that have not been sent to bank"
        FORMAT "Yes/No"
   SKIP(1)   
        
   lcFile AT 10
        LABEL "Excel-file ...."
        HELP "If name is given, then a tab-separated file is made"
   SKIP(5)
   
   with row 1 side-labels width 80
        title " " + ynimi + " DD STATUS REPORT " +
        string(pvm,"99-99-99") + " "
        frame fCrit.


/* previous month as default */
ASSIGN ldtAuthDate2 = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtAuthDate1 = DATE(MONTH(ldtAuthDate2),1,YEAR(ldtAuthDate2))
       ufkey        = FALSE
       llListUnsent = FALSE.

toimi:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO toimi, NEXT toimi:

   DISPLAY 
      ldtAuthDate1 ldtAuthDate2
      lcFile
      llListUnsent
      WITH FRAME fCrit. 

   IF ufkey THEN DO:
      ASSIGN
         ufk[1] = 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5] = 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9] = 1
         ehto   = 3.
      RUN ufkey.p.
      READKEY.
      nap = KEYLABEL(LASTKEY).
   END.
   ELSE ASSIGN nap   = "1"
               ufkey = TRUE.

   IF LOOKUP(nap,"1,f1") > 0 THEN DO:
         
      ehto = 9. RUN ufkey.p.

      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:
      
         UPDATE ldtAuthDate1
                ldtAuthDate2
                llListUnsent
                lcFile
         WITH FRAME fCrit EDITING:
         
            READKEY.
            nap = KEYLABEL(LASTKEY).
         
            IF LOOKUP(nap,poisnap) > 0 THEN DO:

            END.
            
            APPLY LASTKEY.
             
         END. 
         
         LEAVE. 
      END.
         
      ufkey = TRUE.
      NEXT toimi.
         
   END.
      
   ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
      LEAVE toimi.
   END.

   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      RETURN.
   END.
      
END. /* toimi */

/* Avataan striimi */
IF lcFile = "" THEN DO:
   ASSIGN tila = TRUE.
   {Syst/utuloste.i "return"}
END.

MESSAGE "Printing in process".            

ehto = 5.
run ufkey.

run ddstatrep (ldtAuthDate1,
               ldtAuthDate2,
               llListUnsent,
               lcFile). 

IF lcFile = "" THEN DO:
   ASSIGN tila = FALSE.
   {Syst/utuloste.i}
END.

MESSAGE "DD status report is finished."
VIEW-AS ALERT-BOX
TITLE " Report done ".  

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    

