/* ------------------------------------------------------
  MODULE .......: pbankrel.p
  FUNCTION .....: ui for report of payments' banks 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 08.11.02/aam 
  MODIFIED .....: 12.09.03/aam brand
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Syst/utumaa.i "new"}

ASSIGN tuni1 = "pbankrel"
       tuni2 = "".

DEF VAR ufkey         AS LOG                     NO-UNDO.
DEF VAR ok            AS LOG   FORMAT "Yes/No"   NO-UNDO.

DEF VAR InvGroup       AS CHAR  FORMAT "X(10)"    NO-UNDO.
DEF VAR lcIgName      AS CHAR                    NO-UNDO. 
DEF VAR ldtPaid1      AS DATE                    NO-UNDO.
DEF VAR ldtPaid2      AS DATE                    NO-UNDO.
DEF VAR lcCode        AS CHAR                    NO-UNDO. 
DEF VAR lcFrameField  AS CHAR                    NO-UNDO. 
DEF VAR llMarkInv     AS LOG                     NO-UNDO. 
DEF VAR liCount       AS INT                     NO-UNDO. 
DEF VAR lcFile        AS CHAR                    NO-UNDO. 

DEF STREAM slog.

DEF TEMP-TABLE ttMark NO-UNDO
    FIELD InvNum AS INT.

FORM 
   SKIP(4)
   "This program prints out a report from payments' bank accounts."  
       AT 10 SKIP
   "Report is sorted by invoicing group, bank account and payment source."
       AT 10 SKIP
   SKIP(3)

   InvGroup AT 10
        LABEL "Invoicing group"
        HELP "Invoicing group, EMPTY = all"
      lcIgName
        NO-LABEL
        FORMAT "X(30)" 
   SKIP

   ldtPaid1  AT 10
        LABEL "Payment dates ."
        HELP "Payment dates from"
        FORMAT "99-99-9999"
   "-"
   ldtPaid2    
        NO-LABEL
        HELP "Payment dates to"
        VALIDATE (INPUT ldtPaid2 ge INPUT ldtPaid1,
                  "Upper limit must be at least equal to lower limit")
        FORMAT "99-99-9999"

   SKIP(6)

   with row 1 side-labels width 80
        title " " + ynimi + " PAYMENTS' BANK ACCOUNT REPORT " +
        string(pvm,"99-99-99") + " "
        frame fCrit.


       /* previous month as default */
ASSIGN ldtPaid2      = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1
       ldtPaid1      = DATE(MONTH(ldtPaid2),1,YEAR(ldtPaid2)).

display ldtPaid1 ldtPaid2 
        with frame fCrit. 

ASSIGN ufkey = false
       nap   = "first". 

toimi:
REPEAT WITH FRAME fCrit ON ENDKEY UNDO toimi, NEXT toimi:

   IF ufkey THEN DO:
      ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 ufkey = FALSE.
      RUN ufkey.p.
   END.

   IF nap NE "first" THEN DO:
      READKEY.
      ASSIGN
      nap = KEYLABEL(LASTKEY).
   END.
   ELSE ASSIGN nap = "1". 

   IF LOOKUP(nap,"1,f1") > 0 THEN DO:

      ehto = 9. RUN ufkey.p.
      REPEAT WITH FRAME fCrit ON ENDKEY UNDO, LEAVE:
         UPDATE InvGroup
                ldtPaid1
                ldtPaid2
         WITH FRAME fCrit EDITING:

            READKEY.
            nap = KEYLABEL(LASTKEY).

            IF LOOKUP(nap,poisnap) > 0 THEN DO:

               IF FRAME-FIELD = "InvGroup" THEN DO:

                  IF INPUT InvGroup = "" THEN lcIgName = "ALL".
                  ELSE DO:
                     FIND InvGroup WHERE 
                          InvGroup.Brand    = gcBrand AND
                          InvGroup.InvGroup = INPUT InvGroup
                        NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE InvGroup THEN DO:
                        MESSAGE "Unknown invoicing group."
                        VIEW-AS ALERT-BOX.
                        NEXT.
                     END.
                     ASSIGN lcIgName = InvGroup.igname.
                  END.

                  DISPLAY lcIgName. 

               END.

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
ASSIGN tila = TRUE.
{Syst/utuloste.i "return"}

MESSAGE "Printing in process".            

run pbankrep (InvGroup,
              ldtPaid1, 
              ldtPaid2).

ASSIGN tila = FALSE.
{Syst/utuloste.i}

MESSAGE "Payments' bank account report is finished."
VIEW-AS ALERT-BOX
TITLE " Report done ".  

HIDE MESSAGE NO-PAUSE.
HIDE FRAME fCrit NO-PAUSE.    
