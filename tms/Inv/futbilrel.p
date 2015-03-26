 /* ------------------------------------------------------
  MODULE .......: futbilrel.p
  FUNCTION .....: ui for report of future billing
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 12.02.03/aam 
  MODIFIED .....: 08.05.03/aam optionally leave out banned InvGroups
                  22.09.03/aam brand
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF VAR lcUfkey     AS LOG                     NO-UNDO.
DEF VAR lcFile      AS CHAR  FORMAT "X(40)"    NO-UNDO.
DEF VAR InvGroup     AS CHAR  FORMAT "X(8)"     NO-UNDO EXTENT 2.
DEF VAR llContract  AS LOGIC FORMAT "Yes/No"   NO-UNDO.
DEF VAR llSingle    LIKE llContract.
DEF VAR llMobCall   LIKE llContract.
DEF VAR llFixCall   LIKE llContract. 
DEF VAR liCount     AS INT                     NO-UNDO. 
DEF VAR llBanned    AS LOGIC FORMAT "Yes/No"   NO-UNDO. 

FORM 
   SKIP(1)
   "This program prints out a report of future billing" AT 10 SKIP
   "for 12 months from today onwards." AT 10 
   SKIP(2)
   llContract AT 10 
        LABEL "Contracts ..."
        HELP  "Take unbilled contract fees"
        SKIP
   llSingle   AT 10
        LABEL "Single fees ."
        HELP  "Take unbilled single fees"
        SKIP
   llMobCall  AT 10
        LABEL "Mobile calls "
        HELP  "Take unbilled mobile calls"
        SKIP
   llFixCall  AT 10
        LABEL "Fixed calls ."
        HELP "Take unbilled fixed calls"
   SKIP(1)

   InvGroup[1] AT 10
        LABEL "Inv. groups ."
        HELP "Invoicing group"
   "-" 
   InvGroup[2] 
        NO-LABEL
        HELP "Invoicing group"
        SPACE(2)
   SKIP

   llBanned AT 10
        LABEL "Banned groups"
        HELP "Include groups that are banned and/or not invoiced"
   SKIP(1)

   lcFile AT 10
        LABEL "Output file ."
        HELP "Name of output file" 
        SKIP(3)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " FUTURE BILLING " +
        STRING(pvm,"99-99-99") + " "
        FRAME valinta.

VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

FIND LAST InvGroup WHERE
          InvGroup.InvGroup = gcBrand
NO-LOCK NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN InvGroup[2] = InvGroup.InvGroup.

FIND TMSUser WHERE TMSUser.UserCode = katun NO-LOCK NO-ERROR.
lcFile = (IF AVAILABLE TMSUser AND TMSUser.RepDir NE ""
          THEN TMSUser.RepDir 
          ELSE "/tmp") +
          "/future_bill.txt".

ASSIGN llContract = TRUE
       llSingle   = TRUE
       llMobCall  = TRUE
       llFixCall  = TRUE
       llBanned   = TRUE.

DISPLAY InvGroup 
        lcFile
        llContract
        llSingle
        llMobCall
        llFixCall
        llBanned
        WITH FRAME valinta. 

ASSIGN lcUfkey = FALSE
       nap     = "first". 

toimi:
REPEAT WITH FRAME valinta on ENDkey undo toimi, NEXT toimi:

   if lcUfkey THEN DO:

      ASSIGN
         ufk[1]= 132 
         ufk[2]= 0  ufk[3]= 0 ufk[4]= 0 /* 847 */
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 
         lcUfkey = FALSE.

      RUN ufkey.p.

   END.

   IF nap NE "first" THEN DO:
      READKEY.
      nap = KEYLABEL(LASTKEY).
   END.
   ELSE ASSIGN nap = "1". 

   IF LOOKUP(nap,"1,f1") > 0 THEN DO:

      ehto = 9. 
      RUN ufkey.p.

      REPEAT WITH FRAME valinta ON ENDKEY UNDO, LEAVE:

         UPDATE llContract
                llSingle
                llMobCall
                llFixCall
                InvGroup[1]
                InvGroup[2] 
                    validate(input InvGroup[2] >= input InvGroup[1],
                             "Invalid choice")
                llBanned              
                lcFile
         WITH FRAME valinta EDITING:

            READKEY.

            IF LOOKUP(keylabel(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME valinta:

               HIDE MESSAGE.

            END.

            APPLY LASTKEY.

         END.

         LEAVE. 

      END.

      lcUfkey = TRUE.

      NEXT toimi.
   END.

   ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
      LEAVE toimi.
   END.

   ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
      RETURN.
   END.

END. /* toimi */

ehto = 5.
RUN ufkey.

RUN futbilrep  (InvGroup[1],
                InvGroup[2],
                llBanned,
                llContract,
                llSingle,
                llMobCall,
                llFixCall,
                lcFile,
                OUTPUT liCount).

MESSAGE "Future billing -report was printed from unbilled events of" 
        liCount "customers."
VIEW-AS ALERT-BOX
INFORMATION.

HIDE MESSAGE NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.    
