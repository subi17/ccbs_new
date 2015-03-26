/* -----------------------------------------------------
  MODULE .......: cpbrandui.p
  FUNCTION .....: copy data to a new brand from an existing one (ui)
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 14.10.03
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF INPUT PARAMETER icNewBrand AS CHAR NO-UNDO.

DEF VAR lcSrcBrand AS CHAR NO-UNDO. 
DEF VAR llCopy     AS LOG  NO-UNDO.
DEF VAR lcNewBrand AS CHAR NO-UNDO. 
DEF VAR lcNewName  AS CHAR NO-UNDO. 

form
   SKIP(5)
   "Data will be copied from the source brand defined below to the"
        AT 10 SKIP
   "new target brand. Copying wizard guides You through the process."
        AT 10 
   SKIP(2)
   
   lcSrcBrand AT 10 
      LABEL "Source Brand" 
      HELP "Data will be copied from this brand"
   Brand.BRName
      NO-LABEL 
   SKIP
   
   icNewBrand AT 10
      LABEL "Target Brand"
      FORMAT "X(8)"
   lcNewName 
      NO-LABEL
      FORMAT "X(30)"
   
   SKIP(6)
with title color value(ctc) 
  " " + ynimi + " COPY A NEW BRAND  " + 
  STRING(pvm,"99-99-99")  + " "
  side-labels color value(cfc) 
  OVERLAY row 1 width 80 frame main.


FIND Brand WHERE Brand.Brand = icNewBrand NO-LOCK NO-ERROR.
IF NOT AVAILABLE Brand THEN DO:
   MESSAGE "Target brand" icNewBrand "is not available."
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.
IF CAN-FIND(FIRST TMSParam WHERE TMSParam.Brand = icNewBrand)
THEN DO:
   MESSAGE "Target brand" icNewBrand "is already in use." SKIP
           "Copying is not allowed anymore."
   VIEW-AS ALERT-BOX
   ERROR.
   RETURN.
END.

lcNewName = Brand.BRName.

cfc = "lis". RUN ufcolor.
ehto = 9. RUN ufkey.

PAUSE 0.
DISPLAY icNewBrand lcNewName WITH frame main. 

pause 0 no-MESSAGE.

LOOP:
repeat WITH FRAME main:

   REPEAT WITH FRAME main ON ENDKEY UNDO, LEAVE:
   
      ehto = 9. RUN ufkey.
      pause 0 no-MESSAGE.

      update
         lcSrcBrand
      with frame main editing:

         readkey. nap = keylabel(lastkey).

         IF lookup(nap,poisnap) > 0 THEN DO:

            IF FRAME-FIELD = "lcSrcBrand" THEN DO WITH FRAME main:

               IF lcSrcBrand = "" THEN DISPLAY "" @ Brand.BRName.
               
               ELSE DO:
                  FIND Brand WHERE Brand.Brand = INPUT lcSrcBrand
                  NO-LOCK NO-ERROR.
                  
                  IF NOT AVAILABLE Brand THEN DO:
                     MESSAGE "Unknown invoice".
                     BELL. NEXT.
                  END.

                  DISPLAY Brand.BRName.
               END.
            END.

         END.
            
         apply lastkey.

      END.
         
      LEAVE.
   END.

   DISPLAY lcSrcBrand WITH FRAME main.
   
   IF lcSrcBrand = "" THEN DISPLAY "" @ Brand.BrName.
   ELSE DO:
       FIND Brand WHERE Brand.Brand = lcSrcBrand NO-LOCK NO-ERROR.
       IF AVAILABLE Brand THEN DISPLAY Brand.BRName WITH FRAME main.
   END.
   
   toimi:
   repeat WITH FRAME main on ENDkey undo toimi, next toimi:

      ASSIGN
      ufk = 0 ufk[1] = 132 ufk[4] = 0  ufk[5] = 795 ufk[8] = 8 
      ehto = 0.
      RUN ufkey.

      IF toimi = 1 THEN next LOOP.

      else IF toimi = 8 THEN DO:
         llCopy = FALSE.
         leave LOOP.
      END.

      else IF toimi = 5 THEN DO:
         
         IF lcSrcBrand = "" THEN DO:
            MESSAGE "Source brand has not been selected."
            VIEW-AS ALERT-BOX
            ERROR.
            NEXT.
         END.
         
         llCopy = FALSE.
         MESSAGE "All basic data will be copied to brand" icNewBrand + "." SKIP
                 "Start copying ?"
         VIEW-AS ALERT-BOX
         QUESTION
         BUTTONS YES-NO
         TITLE " Copy Data "
         SET llCopy.
         
         IF llCopy THEN LEAVE loop.
      END.

   END.

END.

IF llCopy THEN DO:

   RUN cpbrand.p (lcSrcBrand,
                  icNewBrand,
                  OUTPUT llCopy).

   IF llCopy THEN 
   MESSAGE "Data was succesfully copied to brand" 
           icNewBrand "(" + lcNewName + ")." 
   VIEW-AS ALERT-BOX
   TITLE " Finished ".
      
   ELSE MESSAGE "Data copying was cancelled."
        VIEW-AS ALERT-BOX
        TITLE " Cancelled ".
                       
END.

HIDE MESSAGE NO-PAUSE.
HIDE FRAME main NO-PAUSE.

