/* -----------------------------------------------------
  MODULE .......: RepText.p
  FUNCTION .....: Updating Invoice Language 
  APPLICATION ..: 
  AUTHOR .......: lp
  CREATED ......: 02.01.02
  MODIFIED .....: 04.01.02/aam remove Language, 
                               when adding consider the given TextType,
                               update only text for old records,
                               check also recid when validating new record
                  17.10.02 kl F1 & F2 + display order
                  12.12.02 jr Eventlog
                  15.01.03 ??? more information for frame lis 
                  18.03.03 tk  tokens
                  16.09.03/aam brand
                  21.11.06/aam type from TMSCodes,
                               input parameters,
                               local-procedures etc. 
  VERSION ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable RepText

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'reptext'}

DEF INPUT PARAMETER iiTextType AS INT  NO-UNDO.
DEF INPUT PARAMETER icKeyValue AS CHAR NO-UNDO.

DEF VAR liType     AS INT                 NO-UNDO.
DEF VAR ok         AS LOG FORMAT "Yes/No" NO-UNDO.
DEF VAR haku1      LIKE RepText.LinkNum   NO-UNDO.
DEF VAR haku2      LIKE RepText.LinkCode  NO-UNDO.
DEF VAR firstline  AS INT                 NO-UNDO.
DEF VAR FrmRow     AS INT                 NO-UNDO INIT 1.
DEF VAR FrmDown    AS INT                 NO-UNDO INIT 15.
DEF VAR order      AS INT                 NO-UNDO INIT 1.
DEF VAR ex-order   AS INT                 NO-UNDO.
DEF VAR memory     AS RECID               NO-UNDO.

DEF VAR lcname     AS CHAR                NO-undo.
DEF VAR itName     AS CHAR                NO-UNDO.

DEF VAR line       AS INT FORMAT "99"     NO-UNDO.
DEF VAR delline    AS INT                 NO-UNDO.
DEF VAR must-print AS LOG                 NO-UNDO.
DEF VAR must-add   AS LOG                 NO-UNDO.
DEF VAR ufkey      AS LOG                 NO-UNDO.
DEF VAR fr-header  AS CHAR                NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24     NO-UNDO.
DEF VAR i          AS INT                 NO-UNDO.
DEF VAR xrecid     AS RECID.
DEF VAR liLength   AS INT                 NO-UNDO. 

DEF BUFFER blang FOR RepText. 

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRepText AS HANDLE NO-UNDO.
   lhRepText = BUFFER RepText:HANDLE.
   RUN StarEventInitialize(lhRepText).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhRepText).
   END.
END.


FORM 
   RepText.TextType        COLUMN-LABEL "Type"  
      HELP "Item type"
   itname                  COLUMN-LABEL "Type Desc." FORMAT "X(12)"
   RepText.LinkCode        COLUMN-LABEL "Key Value" FORMAT "x(16)"  
   RepText.Language        COLUMN-LABEL "Lang" 
   RepText.ToDate          FORMAT "99-99-99" COLUMN-LABEL "Valid To"
   RepText.RepText         FORMAT "x(29)"
WITH WIDTH 80 ROW FrmRow OVERLAY SCROLL 1 FrmDown DOWN COLOR VALUE(cfc)
   TITLE COLOR VALUE(ctc) " " + ynimi +
      "  TRANSLATIONS  " + STRING(pvm,"99-99-99") + " "     
   FRAME sel.

FORM
   RepText.TextType AT 2   
      LABEL "Item Type "
      HELP "Item type"
   itname 
      NO-LABEL       
      FORMAT "X(30)" SKIP
   RepText.LinkCode AT 2
      LABEL "Key Value "
      FORMAT "X(40)" SKIP(1)
   RepText.Language AT 2  
      LABEL "Language ."
   lcname 
      NO-LABEL SKIP
   RepText.FromDate AT 2 
      LABEL "Valid From" SKIP
   RepText.ToDate  AT 2
      LABEL "Valid To ." 
      "L:" AT 71
      liLength AT 73 NO-LABEL FORMAT ">>>>>9" SKIP(1)
   "Text:" AT 2 SKIP
   RepText.RepText AT 3 
      NO-LABEL
      VIEW-AS EDITOR SIZE 75 BY 3
WITH OVERLAY ROW 7 CENTERED COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   fr-header WITH SIDE-LABELS FRAME lis.

{Func/brand.i}

FORM /* hakua2 varten */
   "Brand ...:" lcBrand skip
   "Type ....:" liType FORMAT ">>9" SKIP
   "Key Value:" haku2  FORMAT "X(16)" 
      HELP "Key value for type"
   WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND KEY VALUE "
   COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME hayr1.

FUNCTION fCheckExisting RETURNS LOGICAL:

   IF CAN-FIND (FIRST blang WHERE
                      bLang.Brand    = lcBrand AND
                      blang.TextType = RepText.TextType AND
                      blang.Language = RepText.Language AND
                      blang.LinkCode = RepText.LinkCode AND
                      blang.ToDate  >= RepText.FromDate AND
                      blang.FromDate <= RepText.ToDate  AND
                      RECID(blang) NE RECID(RepText))
   THEN DO:
      MESSAGE 
         "Item type"      STRING(RepText.TextType) SKIP
         "with key value" STRING(RepText.LinkCode) SKIP
         "and language"   STRING(RepText.Language) SKIP
         "on period"      STRING(RepText.FromDate,"99-99-99") "-" 
                          STRING(RepText.ToDate,"99-99-99") SKIP
         "already exists" 
      VIEW-AS ALERT-BOX INFORMATION.
      RETURN TRUE.
   END.
      
   RETURN FALSE.
      
END FUNCTION.

IF icKeyValue > "" THEN ASSIGN
   FrmRow  = 4
   FrmDown = 8.

cfc = "sel". 
RUN Syst/ufcolor. 
ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE RepText THEN 
   ASSIGN 
   memory     = RECID(RepText)
   must-print = true 
   must-add   = false.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No report texts available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN 
      memory     = ? 
      must-print = FALSE 
      must-add   = FALSE.
END.

ASSIGN 
   xrecid    = ? 
   delline   = 0 
   ufkey     = true 
   order     = 1 
   firstline = 0.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> ex-order THEN DO:
      ex-order = order.
   END.

   IF must-add THEN DO:  /* RepText -ADD  */
      ASSIGN 
         cfc       = "lis" 
         ufkey     = true 
         fr-header = " ADD " 
         must-add  = false.
      RUN Syst/ufcolor.

      ADD-NEW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-NEW, LEAVE ADD-NEW.
         PAUSE 0 NO-MESSAGE.
         CLEAR FRAME lis NO-PAUSE.
         ehto = 9. RUN Syst/ufkey.
         DO TRANSACTION:
                               
            IF iiTextType > 0 THEN DISPLAY iiTextType @ RepText.TextType.
            ELSE DO:
               PROMPT-FOR RepText.TextType . 
               IF INPUT RepText.TextType = "" THEN LEAVE ADD-NEW.
            END.

            CREATE RepText.

            ASSIGN
            RepText.Brand     = lcBrand
            RepText.TextType  = INPUT FRAME lis RepText.TextType. 

            RUN local-find-others.
            DISPLAY itname WITH FRAME lis.
            
            IF icKeyValue > "" THEN RepText.LinkCode = icKeyValue.    
            DISPLAY RepText.LinkCode.
            
            UPDATE
               RepText.LinkCode WHEN icKeyValue = ""
               RepText.Language
               RepText.FromDate
               RepText.ToDate
               RepText.RepText.


            IF RepText.Language = 0 OR
               RepText.LinkCode = "" 
            THEN DO:
               DELETE RepText.
               LEAVE ADD-NEW.
            END.

            IF fCheckExisting() THEN DO:
               DELETE RepText.
               LEAVE add-new.
            END.

            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRepText).

            ASSIGN
            memory = RECID(RepText)
            xrecid = memory.
         END.
      END.  /* ADD-NEW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* any records available ? */
      RUN local-find-first.
      IF NOT AVAILABLE RepText THEN LEAVE LOOP.
      
      NEXT LOOP.

   END. /* if must-add */

   print-line:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND RepText WHERE RECID(RepText) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose keyvalue = memory
         beginning from line 'delline' */

         /* if a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE RepText THEN DO:
               RUN local-disp-row.

               rtab[FRAME-LINE] = RECID(RepText).
               RUN local-find-next.
            END.
            ELSE DO:
               CLEAR NO-PAUSE.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         UP FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = false.
         PAUSE 0 NO-MESSAGE.

         /* one page of data has been printed and
         the cursor is in the UPmost line for 'choose' */
      END. /* must-print = true */
   END. /* print-line */

   /* if lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:
   
      IF ufkey THEN DO:
         ASSIGN
         ufk[1] = 35 ufk[2]  = 0 ufk[3] = 0 ufk[4] = 0
         ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0) 
         ufk[7] = 0 ufk[8] = 8 ufk[9]= 1
         ehto = 3 ufkey = false.
         RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW RepText.TextType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) RepText.TextType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 2 THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 1.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND RepText WHERE RECID(RepText) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE RepText THEN
               ASSIGN firstline = i memory = RECID(RepText).
            ELSE LEAVE.
         END.
         must-print = true.
         NEXT LOOP.
      END.

      /* previous line */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND RepText WHERE RECID(RepText) = rtab[1] NO-LOCK.
            RUN local-find-prev.
            IF NOT AVAILABLE RepText THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               SCROLL DOWN.
               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 by -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = RECID(RepText)
               memory = rtab[1].
            END.
         END.
         ELSE UP 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF LOOKUP(nap,"cursor-DOWN") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND RepText WHERE RECID(RepText) = rtab[FRAME-DOWN] NO-LOCK .
            RUN local-find-next.

            IF NOT AVAILABLE RepText THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 NO-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               SCROLL UP.

               RUN local-disp-row.
               
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = RECID(RepText).
               /* finally last line's keyvalue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND RepText WHERE RECID(RepText) = memory NO-LOCK NO-ERROR.
         
         RUN local-find-prev.

         IF AVAILABLE RepText THEN DO:
            memory = RECID(RepText).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE RepText THEN memory = RECID(RepText).
               ELSE line = FRAME-DOWN.
            END.
            must-print = true.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the first data page */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
         END.
      END. /* previous page */

      /* NEXT page */
      ELSE IF LOOKUP(nap,"NEXT-page,page-DOWN,+") > 0 THEN DO WITH FRAME sel:
         /* cursor TO the DOWNmost line */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 NO-MESSAGE.
         END.
         ELSE DO: /* the DOWNmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND RepText WHERE RECID(RepText) = memory NO-LOCK.
            must-print = true.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* Haku sarakk. 1 */
      IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN DO:  
         cfc = "puyr". RUN Syst/ufcolor.
         haku2 = "".
         ehto = 9. RUN Syst/ufkey. ufkey = true.
         DISPLAY lcBrand WITH FRAME hayr1.
         UPDATE lcBrand WHEN gcAllBrand
                liType
                haku2 WITH FRAME hayr1.

         HIDE FRAME hayr1 NO-PAUSE.

         IF liType > 0  THEN DO:
            FIND FIRST RepText WHERE 
                       RepText.Brand    = lcBrand AND
                       RepText.TextType = liType  AND
                       RepText.LinkCode >= haku2 
            USE-INDEX LinkCode NO-LOCK NO-ERROR.     

            IF NOT fRecFound(1) THEN NEXT BROWSE.

            NEXT LOOP.
         END.
      END. /* Haku sar. 1 */

      ELSE IF  LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
         must-add = true.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
      THEN DO TRANSACTION:  /* removal */
         delline = FRAME-LINE.
         FIND RepText WHERE RECID(RepText) = rtab[FRAME-LINE] NO-LOCK.

         /* line to be deleted is lightened */
         COLOR DISPLAY VALUE(ctc)
            RepText.TextType
            RepText.Language
            RepText.RepText
            RepText.LinkCode.
            
         RUN local-find-next.   

         IF AVAILABLE RepText THEN memory = RECID(RepText).
         ELSE DO:
            /* the one to be deleted is rereaden */
            FIND RepText WHERE RECID(RepText) = rtab[FRAME-LINE] NO-LOCK.
            /* and THEN the previous one */
            RUN local-find-prev.

            IF AVAILABLE RepText THEN DO:
               ASSIGN
               delline = delline - 1  /* cause the last one is to be deleted */
               memory = RECID(RepText).
            END.
         END.

         /* 'FIND' back to the row to be deleted */
         FIND RepText WHERE RECID(RepText) = rtab[FRAME-LINE]
         EXCLUSIVE-LOCK.

         ASSIGN ok = false.
         MESSAGE " ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
         COLOR DISPLAY VALUE(ccc)
            RepText.TextType
            RepText.Language
            RepText.RepText
            RepText.LinkCode.
         IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRepText).
            DELETE RepText.

            /* in the last record was deleted ? */
            RUN local-find-first.
            IF NOT AVAILABLE RepText THEN DO:
               CLEAR FRAME sel NO-PAUSE.
               PAUSE 0 NO-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = true.
            NEXT LOOP.
         END.
         ELSE delline = 0. /* wasn't the last one */
      END. /* removal */

      ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
      DO WITH FRAME lis TRANSACTION:
         /* change */
         FIND RepText WHERE RECID(RepText) = rtab[FRAME-LINE(sel)]
         NO-LOCK.

         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRepText).
         
         ASSIGN  
            fr-header = " CHANGE " 
            ufkey = true
            liLength = LENGTH(RepText.RepText).
            cfc = "lis". RUN Syst/ufcolor.   

       
       FIND FIRST language WHERE
                    language.Language = RepText.Language NO-LOCK NO-ERROR.
         IF avail language then lcname = language.langname.
         ELSE                   lcname = "Unknown".           


         RUN local-find-others.

         DISP  RepText.TextType
               itname
               RepText.Language
               lcname
               RepText.LinkCode
               RepText.FromDate
               RepText.ToDate
               RepText.RepText
               liLength
               WITH FRAME lis.
               
         ASSIGN
            ufk = 0
            ufk[1] = 7
            ufk[8] = 8
            ehto = 0.

         RUN Syst/ufkey.
         
         IF toimi = 1 THEN DO:
         
            FIND CURRENT RepText EXCLUSIVE-LOCK.

            REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
            
               ehto = 9.
               RUN Syst/ufkey.
            
               UPDATE 
                  RepText.FromDate
                  RepText.ToDate
                  RepText.RepText.

               /* IF  User Wanted TO Cancel this Change TRANSACTION */
               IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
               KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
               IF fCheckExisting() THEN NEXT.

               /* Verify notification of the SMS length. */
               IF RepText.TextType = 32 AND 
                  LENGTH(INPUT FRAME lis RepText.RepText) > 160 THEN DO:
                  ok = FALSE.
                  MESSAGE "Length of text exceeds 160 characters." SKIP
                          "Do You still want to save this text ?"
                  VIEW-AS ALERT-BOX QUESTION
                  BUTTONS YES-NO
                  SET ok.
                  IF NOT ok THEN UNDO, LEAVE.
               END. /* IF RepText.TextType = 32 AND  */
               LEAVE.
            END.   
         END.
         
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRepText).
         
         HIDE FRAME lis NO-PAUSE. 
         
         RUN local-disp-row.   

         xrecid = RECID(RepText).    
      END.

      ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
         RUN local-find-first.
         
         ASSIGN memory = RECID(RepText) must-print = true.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* last record */
         RUN local-find-last.
         
         ASSIGN memory = RECID(RepText) must-print = true.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

   END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

PROCEDURE local-find-others:

   itname = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                             "RepText",
                             "TextType",
                             RepText.TextType).

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.
   
   DISPLAY
      RepText.TextType
      itname
      RepText.Language
      RepText.RepText
      RepText.LinkCode
      RepText.ToDate
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icKeyValue > "" THEN DO:
      FIND FIRST RepText WHERE
                 RepText.Brand    = gcBrand    AND
                 RepText.TextType = iiTextType AND
                 RepText.LinkCode = icKeyValue NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      IF order = 1 THEN 
      FIND FIRST RepText WHERE RepText.Brand = gcBrand
         USE-INDEX LinkCode NO-LOCK NO-ERROR.
   END.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF icKeyValue > "" THEN DO:
      FIND LAST RepText WHERE
                RepText.Brand    = gcBrand    AND
                RepText.TextType = iiTextType AND
                RepText.LinkCode = icKeyValue NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      FIND LAST RepText WHERE RepText.Brand = gcBrand
         USE-INDEX LinkCode NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF icKeyValue > "" THEN DO:
      FIND NEXT RepText WHERE
                RepText.Brand    = gcBrand    AND
                RepText.TextType = iiTextType AND
                RepText.LinkCode = icKeyValue NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      FIND NEXT RepText WHERE RepText.Brand = gcBrand
         USE-INDEX LinkCode NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
   IF icKeyValue > "" THEN DO:
      FIND PREV RepText WHERE
                RepText.Brand    = gcBrand    AND
                RepText.TextType = iiTextType AND
                RepText.LinkCode = icKeyValue NO-LOCK NO-ERROR.
   END.
   
   ELSE DO:
      FIND PREV RepText WHERE RepText.Brand = gcBrand
         USE-INDEX LinkCode NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

