/* ---------------------------------------------------------------------------
  MODULE .......: NNATMU.P
  TEHTAVA ......: A-tilaajanumeroiden muodostaminen
  APPLICATION ..: NN
  TEKIJA .......: PT
  CREATED ......: 29-08-96
  CHANGE .......: 23.03.97 pt lisatty kutsunumero tietueelle
                  14.07.97 tt Ruotsiin laitettu nama ominaisuudet, ruotsiksi
                  11.05.00 kl at last in english
                  26.06.02 lp new INPUT PARAMETER ->connected day to CLI
                              if tila = 1 -> check clStamp 
                              if tila = 3 -> clouse (clStamp = today)
                  04.09.02 lp clStamp from DefClStamp
                  24.10.02 lp eventlogging added
                  20.01.02 kl prevent wrong order in timestamps
                  27.03.03 kl M1.0
                  28.05.03 tk Close numbers with SerNum

  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}

/*
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCLI AS HANDLE NO-UNDO.
   lhCLI = BUFFER CLI:HANDLE.
   RUN StarEventInitialize(lhCLI).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCLI).
   END.

END.
*/

DEF INPUT  PARAMETER ipSerNum LIKE CLI.SerNum NO-UNDO.
DEF INPUT  PARAMETER n1       LIKE CLI.CLI    NO-UNDO.
DEF INPUT  PARAMETER n2       LIKE CLI.CLI    NO-UNDO.
DEF INPUT  PARAMETER lCRStamp AS DEC          NO-UNDO.
DEF INPUT  PARAMETER lCLStamp AS DEC          NO-UNDO.
DEF INPUT  PARAMETER asno     AS INT          NO-UNDO.
DEF INPUT  PARAMETER bilt     AS INT          NO-UNDO.
DEF INPUT  PARAMETER tila     AS INT          NO-UNDO.
DEF OUTPUT PARAMETER rc       AS INT          NO-UNDO.
DEF OUTPUT PARAMETER expl     AS CHAR         NO-UNDO.

DEF VAR x1        AS DEC  NO-UNDO.
DEF VAR x2        AS DEC  NO-UNDO.
DEF VAR x         AS DEC  NO-UNDO.
DEF VAR cx        AS CHAR NO-UNDO.
DEF VAR i         AS INT  NO-UNDO.
DEF VAR lPrevTime AS INT  NO-UNDO.
DEF VAR lPrevDate AS DA   NO-UNDO.

ASSIGN
   expl = ""
   rc   = 0. 

runko:
repeat:  

   /* 1st: format of n1 & n2 */
   IF LENGTH(n1) = 0 THEN DO:
      ASSIGN
         expl = "1. number is missing"
         rc   = -1.
      LEAVE runko.
   END.

   /* no characters */
   DO i = 1 to LENGTH(n1).
      IF INDEX("0123456789",SUBSTR(n1,i,1)) = 0 THEN DO:
         ASSIGN
            expl = STRING(i) + ". digit in 1. number is not allowed"
            rc   = -1.
         LEAVE runko.
      END.
   END.

   IF LENGTH(n2) = 0 THEN DO:
      ASSIGN
         expl = "2. number is missing"
         rc   = -2.
      LEAVE runko.
   END.

   /* n2:n numeerisuus */
   DO i = 1 TO LENGTH(n2).
      IF INDEX("0123456789",SUBSTR(n2,i,1)) = 0 THEN DO:
         ASSIGN
            expl = STRING(i) + ". digit in 2. number is not allowed"
            rc   = -2.
         LEAVE runko.
      END.
   END.

   IF LENGTH(n1) NE LENGTH(n2) THEN DO:
      ASSIGN
         expl = "Numbers length differs !"
         rc   = -3.
      LEAVE runko.
   END.

   /* OK at this point */
   ASSIGN
     x1 = decimal(n1)
     x2 = decimal(n2).

   IF tila = 1 OR tila = 2 THEN DO:
      /* create number series */
      DO x = x1 to x2 trans.

         cx = STRING(x,FILL("9",LENGTH(n1))).

         IF tila = 1 THEN DO:  /* check */
            FIND FIRST CLI WHERE 
                       CLI.CLI     = cx AND
                       CLI.clStamp > lCRStamp
            NO-LOCK NO-ERROR.
            IF AVAIL CLI THEN DO:
               rc = rc + 1.
               IF rc = 1 THEN
                  expl = "CLI " + cx + " exists for customer " + 
                         STRING(CLI.CustNum).
            END.
         END.

         ELSE IF tila = 2 THEN DO: /* create */

            FIND FIRST CLI WHERE
                       CLI.CLI      = cx AND
                       CLI.clStamp >= lCRStamp
            EXCLUSIVE-LOCK NO-ERROR.

            IF AVAIL CLI THEN DO:

               /*
               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCLI).
               */

               fSplitTS(lCRStamp, OUTPUT lPrevDate, OUTPUT lPrevTime).

               IF lPrevTime = 0 THEN ASSIGN
                  lPrevDate = lPrevDate - 1
                  lPrevTime = 86399.
               ELSE ASSIGN 
                  lPrevTime = lPrevTime - 1.

               CLI.clStamp = fHMS2TS(lPrevDate,string(lPrevTime,"hh:mm:ss")).

               IF CLI.crStamp > CLI.clStamp THEN
                  CLI.crStamp = CLI.clStamp.

               /*
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCLI).
               */

            END.

            CREATE CLI.
            ASSIGN
               rc             = rc + 1
               CLI.SerNum     = ipSerNum
               CLI.BillTarget = bilt
               CLI.CustNum    = asno
               CLI.CLI        = cx
               CLI.crStamp    = lCRStamp
               CLI.clStamp    = lCLStamp. 

            /*
            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCLI).    
            */

         END.

      END.
   END.
   ELSE IF tila = 3 THEN DO: /* close */

      FOR EACH CLI EXCLUSIVE-LOCK WHERE 
               CLI.CustNum  = asno     AND 
               CLI.SerNum   = ipSerNum AND
               CLI.clStamp >= lCLStamp:

         /*
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCLI).
         */

         ASSIGN 
            CLI.clStamp = lCLStamp
            rc          = rc + 1.

         /*
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCLI).
         */

      END.

   END.

   ELSE IF tila = 4 THEN DO: /* delete */

      FOR EACH CLI EXCLUSIVE-LOCK WHERE 
               CLI.SerNum = ipSerNum:

         /*
         IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCLI).
         */

         DELETE CLI.

         rc = rc + 1.

      END.

   END.

   /* final explanation */
   IF tila = 1 THEN DO:
      IF      rc > 1 THEN expl = STRING(rc)  + " Overlapping number ".
      ELSE IF rc = 0 THEN expl = "OK, no doubles ".
   END.
   ELSE IF tila = 2 THEN expl = STRING(rc) + " numbers created".
   ELSE IF tila = 3 THEN expl = STRING(rc) + " numbers closed".
   ELSE IF tila = 4 THEN expl = STRING(rc) + " numbers deleted".

   LEAVE runko.

END. /* runko */

