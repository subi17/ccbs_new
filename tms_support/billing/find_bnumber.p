DEF var /* INPUT PARAMETER */ idtDate    AS DATE NO-UNDO FORMAT "99-99-9999" INIT TODAY.
DEF var /* INPUT PARAMETER */ icBnumber  AS CHAR NO-UNDO FORMAT "X(12)" INIT "".
DEF VAR /* INPUT PARAMETER */ iiRateccn  AS INT  NO-UNDO.
DEF VAR                       bsuunta    LIKE Bdest.bdest NO-UNDO.
DEF VAR                       paytype    LIKE MobSub.PayType NO-UNDO.
DEF VAR                       rateid     LIKE Mobcdr.TariffNum NO-UNDO INIT 0.
DEF VAR                       llFound    AS LOG  NO-UNDO.

IF idtdate = TODAY THEN idtdate = TODAY - 200.
llFound = FALSE.

MAIN_LOOP:
DO WHILE TRUE
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

   IF RETRY THEN LEAVE.

   UPDATE 
   idtdate   LABEL "Alkaen päivästä"
   icBNumber LABEL "B-numero alkaen"
   iiRateCCN LABEL "CC"
   bsuunta   COLUMN-LABEL "B-suunta" SKIP
   paytype   LABEL "Paytype" 
   rateid    LABEL "Rate ID".

   DEF VAR lddate AS DATE NO-UNDO.

   DO lddate = idtdate TO TODAY:

      IF icbnumber NE ""  THEN DO:

         IF NOT paytype THEN
         FOR EACH Mobcdr WHERE 
                  Mobcdr.datest = lddate AND 
                  Mobcdr.gsmbnr BEGINS icbnumber NO-LOCK.

            IF rateid           NE 0 AND 
               Mobcdr.TariffNum NE rateid  THEN NEXT.

            IF bsuunta      NE "" AND 
               Mobcdr.Bdest NE bsuunta  THEN NEXT.
            
            IF iiRateccn = 0 OR 
               iiRateCCN = Mobcdr.RAteccn THEN DO:
               DISP datest STRING(Mobcdr.timest,"hh:mm:ss")
                  cli    FORMAT "x(10)"
                  gsmbnr FORMAT "x(10)"
                  spocmt FORMAT ">>9"
                  errorcode
                  Mobcdr.billdur
                  Mobcdr.Bdest.

               llFound = TRUE.
            END.
         END.
         ELSE 
         FOR EACH PrepCDR WHERE 
                  PrepCDR.datest = lddate AND 
                  PrepCDR.gsmbnr BEGINS icbnumber NO-LOCK.

            IF rateid            NE 0 AND 
               PrepCDR.TariffNum NE rateid  THEN NEXT.

            IF bsuunta       NE "" AND 
               PrepCDR.Bdest NE bsuunta  THEN NEXT.
            
            IF iiRateccn = 0 OR 
               iiRateCCN = PrepCDR.RAteccn THEN DO:
               DISP datest STRING(PrepCDR.timest,"hh:mm:ss")
                  cli    FORMAT "x(10)"
                  gsmbnr FORMAT "x(10)"
                  spocmt FORMAT ">>9"
                  errorcode
                  PrepCDR.billdur
                  PrepCDR.Bdest.

               llFound = TRUE.
            END.
         END.
      END.
      ELSE DO:
         IF NOT paytype THEN
            FOR EACH Mobcdr WHERE
                     Mobcdr.datest = lddate NO-LOCK.

               IF rateid           NE 0 AND 
                  Mobcdr.TariffNum NE rateid  THEN NEXT.

               IF bsuunta      NE "" AND
                  Mobcdr.Bdest NE bsuunta  THEN NEXT.

               IF iiRateccn = 0 OR
                  iiRateCCN = Mobcdr.RAteccn THEN DO:
                  DISP datest STRING(Mobcdr.timest,"hh:mm:ss")
                     cli    FORMAT "x(10)"
                     gsmbnr FORMAT "x(10)"
                     spocmt FORMAT ">>9" 
                     errorcode
                     Mobcdr.billdur
                     Mobcdr.Bdest.

                  llFound = TRUE.
               END.
            END.
         ELSE
            FOR EACH PrepCDR WHERE
                     PrepCDR.datest = lddate NO-LOCK.

            IF rateid            NE 0 AND 
               PrepCDR.TariffNum NE rateid  THEN NEXT.

               IF bsuunta      NE "" AND
                  PrepCDR.Bdest NE bsuunta  THEN NEXT.

               IF iiRateccn = 0 OR
                  iiRateCCN = PrepCDR.RAteccn THEN DO:
                  DISP datest STRING(PrepCDR.timest,"hh:mm:ss")
                     cli    FORMAT "x(10)"
                     gsmbnr FORMAT "x(10)"
                     spocmt FORMAT ">>9"
                     errorcode
                     PrepCDR.billdur
                     PrepCDR.Bdest.

                  llFound = TRUE.
               END.
            END.
      END.
   END.
   IF NOT llFound THEN MESSAGE "Nothing found" VIEW-AS ALERT-BOX.
UNDO.
END. /* DO WHILE TRUE: */
