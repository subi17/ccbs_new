DEF var /* INPUT PARAMETER */ idtDate    AS DATE No-UNDO FORMAT "99-99-9999".
DEF var /*INPUT PARAMETER */ icBnumber   AS CHAR NO-UNDO FORMAT "X(12)" .
DEF VAR /* INPUT PARAMETER */ iiRateccn  AS INT  NO-UNDO .
DEF VAR                       bsuunta    like Bdest.bdest NO-UNDO.
DEF VAR                       paytype    like MobSub.PayType NO-UNDO.

idtdate = today - 200.
icbnumber = "".

UPDATE 
idtdate   label "Alkaen päivästä"
icBNumber label "B-numero alkaen"
iiRateCCN label "CC"
bsuunta   column-label "B-suunta"
paytype  label "Paytype".

def var lddate as date no-undo.

do lddate = idtdate to today:

   IF icbnumber ne ""  THEN DO:

      IF not paytype THEN
      for each mobcdr WHERE 
               MObcdr.datest = lddate AND 
               mobcdr.gsmbnr begins icbnumber no-lock.

         IF bsuunta      ne "" AND 
            Mobcdr.Bdest ne bsuunta  THEN NEXT.
         
         IF iiRateccn = 0 OR 
            iiRateCCN = mobcdr.RAteccn THEN 
         disp datest string(mobcdr.timest,"hh:mm:ss")
            cli    format "x(12)"
            gsmbnr
            spocmt errorcode
            mobcdr.billdur.
      
      ENd.
      ELSE 
      for each PrepCDR WHERE 
               PrepCDR.datest = lddate AND 
               PrepCDR.gsmbnr begins icbnumber no-lock.

         IF bsuunta      ne "" AND 
            PrepCDR.Bdest ne bsuunta  THEN NEXT.
         
         IF iiRateccn = 0 OR 
            iiRateCCN = PrepCDR.RAteccn THEN 
         disp datest string(PrepCDR.timest,"hh:mm:ss")
            cli    format "x(12)"
            gsmbnr
            spocmt errorcode
            PrepCDR.billdur.
      
      ENd.
   END.
   ELSE DO:
      IF not paytype THEN
         for each mobcdr WHERE
                  MObcdr.datest = lddate no-lock.
                                    
            IF bsuunta      ne "" AND
               Mobcdr.Bdest ne bsuunta  THEN NEXT.

            IF iiRateccn = 0 OR
               iiRateCCN = mobcdr.RAteccn THEN
            disp datest string(mobcdr.timest,"hh:mm:ss")
               cli    format "x(12)"
               gsmbnr
               spocmt errorcode
               mobcdr.billdur.
         END.
      ELSE
         for each prepcdr WHERE
                  prepcdr.datest = lddate no-lock.
                                    
            IF bsuunta      ne "" AND
               prepcdr.Bdest ne bsuunta  THEN NEXT.

            IF iiRateccn = 0 OR
               iiRateCCN = prepcdr.RAteccn THEN
            disp datest string(prepcdr.timest,"hh:mm:ss")
               cli    format "x(12)"
               gsmbnr
               spocmt errorcode
               prepcdr.billdur.
         END.
   END.
ENd.
