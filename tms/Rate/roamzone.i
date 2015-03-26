
DEFINE TEMP-TABLE ttRoamCC NO-UNDO
   FIELD Country  AS CHARACTER
   FIELD Prefix   AS CHARACTER
   FIELD RateZone AS INTEGER.

DEFINE TEMP-TABLE ttRateZone NO-UNDO
   FIELD RateZone  AS INTEGER
   FIELD ConnRate  AS DECIMAL
   FIELD FirstSec  AS INTEGER
   FIELD FirstRate AS DECIMAL
   FIELD ScndSec   AS INTEGER
   FIELD ScndRate  AS DECIMAL.

DEFINE VARIABLE lcTTLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE liTTLoop AS INTEGER   NO-UNDO.

DO liTTLoop = 0 TO 7:

   CREATE ttRateZone.
   ASSIGN
      ttRateZone.RateZone = liTTLoop
      ttRateZone.ConnRate = 0.12
      ttRateZone.FirstSec = 60
      ttRateZone.ScndSec  = 30.
   
   CASE liTTLoop:
      WHEN 1 THEN ttRateZone.FirstRate = 0.30.
      WHEN 2 THEN ttRateZone.FirstRate = 0.48.
      WHEN 3 THEN ttRateZone.FirstRate = 0.67.
      WHEN 4 THEN ttRateZone.FirstRate = 0.82.
      WHEN 5 THEN ttRateZone.FirstRate = 0.97.
      WHEN 6 THEN ttRateZone.FirstRate = 0.24.
      WHEN 7 THEN ttRateZone.FirstRate = 7.40.
   END.

   ttRateZone.ScndRate = ttRateZone.FirstRate / 2.

END.

INPUT FROM /apps/tms/Rate/roamcc.txt.

REPEAT:

   IMPORT UNFORMATTED lcTTLine.

   CREATE ttRoamCC.
   ttRoamCC.Country = lcTTLine.
   
   IMPORT UNFORMATTED lcTTLine.
   ttRoamCC.Prefix = lcTTLine.
   
   IMPORT UNFORMATTED lcTTLine.
   ttRoamCC.RateZone = INT(lcTTLine).

END.
