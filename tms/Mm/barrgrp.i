/* barrgrp.i            2004/jp
  
   changes:             22.06.06/aam DL8
*/

DEF VAR lcDL8 AS CHAR NO-UNDO INIT "PES,SMP".

DEF TEMP-TABLE ttBarring NO-UNDO
   FIELD ttGroup   AS C
   FIELD ttservcom AS C
   FIELD ttPrior   AS I    INIT 1
   INDEX igroup   ttgroup
   INDEX Servcom ttSERVcom.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "1"
ttBarring.ttservcom = "PES"
ttBarring.ttPrior   = 1.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "1"
ttBarring.ttservcom = "HES"
ttBarring.ttPrior   = 2.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "1"
ttBarring.ttservcom = "VES"
ttBarring.ttPrior   = 3.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "1"
ttBarring.ttservcom = "VEA"
ttBarring.ttPrior   = 4.


CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "2"
ttBarring.ttservcom = "SMP"
ttBarring.ttPrior   = 1.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "2"
ttBarring.ttservcom = "SMH"
ttBarring.ttPrior   = 2.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "2"
ttBarring.ttservcom = "SMV"
ttBarring.ttPrior   = 3.

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "2"
ttBarring.ttservcom = "SMA"
ttBarring.ttPrior   = 4.


CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "3"
ttBarring.ttservcom = "KLL".

CREATE ttBarring.
ASSIGN 
ttBarring.ttGroup   = "3"
ttBarring.ttservcom = "KLU".


