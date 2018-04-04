DEF VAR liCont      AS INTEGER   NO-UNDO.
DEF VAR lcCode      AS CHARACTER NO-UNDO.
DEF VAR lcLabel     AS CHARACTER NO-UNDO FORMAT "X(35)".
DEF VAR lcCodeLabel AS CHARACTER NO-UNDO EXTENT 5.
DEF VAR cNewTxt     AS CHARACTER NO-UNDO FORMAT "X(35)".

ASSIGN
   lcCodeLabel [1]  = "CONTDSL99, ADSL + La Sinfín Infinitos GB"
   lcCodeLabel [2]  = "CONTFH99_50, Fibra 50 Mb + La Sinfín Infinitos GB"
   lcCodeLabel [3]  = "CONTFH109_300, Fibra 300 Mb + La Sinfín Infinitos GB"
   lcCodeLabel [4]  = "CONTFH129_1000, Fibra 1 Gb + La Sinfín Infinitos GB"
   lcCodeLabel [5]  = "CONT29, La Dúo".

DO liCont = 1 TO 5: 

   ASSIGN
      lcCode  = TRIM(ENTRY(1, lcCodeLabel[liCont]))
      lcLabel = TRIM(ENTRY(2, lcCodeLabel[liCont])).
 
   MESSAGE "Checking clitype for" lcCode VIEW-AS ALERT-BOX.
   FOR EACH CLIType WHERE 
            CLIType.Brand   EQ "1" AND
            CLIType.Clitype EQ lcCode NO-LOCK:
      DISP CLIType.CLIName FORMAT "X(35)" COLUMN-LABEL "Clitype" lcLabel.
   END.
 
   MESSAGE "Checking daycampaign for" lcCode VIEW-AS ALERT-BOX.
   FOR EACH DayCampaign WHERE
            DayCampaign.Brand   EQ "1" AND
            DayCampaign.DCEvent EQ lcCode NO-LOCK:
      DISP DayCampaign.DCName FORMAT "X(35)" COLUMN-LABEL "Daycampaign" lcLabel.
   END.

   MESSAGE "Checking reptext with TextType 9 for" lcCode VIEW-AS ALERT-BOX.
   FOR EACH RepText WHERE 
            RepText.TextType EQ 9 AND
            RepText.LinkCode EQ lcCode NO-LOCK:
      DISP RepText.RepText FORMAT "X(35)" COLUMN-LABEL "RepText 9" lcLabel.
   END.

   MESSAGE "Checking reptext for" lcCode VIEW-AS ALERT-BOX.
   FOR EACH RepText WHERE 
            RepText.LinkCode BEGINS lcCode NO-LOCK:
      
      cNewTxt = REPLACE(RepText.RepText,"La Dúo Interminable",lcLabel).
      
      DISP RepText.LinkCode FORMAT "X(15)" COLUMN-LABEL "Link code"
           RepText.RepText FORMAT "X(25)" COLUMN-LABEL "RepText" cNewTxt.
   END.


   MESSAGE "Checking servicelimitgroup for" lcCode VIEW-AS ALERT-BOX.
   FOR EACH ServiceLimitGroup WHERE
            ServiceLimitGroup.Brand     EQ "1" AND
            ServiceLimitGroup.GroupCode EQ lcCode NO-LOCK:
      DISP ServiceLimitGroup.GroupName FORMAT "X(35)" COLUMN-LABEL "ServLimitGroup" lcLabel.
   END.
 
END. /* DO liCont = 1 TO 5 */