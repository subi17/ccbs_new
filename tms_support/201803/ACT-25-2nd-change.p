DEF VAR liCont      AS INTEGER   NO-UNDO.
DEF VAR lcCode      AS CHARACTER NO-UNDO.
DEF VAR lcLabel     AS CHARACTER NO-UNDO.
DEF VAR lcCodeLabel AS CHARACTER NO-UNDO EXTENT 5.

ASSIGN
   lcCodeLabel [1]  = "CONTDSL99, ADSL + La Sinfín Infinitos GB"
   lcCodeLabel [2]  = "CONTFH99_50, Fibra 50 Mb + La Sinfín Infinitos GB"
   lcCodeLabel [3]  = "CONTFH109_300, Fibra 300 Mb + La Sinfín Infinitos GB"
   lcCodeLabel [4]  = "CONTFH129_1000, Fibra 1 Gb + La Sinfín Infinitos GB".
   lcCodeLabel [5]  = "CONT29, La Dúo".

DO liCont = 1 TO 5: 

   ASSIGN
      lcCode  = TRIM(ENTRY(1, lcCodeLabel[liCont]))
      lcLabel = TRIM(ENTRY(2, lcCodeLabel[liCont])).
 
   FOR EACH CLIType WHERE 
            CLIType.Brand   EQ "1" AND
            CLIType.Clitype EQ lcCode:
      CLIType.CLIName = lcLabel.
   END.
   
   /* ****************************************************
      According to findings made with ACT-25-check.p:
      - Except for CONT29, only the CLIType table needs to be updated
      - For CONT29, more tables need to be updated 
      **************************************************** */
   IF lcCode = "CONT29" then
   DO:
      FOR EACH DayCampaign WHERE
               DayCampaign.Brand   EQ "1" AND
               DayCampaign.DCEvent EQ lcCode:
         DayCampaign.DCName = lcLabel.
      END.
   
      FOR EACH RepText WHERE 
               RepText.TextType EQ 9 AND
               RepText.LinkCode EQ lcCode:
         RepText.RepText = lcLabel.
      END.
      
      FOR EACH RepText WHERE 
               RepText.LinkCode EQ (lcCode + "MF"):
        RepText.RepText = lcLabel.
      END.
      
      FOR EACH RepText WHERE 
               RepText.LinkCode EQ (lcCode + "DISC"):
        RepText.RepText = REPLACE(RepText.RepText,"La Dúo Interminable",lcLabel).
      END.
      
   
      FOR EACH ServiceLimitGroup WHERE
               ServiceLimitGroup.Brand     EQ "1" AND
               ServiceLimitGroup.GroupCode EQ lcCode:
         ServiceLimitGroup.GroupName = lcLabel.
      END.
   END.
   
END. /* DO liCont = 1 TO 5 */