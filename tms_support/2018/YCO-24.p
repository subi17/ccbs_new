/* Changing tariff names as requested in YCO-24 */
/* https://kethor.qvantel.com/browse/YCO-24     */

DEF VAR liCont   AS INTEGER   NO-UNDO.
DEF VAR lcCode   AS CHARACTER NO-UNDO.
DEF VAR lcLabel  AS CHARACTER NO-UNDO.

DEF VAR lcCodeLabel AS CHARACTER NO-UNDO EXTENT 8.
ASSIGN                    
   lcCodeLabel [1]  = "CONTFH69_1000,Fibra 1 Gb + La del Cero 1,5 GB"   /* La Combinada Naranja 1Gbps */
   lcCodeLabel [2]  = "CONTFH49_300,Fibra 300 Mb + La del Cero 1,5 GB"  /* La Combinada Naranja 300 */
   lcCodeLabel [3]  = "CONTFH39_50,Fibra 50 Mb + La del Cero 1,5 GB"    /* La Combinada Naranja 50 */
   lcCodeLabel [4]  = "CONTDSL39,ADSL + La del Cero 1,5 GB"             /* La Combinada Naranja 20  */
   lcCodeLabel [5]  = "CONTDSL48,ADSL + La Doscientos 5 GB"             /* La Combinada Verde 20 */
   lcCodeLabel [6]  = "CONTFH48_50,Fibra 50 Mb + La Doscientos 5 GB,"   /* La Combinada Verde 50 */
   lcCodeLabel [7]  = "CONTFH58_300,Fibra 300 Mb + La Doscientos 5 GB"  /* La Combinada Verde 300 */
   lcCodeLabel [8]  = "CONTFH76_1000,Fibra 1 Gb + La Doscientos 5 GB"   /* La Combinada Verde 1Gbps */
.

DO liCont = 1 to 8: 

   ASSIGN
      lcCode  = TRIM(ENTRY(1, lcCodeLabel[liCont]))
      lcLabel = TRIM(ENTRY(2, lcCodeLabel[liCont])).
 
   FOR EACH CLIType WHERE 
            CLIType.Brand   EQ "1" AND
            CLIType.Clitype EQ lcCode:
      CLIType.CLIName = lcLabel.
   END.

   FOR EACH RepText WHERE 
            RepText.TextType EQ 9 AND
            RepText.LinkCode EQ lcCode:
      RepText.RepText = lcLabel.
   END.

END. /* DO liCont = 1 to 8 */