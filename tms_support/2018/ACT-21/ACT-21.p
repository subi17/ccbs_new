/* Changing tariff names as requested in ACT-21 */
/* https://kethor.qvantel.com/browse/ACT-21     */

DEF VAR liCont AS INTEGER NO-UNDO.

DEF VAR lcCode      AS CHARACTER NO-UNDO.
DEF VAR lcLabel     AS CHARACTER NO-UNDO.

DEF VAR lcCodeLabel AS CHARACTER NO-UNDO EXTENT 16.
ASSIGN
   lcCodeLabel [1]  = "CONT15,La Ciento 5 GB"                       /* La del Cero 5 GB */
   lcCodeLabel [2]  = "CONT26,La Sinfín 5 GB"                       /* La Infinita 5 GB */
   lcCodeLabel [3]  = "CONT25,La Sinfín 25 GB"                      /* La Sinfín */
   lcCodeLabel [4]  = "CONTDSL52,ADSL + La Sinfín 5 GB"             /* La Combinada Morada 20  */
   lcCodeLabel [5]  = "CONTFH52_50,Fibra 50 Mb + La Sinfín 5 GB"    /* La Combinada Morada 50 */
   lcCodeLabel [6]  = "CONTFH62_300,Fibra 300 Mb + La Sinfín 5 GB"  /* La Combinada Morada 300 */
   lcCodeLabel [7]  = "CONTFH82_1000,Fibra 1 Gb + La Sinfín 5 GB"   /* La Combinada Morada 1Gbps */
   lcCodeLabel [8]  = "CONTDSL59,ADSL + La Sinfín 25 GB"            /* La Combinada Azul 20 */
   lcCodeLabel [9]  = "CONTFH59_50,Fibra 50 Mb + La Sinfín 25 GB"   /* La Combinada Azul 50 */
   lcCodeLabel [10] = "CONTFH69_300,Fibra 300 Mb + La Sinfín 25 GB" /* La Combinada Azul 300 */
   lcCodeLabel [11] = "CONTFH89_1000,Fibra 1 Gb + La Sinfín 25 GB"  /* La Combinada Azul 1Gbps */
   lcCodeLabel [12] = "CONTDSL35,El ADSL 20 Mb"                     /* La de Casa 20 */ 
   lcCodeLabel [13] = "CONTFH35_50,La Fibra 50 Mb"                  /* La de Casa 50 */
   lcCodeLabel [14] = "CONTFH45_300,La Fibra 300 Mb"                /* La de Casa 300 */ 
   lcCodeLabel [15] = "CONTFH65_1000,La Fibra 1 Gb"                 /* La de Casa 1Gbps */
   lcCodeLabel [16] = "CONT28,La Dúo"                               /* Línea móvil Extra */ 
.

DO liCont = 1 to 16: 

   ASSIGN
      lcCode  = TRIM(ENTRY(1, lcCodeLabel[liCont]))
      lcLabel = TRIM(ENTRY(2, lcCodeLabel[liCont])).
 
   FOR EACH CLIType WHERE 
            CLIType.Brand   EQ "1" AND
            CLIType.Clitype EQ lcCode:
      CLIType.CLIName = lcLabel.
   END.
 
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

   FOR EACH ServiceLimitGroup WHERE
            ServiceLimitGroup.Brand     EQ "1" AND
            ServiceLimitGroup.GroupCode EQ lcCode:
      ServiceLimitGroup.GroupName = lcLabel.
   END.

END. /* DO liCont = 1 to 16 */
