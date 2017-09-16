/*YDR-2632
Make new retention configuration. The new config supports dividing % for each
operator/operator category (for example Masmovil and Pepe are in same category)

The progam closes ll existing configs.

*/


DEF VAR lcBrand AS CHAR NO-UNDO INIT "1".
DEF VAR FromDate AS DATE INIT TODAY.
DEF VAR ldePercentage AS decimal NO-UNDO.
DEF VAR lcRetentionPlatform AS CHAR NO-UNDO.
DEF VAR lcSMSSender AS CHAR NO-UNDO.
DEF VAR TODate AS DATE INIT  12/31/49.
DEF VAR lcConf AS CHAR NO-UNDO.
DEF VAR liCount AS INT NO-UNDO INIT 4.

message "Showing old configs" VIEW-AS ALERT-BOX.
FOR EACH MNPRetPlatForm NO-LOCK :
   DISP MNPRetPlatForm.
END.

/*Close existing configs.*/
FOR EACH MNPRetPlatForm  WHERE
         MNPRetPlatForm.brand eq lcBrand and
         MNPRetPlatForm.Todate >= TODAY:
   MNPRetPlatForm.ToDate = TODAY - 1.
END.

FUNCTION fMNPRet RETURNS LOGICAL
   (icConf AS CHAR,
    /*icPlatf AS CHAR,*/
    idPer AS DECIMAL,
    icSMSNBR AS CHAR):
   liCount = liCount + 1.
   CREATE MNPRetPlatForm.
   ASSIGN MNPRetPlatForm.Brand = lcBrand /*constant*/
          MNPRetPlatForm.FromDate = FromDate /*constant*/
          MNPRetPlatForm.Percentage = idPer
          MNPRetPlatForm.RetentionPlatform = STRING(liCount)
          MNPRetPlatForm.SMSSender = icSMSNBR
          MNPRetPlatForm.Name = ENTRY(1,icConf,";")
          MNPRetPlatForm.Operators = ENTRY(2,icConf,";")
          MNPRetPlatForm.ToDate = ToDate. /*constant*/
END.

/*Row 1*/

/*Change this values*/
lcConf = "Castilian1;Movistar".
ldePercentage = 28.
lcSMSSender = "800622123".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Emergia;Movistar".
ldePercentage = 61.
lcSMSSender = "800622022".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Marktel;Movistar".
ldePercentage = 11.
lcSMSSender = "8000000". /*!!!!!!!!!!!!!!!!!*/
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).


/*Row 2*/

/*Change this values*/
lcConf = "Castilian1;Vodafone*".
ldePercentage = 44.
lcSMSSender = "800622123".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Emergia;Vodafone*".
ldePercentage = 45.
lcSMSSender = "800622022".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Marktel;Vodafone*".
ldePercentage = 11.
lcSMSSender = "8000000". /*!!!!!!!!!!!!!!!!!*/
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).



/*Row 3*/

/*Change this values*/
lcConf = "Castilian1;Orange,Jazztel".
ldePercentage = 44.
lcSMSSender = "800622123".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Emergia;Orange,Jazztel".
ldePercentage = 45.
lcSMSSender = "800622022".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Marktel;Orange,Jazztel".
ldePercentage = 11.
lcSMSSender = "8000000". /*!!!!!!!!!!!!!!!!!*/
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).



/*Row 4*/

/*Change this values*/
lcConf = "Castilian1;M*vil,Pepe*".
ldePercentage = 50.
lcSMSSender = "800622123".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Emergia;M*vil,Pepe*".
ldePercentage = 50.
lcSMSSender = "800622022".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
/*lcConf = "Marktel;".
lcRetentionPlatform = "3".
ldePercentage = 0.
lcSMSSender = "8000000"./*!!!!!!!!!!!!!!!!!*/
fMNPRet(lcConf, 
        lcRetentionPlatform, 
        ldePercentage, 
        lcSMSSender).
*/

/*Row 5*/

/*Change this values*/
lcConf = "Castilian1;".
ldePercentage = 70.
lcSMSSender = "800622123".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Emergia;".
ldePercentage = 17.
lcSMSSender = "800622022".
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).

/*Change this values*/
lcConf = "Marktel;".
ldePercentage = 13.
lcSMSSender = "8000000". /*!!!!!!!!!!!!!!!!!*/
fMNPRet(lcConf, 
        ldePercentage, 
        lcSMSSender).





