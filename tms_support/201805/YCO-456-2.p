 /*---------------------------------------------------------
 YCO-456.
 /tms_support/201805/YCO-456-2.p
 Subscription types allowed for new periodical contracts.
 DTERM12-120, DTERM24-240
 jotorres. 28/05/2018
---------------------------------------------------------*/
 
 DEFINE VARIABLE liCont        AS INTEGER   NO-UNDO.
 DEFINE VARIABLE lcCliTypeList AS CHARACTER NO-UNDO.
 
 lcCliTypeList =
    "CONT15,CONT25,CONT33,CONT34," +
    "CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFH39_50,"   + 
    "CONTFH49_300,CONTFH69_1000,CONTFH48_50,CONTFH58_300,"  + 
    "CONTFH76_1000,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000," +
    "CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTFH59_50,"   +
    "CONTFH69_300,CONTFH89_1000,CONTFH99_50,CONTFH109_300," +
    "CONTFH129_1000,CONTFH35_50,CONTFH45_300,CONTFH65_1000".
 
 DO liCont = 1 TO NUM-ENTRIES(lcCliTypeList):
    
    /* Matrix must exist */
    /* (Previously I've checked that all the CliTypes have Matrix record). */
    FIND Matrix NO-LOCK WHERE 
         Matrix.Brand  EQ "1" AND 
         Matrix.MXKey  EQ "PERCONTR" AND 
         Matrix.MXName EQ ENTRY(liCont, lcCliTypeList) NO-ERROR.
    IF NOT AVAILABLE Matrix THEN 
      NEXT.
    
    /* DTERM12-120*/
    FIND FIRST MXItem NO-LOCK WHERE
               MXItem.MXSeq   EQ Matrix.MXSeq  AND 
               MXItem.MXName  EQ "Percontract" AND
               MXItem.MXValue EQ "DTERM12-120" NO-ERROR.
    IF NOT AVAILABLE MXItem THEN DO:
       CREATE MXItem.
       ASSIGN
          MXItem.MXSeq   = Matrix.MXSeq   
          MXItem.MXName  = "Percontract" 
          MXItem.MXValue = "DTERM12-120".  
    END. 
    
    /* DTERM24-240 */
    FIND FIRST MXItem NO-LOCK WHERE
               MXItem.MXSeq   EQ Matrix.MXSeq  AND 
               MXItem.MXName  EQ "Percontract" AND
               MXItem.MXValue EQ "DTERM24-240" NO-ERROR.
    IF NOT AVAILABLE MXItem THEN DO:
       CREATE MXItem.
       ASSIGN
          MXItem.MXSeq   = Matrix.MXSeq   
          MXItem.MXName  = "Percontract" 
          MXItem.MXValue = "DTERM24-240".  
    END.
    
    RELEASE Matrix.              
 END.   
