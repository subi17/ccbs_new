/* fctserval.i     14.12.04/aam

   get current values from clitype services 
   
   changes:        23.03.05/aam fServParamValue
*/


&IF "{&fServComValue}" NE "YES"
&THEN

&GLOBAL-DEFINE fServComValue YES

/* get current service component value from clitype */
FUNCTION fServComValue RETURNS INTEGER
   (INPUT  icCLIType AS CHAR,
    INPUT  icServCom AS CHAR,
    OUTPUT olAllowed AS LOG).
    
   DEF VAR liComValue AS INT NO-UNDO.
    
   ASSIGN liComValue = ?
          olAllowed  = FALSE.
   
   FOR FIRST CTServEl NO-LOCK WHERE
             CTServEl.Brand     = gcBrand   AND
             CTServEl.ServCom   = icServCom AND
             CTServEl.CLIType   = icCLIType AND
             CTServEl.FromDate <= TODAY:
     
      ASSIGN liComValue = CTServEl.DefValue
             olAllowed  = CTServEl.ChgAllowed.
   END.
   
   RETURN liComValue.
   
END FUNCTION.

/* get current service component's parameter value from clitype */
FUNCTION fServParamValue RETURNS CHAR
   (INPUT  icCLIType AS CHAR,
    INPUT  icServCom AS CHAR).
    
   DEF VAR lcComValue AS CHAR NO-UNDO.
    
   lcComValue = ?.
   
   FOR FIRST CTServEl NO-LOCK WHERE
             CTServEl.Brand     = gcBrand   AND
             CTServEl.ServCom   = icServCom AND
             CTServEl.CLIType   = icCLIType AND
             CTServEl.FromDate <= TODAY:
     
      lcComValue = CTServEl.DefParam.
   END.
   
   RETURN lcComValue.
   
END FUNCTION.


/* get current service attribute value from clitype */
FUNCTION fServAttrValue RETURNS CHARACTER
   (INPUT  icCLIType  AS CHAR,
    INPUT  icServCom  AS CHAR,
    INPUT  icServAttr AS CHAR,
    OUTPUT olAllowed  AS LOG).
    
   DEF VAR lcComValue AS CHAR NO-UNDO.
    
   ASSIGN lcComValue = ?
          olAllowed  = FALSE.
   
   FOR FIRST CTServEl NO-LOCK WHERE
             CTServEl.Brand     = gcBrand   AND
             CTServEl.ServCom   = icServCom AND
             CTServEl.CLIType   = icCLIType AND
             CTServEl.FromDate <= TODAY,
       FIRST CTServAttr OF CTServEl NO-LOCK WHERE
             CTServAttr.ServAttr = icServAttr:
      
      ASSIGN lcComValue = CTServAttr.DefValue
             olAllowed  = CTServAttr.ChgAllowed.
   END.
   
   RETURN lcComValue.
   
END FUNCTION.

&ENDIF

