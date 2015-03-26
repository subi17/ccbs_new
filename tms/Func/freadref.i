/* freadref.i       09.02.04/aam
  
   get customer nbr and/or invoice nbr from reference 
   
   callers: nnocko.p
            nnockott.p

   changes:         28.09.04/aam first character "6" 
                    10.12.04/aam first character "7"
                    17.06.05/aam check length of reference in "standard nbr"
                    22.03.06/aam new structure for payment plan 
*/

DEF VAR liRefNumForm AS INT NO-UNDO. 

/* contents of reference nbr */
liRefNumForm = fCParamI("RefNumForm").


FUNCTION fReadRefNum RETURNS CHARACTER
   (icRefNum AS CHAR,
    OUTPUT oiCustNum AS INT,
    OUTPUT oiInvNum  AS INT).
    
   DEF VAR lcPaymSrc AS CHAR NO-UNDO.
   DEF VAR liRefCnt  AS INT  NO-UNDO. 
       
   ASSIGN lcPaymSrc = ""
          oiInvNum  = 0
          oiCustNum = 0.
       
   /* special reference nbr */
   IF icRefNum BEGINS "1" AND
      LENGTH(icRefNum) = 10
   THEN ASSIGN oiCustNum = INTEGER(SUBSTRING(icRefNum,2,8))
               lcPaymSrc = "V".

   /* payment plan */
   ELSE IF icRefNum BEGINS "2" AND
      LENGTH(icRefNum) = 17
   THEN ASSIGN oiCustNum = INTEGER(SUBSTRING(icRefNum,9,8))
               oiInvNum  = INTEGER(SUBSTRING(icRefNum,2,7)) /* PPlanID*/
               lcPaymSrc = "L".

   /* deposit invoice */
   ELSE IF icRefNum BEGINS "3" AND
      LENGTH(icRefNum) = 10
   THEN ASSIGN oiInvNum = INTEGER(SUBSTRING(icRefNum,2,8))
               lcPaymSrc = "D".

   /* adv.payment invoice */
   ELSE IF icRefNum BEGINS "4" AND
      LENGTH(icRefNum) = 10
   THEN ASSIGN oiInvNum = INTEGER(SUBSTRING(icRefNum,2,8))
               lcPaymSrc = "A".

   /* addition to subscription balance */
   ELSE IF icRefNum BEGINS "6" AND
      LENGTH(icRefNum) = 10
   THEN ASSIGN oiCustNum = INTEGER(SUBSTRING(icRefNum,2,8))
               lcPaymSrc = "S".

   /* addition to subscription balance as gift */
   ELSE IF icRefNum BEGINS "7" AND
      LENGTH(icRefNum) = 10
   THEN ASSIGN oiCustNum = INTEGER(SUBSTRING(icRefNum,2,8))
               lcPaymSrc = "G".
    
   /* standard nbr */
   ELSE DO:
      /* remove leading zeros */
      DO liRefCnt = 1 TO LENGTH(icRefNum):
         IF SUBSTRING(icRefNum,liRefCnt,1) NE "0" THEN LEAVE.
      END.
      
      icRefNum = SUBSTRING(icRefNum,liRefCnt).
      
      CASE liRefNumForm:
      
      WHEN 1 THEN DO:
         IF LENGTH(icRefNum) <= 9
         THEN oiCustNum = INT(SUBSTRING(icRefNum,1,LENGTH(icRefNum) - 1)).
      END.   
      
      WHEN 2 THEN DO:
         IF LENGTH(icRefNum) <= 17 AND LENGTH(icRefNum) > 9 
         THEN ASSIGN oiCustNum = INT(SUBSTRING(icRefNum,1,
                                               LENGTH(icRefNum) - 9))
                     oiInvNum  = INT(SUBSTRING(icRefNum,
                                               LENGTH(icRefNum) - 8,
                                               8)).
      END.                                         
      
      OTHERWISE DO:
        IF LENGTH(icRefNum) <= 9  
        THEN oiInvNum = INT(SUBSTRING(icRefNum,1,LENGTH(icRefNum) - 1)).
      END. 
      
      END CASE.                                   
   END.

   RETURN lcPaymSrc.
   
END FUNCTION.

