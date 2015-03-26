/*------------------------------------------------------------
  MODULE .......: clivalif.i 
  FUNCTION .....: VALIDATE CLI-file FORMAT
  APPLICATION ..: 
  AUTHOR .......: PG Orbil
  CREATED ......: 20.09.1999 
  MODIFIED .....: 26.10.1999 1.0d   * Handle a missing header
                                      correctly
  Version ......: M15
--------------------------------------------------------------*/

DEFINE STREAM   valStream.          
DEFINE VARIABLE valRecord      AS CHARACTER NO-UNDO.
DEFINE VARIABLE valResellerID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE valCode        AS CHARACTER NO-UNDO.
DEFINE VARIABLE valOrderID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE valDetailCount AS CHARACTER NO-UNDO.
DEFINE VARIABLE valDetailFlag  AS LOGICAL   NO-UNDO INITIAL FALSE.   
DEFINE VARIABLE valTrailerFlag AS LOGICAL   NO-UNDO INITIAL FALSE. 
DEFINE VARIABLE valReturn      AS INTEGER   NO-UNDO.

FUNCTION valFile RETURNS INTEGER 
  (INPUT pFileName  AS CHARACTER,
   INPUT pSeparator AS CHARACTER,
   INPUT pReseller  AS CHARACTER).

   FIND FIRST TMSParam WHERE
              TMSParam.ParamGroup = pReseller        AND
              TMSParam.ParamCode  = "CodeDef"
   NO-LOCK No-ERROR.
   IF AVAIL TMSParam THEN valCode = TMSParam.CharVal. 

   ASSIGN
      valDetailFlag  = NO
      valTrailerFlag = NO
      valDetailCount = ""
      valOrderId     = ""
      valREsellerId  = ""
      valReturn      = 0.

   INPUT STREAM valStream FROM VALUE(pFileName).

   ValDtl:
   REPEAT:

      IMPORT STREAM valStream UNFORMATTED valRecord. 

      IF ENTRY(1,valRecord,pSeparator) = "HDR" THEN DO:        
         ENTRY(5,valRecord,pSeparator) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN DO:
            valReturn = 13.
            LEAVE ValDtl.
         END. /* END IF */    
         ENTRY(6,valRecord,pSeparator) NO-ERROR. 
         IF NOT ERROR-STATUS:ERROR THEN DO:
            valReturn = 13.
            LEAVE ValDtl.
         END. /* END IF */    
         valOrderID = ENTRY(3,valRecord,pSeparator).

         IF NOT valCode = SUBSTRING(valOrderID,1,length(valCode)) THEN DO:
            valReturn = 13.
            LEAVE ValDtl.
         END. /* END IF */
         IF NOT LENGTH(valOrderID) = 8 THEN DO:
            valReturn = 8.
            LEAVE ValDtl.
         END.  /* END IF */
         valResellerID = ENTRY(2,valRecord,pSeparator).        
         LEAVE ValDtl.
      END. /* END IF */                
      ELSE IF ENTRY(1,valRecord,pSeparator) = "DTL" OR
              ENTRY(1,valRecord,pSeparator) = "TLR" THEN DO:        
        valReturn = 10.
        LEAVE ValDtl.
      END.
      /* This means that the reqfile's FIRST ROW is NOT the header */
      ELSE valReturn = 3.

   END. /* REPEAT ValDtl: */                  

   IF valReturn NE 0 THEN DO:
      INPUT STREAM valStream CLOSE.
      RETURN valReturn.
   END.

   val-dtl:
   REPEAT:

      IMPORT STREAM valStream UNFORMATTED valRecord.

      IF valTrailerFlag THEN LEAVE val-dtl.

      CASE ENTRY(1,valRecord,pSeparator):
         WHEN "DTL" THEN DO:
            ENTRY(5,valRecord,pSeparator) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               valReturn = 10.
               LEAVE val-dtl.
            END. /* END IF */
            ENTRY(6,valRecord,pSeparator) NO-ERROR. 
            IF NOT ERROR-STATUS:ERROR THEN DO:
               valReturn = 14.
               LEAVE val-dtl.
            END. /* END IF */

            ASSIGN
               valDetailCount = STRING(INT(valDetailCount) + 1)
               valDetailFlag  = TRUE.
         END. /* WHEN "DTL" */       

         WHEN "TLR" THEN DO:
            ENTRY(5,valRecord,pSeparator) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
               valReturn = 15.
               LEAVE val-dtl.
            END. /* END IF */
            ENTRY(6,valRecord,pSeparator) NO-ERROR. 
            IF NOT ERROR-STATUS:ERROR THEN DO:
               valReturn = 15.
               LEAVE val-dtl.
            END. /* END IF */
            IF NOT ENTRY(2,valRecord,pSeparator) = valResellerID THEN DO:
               valReturn = 7.
               LEAVE val-dtl.
            END. /* END IF */
            IF NOT ENTRY(3,valRecord,pSeparator) = valOrderID THEN DO:
               valReturn = 8.
               LEAVE val-dtl.
            END.  /* END IF */
            IF NOT ENTRY(4,valRecord,pSeparator) = valDetailCount THEN DO:
               valReturn = 10.
               LEAVE val-dtl.
            END. /* END IF */
            valTrailerFlag = TRUE.

         END.    /* END WHEN "TLR" */

      END.    /* END CASE */

   END.        /* REPEAT val-dtl */

   IF valReturn NE 0 THEN DO:
      INPUT STREAM valStream CLOSE.
      RETURN valReturn.
   END.

   IF NOT valDetailFlag OR NOT valTrailerFlag THEN DO:
      IF NOT valTrailerFlag THEN DO:
         valReturn = 12.
      END. /* END IF */    
      ELSE DO:
         valReturn = 11.
      END. /* END ELSE */
   END.  /* END ELSE */

   INPUT STREAM valStream CLOSE.

   RETURN valReturn.

END FUNCTION.      

