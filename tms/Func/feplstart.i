/* feplstart.i      09.10.03/aam

   confirm the forming of an EPL file 
*/

FUNCTION fEPLStart RETURNS LOGICAL
  (icFlag AS CHAR).

   DEF VAR llStart AS LOG NO-UNDO.
   
   IF icFlag NE "" THEN DO:

      llStart = FALSE.
      
      MESSAGE "File will be created with the"
              (IF icFlag = "T" 
               THEN "test" 
               ELSE '"special work"')
              "flag" icFlag "on" 
              (IF icFlag = "T"
               THEN ", i.e. invoices will not be delivered to customers" +
                    " from the printing house."
               ELSE ".") SKIP
              "Do You want to continue ?"
      VIEW-AS ALERT-BOX
      QUESTION
      BUTTONS YES-NO
      TITLE " Exceptional EPL File " 
      SET llStart.
   END.
          
   ELSE DO:

      llStart = TRUE.
      
      MESSAGE "Start forming the EPL file ?"
      VIEW-AS ALERT-BOX
      QUESTION
      BUTTONS YES-NO
      TITLE " EPL File "
      SET llStart.
   END.
         
   RETURN llStart.
   
END FUNCTION.
