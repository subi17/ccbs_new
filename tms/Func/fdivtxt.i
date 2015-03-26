/* fdivtxt.i        16.02.04/aam separated from invotxtp.i

*/


FUNCTION fSeparateInvoTxt RETURNS CHARACTER
    (InvText   AS CHAR,
     iLength   AS INT). 

     DEF VAR lcString AS CHAR NO-UNDO.
     DEF VAR lInvoTxt AS CHAR NO-UNDO. 
     DEF VAR lSplit   AS INT  NO-UNDO. 

     IF InvText = "" THEN RETURN "". 

     /* divide lines from spaces, hyphens AND line feeds */
     REPEAT:

        /* IF line feed included THEN split from that */
        ASSIGN lSplit = INDEX(InvText,CHR(10)).

        /* line LENGTH is less */
        IF lSplit > iLength OR
           (lSplit = 0 AND LENGTH(InvText) > iLength) 
        THEN DO:

           /* FIRST take MAXIMUM LENGTH + 1 
              (in case the next character after max length is a space 
               or hyphen ..) 
           */
           ASSIGN lInvoTxt = SUBSTRING(InvText,1,iLength + 1). 

           /* FIND the FIRST space OR hyphen beginning from the END */
           DO lSplit = LENGTH(lInvoTxt) TO 1 BY -1:
              IF SUBSTRING(lInvoTxt,lSplit,1) = CHR(32) OR
                 SUBSTRING(lInvoTxt,lSplit,1) = CHR(45)
              THEN DO:
                 IF SUBSTRING(lInvoTxt,lSplit,1) NE CHR(32)
                 THEN lSplit = MIN(lSplit,iLength).
                 LEAVE.
              END. 
           END. 

           /* IF cannot split THEN just print MAXIMUM LENGTH */
           IF lSplit LE 1 THEN ASSIGN lSplit = iLength. 

        END.
        /* ALL fits */
        ELSE IF lSplit = 0 THEN ASSIGN lSplit = LENGTH(InvText). 

        ASSIGN lInvoTxt = RIGHT-TRIM(SUBSTRING(InvText,1,lSplit))
               /* remove lf */                               
               lInvoTxt = REPLACE(lInvoTxt,CHR(10),"")
               
               lcString = lcString + lInvoTxt.

        /* ALL done */
        IF lSplit >= LENGTH(InvText) 
        THEN LEAVE.
        ELSE lcString = lcString + CHR(9).

        /* NEXT line */
        ASSIGN InvText = SUBSTRING(InvText,lSplit + 1).

     END.        

     RETURN lcString.

END FUNCTION. 

