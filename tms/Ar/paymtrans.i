/* paymtrans.i      03.05.05/aam separated from nnkosu
*/

DEF VAR lcBuDir   AS CHAR NO-UNDO.

/* backup directory for payment PaymFile */
lcBuDir = fCParamC("OcrBackup").


PROCEDURE pDeleteFile:

   DEF INPUT PARAMETER icPaymFile AS CHAR NO-UNDO.

   DEF VAR lcBuFile  AS CHAR NO-UNDO.
   DEF VAR lcBuAdd   AS CHAR NO-UNDO.
   DEF VAR liBuCnt   AS INT  NO-UNDO.

   IF lcBuDir NE "" THEN DO:

      /* backup PaymFile */
      IF INDEX(icPaymFile,"/") GT 0
      THEN ASSIGN liBuCnt  = R-INDEX(icPaymFile,"/")
                  lcBuFile = SUBSTRING(icPaymFile,liBuCnt + 1).
      ELSE ASSIGN lcBuFile = icPaymFile.

      ASSIGN lcBuFile = lcBuDir + "/" + lcBuFile + "_" + 
                                string(year(today) mod 100,"99") +
                                string(month(today),"99") +
                                string(day(today),"99")
             liBuCnt  = 0
             lcBuAdd  = "".

      /* check that the file doesn't exist */        
      REPEAT:                                                  
          IF SEARCH(lcBuFile + lcBuAdd) = ? THEN LEAVE.
          ASSIGN liBuCnt = liBuCnt + 1
                 lcBuAdd = "_" + string(liBuCnt).
      END.
      ASSIGN lcBuFile = lcBuFile + lcBuAdd.                              

      UNIX SILENT mv VALUE(icPaymFile + " " + lcBuFile).

   END.

   ELSE UNIX SILENT rm VALUE(icPaymFile).
                                
END PROCEDURE.

