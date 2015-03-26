DEFINE VARIABLE ldToday  AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liRowNum AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttBifo NO-UNDO
   FIELD tcDB     AS CHARACTER
   FIELD tcArea   AS CHARACTER
   FIELD tiRowNum AS INTEGER
   FIELD tdeFree  AS DECIMAL
   FIELD tiSize   AS INTEGER
   FIELD tiTime   AS INTEGER
   FIELD tdaDate  AS DATE
   
   INDEX idxArea AS PRIMARY
      tcDB
      tiRowNum.

DEFINE BUFFER bufBifo FOR ttBifo.

DEFINE STREAM sArea.

FORM
   SKIP
   liLoop
   ldToday  FORMAT "99.99.9999"
   lcTime
WITH
   ROW 6 WIDTH 36 CENTERED OVERLAY NO-LABEL
   TITLE " BIFO AREASTATUS STARTED "
FRAME frmLog.
                  
SESSION:NUMERIC-FORMAT = "EUROPEAN".

DO WHILE TRUE:

   IF ldToday NE TODAY THEN ASSIGN
      ldToday = TODAY
      liLoop  = 0.
               
   PUT SCREEN ROW 22 COL 1
      "BIFO AREASTATUS RUNNING ....                             ".
   
 /*   RUN pBifoReport(INPUT-OUTPUT liRowNum). */

   RUN pAreaReport.

   liLoop = liLoop + 1.
   
   DISP
      liLoop
      ldToday
      STRING(TIME,"hh:mm:ss") @ lcTime
   WITH FRAME frmLog.
   
   PAUSE 0.
   
   PUT SCREEN ROW 22 COL 1
      "F8 TO QUIT, OTHER KEYS START BIFO AREASTATUS IMMEDIATELLY".

   READKEY PAUSE 300.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN QUIT.

END.

PROCEDURE pBifoReport:

   DEFINE INPUT-OUTPUT PARAMETER liRowNum AS INTEGER NO-UNDO.

   DEFINE VARIABLE liLoop1       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDB          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhTable       AS HANDLE    NO-UNDO.   
   DEFINE VARIABLE lhField       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lhQuery       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcQuery       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcField       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lDEFree%      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liFreeB       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcAreaName    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liHiWater     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liTotBlocks   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liExtents     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcLastExtent  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liFreeExtents AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeFreePrev1  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeFreePrev2  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE ldeFreePrev3  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE liSizePrev    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcStorageArea AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcAreaType    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcMessage     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSMS         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResponse    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcEOF         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldeFSize      AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcFSize       AS CHARACTER NO-UNDO.

   DEFINE VARIABLE lhTableX      AS HANDLE    NO-UNDO.   
   DEFINE VARIABLE lhFieldX      AS HANDLE    NO-UNDO.   
   DEFINE VARIABLE lhQueryX      AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcQueryX      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSpoolPath   AS CHARACTER NO-UNDO.

   lcSpoolPath = "/scratch/nagios/".

   OUTPUT STREAM sArea TO VALUE(lcSpoolPath + "areastatus_bifo").

   CREATE QUERY lhQuery.

   lcMessage = "".

   DO liLoop1 = 1 to NUM-DBS:

      ASSIGN
         lcDB = LDBNAME(liLoop1)
         ldeFreePrev1 = 0
         ldeFreePrev2 = 0
         ldeFreePrev3 = 0.
           
      FIND FIRST bufBifo WHERE
                 bufBifo.tcDB     = lcDB AND
                 bufBifo.tiRowNum = liRowNum
      NO-LOCK NO-ERROR.
   
      IF AVAIL bufBifo THEN DO:
      
         ASSIGN
            liSizePrev   = bufBifo.tiSize
            ldeFreePrev1 = bufBifo.tdeFree.
      
         IF liRowNum >= 4 THEN DO:
         
            FIND FIRST bufBifo WHERE
                       bufBifo.tcDB     = lcDB AND
                       bufBifo.tiRowNum = liRowNum - 4
            NO-LOCK NO-ERROR.
         
            IF AVAIL bufBifo THEN ldeFreePrev2 = bufBifo.tdeFree.

         END.
         
         IF liRowNum > 96 THEN DO:
         
            FIND FIRST bufBifo WHERE
                       bufBifo.tcDB = lcDB
            EXCLUSIVE-LOCK NO-ERROR.

            ldeFreePrev3 = bufBifo.tdeFree.

            DELETE bufBifo.
      
         END.

      END.
   
      EMPTY TEMP-TABLE ttBifo.
      
      CREATE ttBifo.
      ASSIGN
         liRowNum        = liRowNum + 1
         ttBifo.tiRowNum = liRowNum
         ttBifo.tcDB     = UPPER(lcDB)
         ttBifo.tdaDate  = TODAY
         ttBifo.tiTime   = TIME.

      CREATE BUFFER lhTable FOR TABLE lcDB + "._AreaStatus".

      lcQuery = "FOR EACH " + lcDB + "._AreaStatus WHERE "
                            + lcDB + "._AreaStatus._AreaStatus-AreaName = " 
                            + "'Primary Recovery Area'".

      lhQuery:SET-BUFFERS(lhTable).
      lhQuery:QUERY-PREPARE(lcQuery).
      lhQuery:QUERY-OPEN.
      
      lhQuery:GET-NEXT(NO-LOCK).
      
      DO liLoop2 = 1 TO lhTable:NUM-FIELDS:

         lhField = lhTable:BUFFER-FIELD(liLoop2).

         CASE lhField:NAME:
            WHEN "_AreaStatus-HiWater"    THEN
               liHiWater    = lhField:BUFFER-VALUE.
            WHEN "_AreaStatus-TotBlocks"  THEN
               liTotBlocks  = lhField:BUFFER-VALUE.
            WHEN "_AreaStatus-Lastextent" THEN
               lcLastExtent = lhField:BUFFER-VALUE.
            WHEN "_AreaStatus-Extents" THEN
               liExtents    = lhField:BUFFER-VALUE.
            WHEN "_AreaStatus-AreaName" THEN
               lcAreaName   = lhField:BUFFER-VALUE.
         END.
   
      END.
      
      INPUT THROUGH VALUE("ls -l " + lcLastExtent).
      IMPORT UNFORMATTED lcFSize.
      INPUT CLOSE.

      DO WHILE INDEX(lcFSize,"  ") > 0:
         lcFSize = REPLACE(lcFSize,"  "," ").
      END.
      
      lcFSize = ENTRY(5,lcFSize," ").

      ASSIGN
         ldeFSize = DEC(lcFSize) / 1024000000
         lcFSize  = STRING(ldeFSize,">9.99") + "GB".

      ASSIGN
         liLoop2  = INDEX(lcLastExtent,".b")
         liFreeExtents  = liExtents - INT(SUBSTR(lcLastExtent,liLoop2 + 2))
         ldeFree% = 100 - (liHiWater / liTotBlocks) * 100
         liFreeB  = liTotBlocks - liHiWater
         lcAreaName     = REPLACE(lcAreaName," ","")
         ttBifo.tcArea  = lcAreaName
         ttBifo.tdeFree = ldeFree%.

      IF ttBifo.tcArea = "PrimaryRecoveryArea" THEN ttBifo.tcArea = "Bifo".

      IF   ldeFreePrev1 = 0 THEN ldeFreePrev1 = ldeFree%.
      ELSE ldeFreePrev1 = ldeFreePrev1 - ldeFree%.

      IF   ldeFreePrev2 = 0 THEN ldeFreePrev2 = ldeFree%.
      ELSE ldeFreePrev2 = ldeFreePrev2 - ldeFree%.

      IF   ldeFreePrev3 = 0 THEN ldeFreePrev3 = ldeFree%.
      ELSE ldeFreePrev3 = ldeFreePrev3 - ldeFree%.

      PUT STREAM sArea UNFORMATTED
         ttBifo.tdaDate FORMAT "99.99.9999"  AT 1
         STRING(ttBifo.tiTime,"HH:MM:SS")    AT 12
         ttBifo.tcDB                         AT 21
         liTotBlocks                         AT 32
         liFreeB        FORMAT ">>>>>>>9"    AT 39
         ldeFree%       FORMAT ">>9.99"      AT 48
         liFreeExtents  FORMAT ">9"          AT 54
         lcFSize                             AT 56.

      CREATE QUERY lhQueryX.

      CREATE BUFFER lhTableX FOR TABLE lcDB + "._AreaExtent".

      lcQueryX = "FOR EACH " + lcDB + "._AreaExtent WHERE "
                             + lcDB + "._AreaExtent._Extent-Path MATCHES " 
                             + "'*\\\\.b*'".
      
      lhQueryX:SET-BUFFERS(lhTableX).
      lhQueryX:QUERY-PREPARE(lcQueryX).
      lhQueryX:QUERY-OPEN.
      
      lhQueryX:GET-LAST(NO-LOCK).

      lhFieldX = lhTableX:BUFFER-FIELD("_Extent-Size").
      ttBifo.tiSize = lhFieldX:BUFFER-VALUE.

      lhFieldX = lhTableX:BUFFER-FIELD("_Extent-Type").

      IF lhFieldX:BUFFER-VALUE = 6 THEN lcAreaType = "VARIABLE".
      ELSE                              lcAreaType = "FIXED".

      IF ldeFree% < 40 THEN DO:

         IF lhFieldX:BUFFER-VALUE NE 6 THEN
            lcMessage  = lcMessage + ttBifo.tcDB + "." + ttBifo.tcArea + ": " +
                         STRING(INT(ldeFree%)) + "% free. ".
         
         ELSE IF liSizePrev = 0 AND liFreeExtents = 0 THEN
            lcMessage  = lcMessage + ttBifo.tcDB + "." + ttBifo.tcArea + ": " +
                         STRING(INT(ldeFree%)) + "% free. ".

         ELSE DO:
      
            IF liSizePrev NE 0 AND liFreeExtents = 0 AND
             ((liSizePrev / ttBifo.tiSize) * 100) >= 5 THEN
               lcMessage = lcMessage + ttBifo.tcDB + "." + ttBifo.tcArea + 
               ": grows " + STRING((ttBifo.tiSize / liSizePrev) * 100) + "%. ".
               
         END.

      END.
      ELSE IF ldeFreePrev1 - ldeFree% >= 5 THEN
         lcMessage = lcMessage + ttBifo.tcDB + "." + ttBifo.tcArea + 
         ": grows " + STRING(INT(ldeFreePrev1 - ldeFree%)) + "%. ".

      PUT STREAM sArea UNFORMATTED lcAreaType AT 64.

      IF VALID-HANDLE(lhTableX) THEN DELETE OBJECT lhTableX.
      IF VALID-HANDLE(lhFieldX) THEN DELETE OBJECT lhFieldX.
      IF VALID-HANDLE(lhQueryX) THEN DELETE OBJECT lhQueryX.
      
   END.
   
   PUT STREAM sArea UNFORMATTED CHR(10).

   OUTPUT STREAM sArea CLOSE.

   IF VALID-HANDLE(lhTable)  THEN DELETE OBJECT lhTable.
   IF VALID-HANDLE(lhField)  THEN DELETE OBJECT lhField.
   IF VALID-HANDLE(lhQuery)  THEN DELETE OBJECT lhQuery.

END.

PROCEDURE pAreaReport:

   DEFINE VARIABLE liLoop1       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liLoop2       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDB          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lhTable       AS HANDLE    NO-UNDO.    
   DEFINE VARIABLE lhField       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lhQuery       AS HANDLE    NO-UNDO. 
   DEFINE VARIABLE lcQuery       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcField       AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lDEFree%      AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE liFreeB       AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE lcAreaName    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liHiWater     AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liTotBlocks   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcSpoolPath   AS CHARACTER NO-UNDO.

   lcSpoolPath = "/scratch/nagios/".

   OUTPUT STREAM sArea TO VALUE (lcSpoolPath + "areastatus_xfera").

   PUT STREAM sArea UNFORMATTED
      "Date and time " STRING(today,"99-99-9999") + " " + 
      STRING(time,"hh:mm:ss") skip(1)
                  
      "DataBase"    AT 1   
      "AreaName"    AT 12  
      "TotalBlocks" AT 36 
      "FreeBlocks"  AT 50 
      "Free% "      AT 65 skip.
   
   DO liLoop1 = 1 to NUM-DBS:

      lcDB = LDBNAME(liLoop1).

      CREATE QUERY lhQuery.
   
      CREATE BUFFER lhTable FOR TABLE lcDB + "._AreaStatus".

      lcQuery = "FOR EACH " + lcDB + "._AreaStatus".

      lhQuery:SET-BUFFERS(lhTable).
      lhQuery:QUERY-PREPARE(lcQuery).
      lhQuery:QUERY-OPEN.
      
      REPEAT:
      
         lhQuery:GET-NEXT(NO-LOCK).
      
         IF lhTable:AVAILABLE THEN DO liLoop2 = 1 TO lhTable:NUM-FIELDS:

            lhField = lhTable:BUFFER-FIELD(liLoop2).

            CASE lhField:NAME:
               WHEN "_AreaStatus-HiWater"    THEN
                  liHiWater    = lhField:BUFFER-VALUE.
               WHEN "_AreaStatus-TotBlocks"  THEN
                  liTotBlocks  = lhField:BUFFER-VALUE.
               WHEN "_AreaStatus-AreaName" THEN
                  lcAreaName   = lhField:BUFFER-VALUE.
            END.
         
         END.
   
         IF NOT lhTable:AVAILABLE THEN LEAVE.

         ASSIGN
            ldeFRee% = 100 - ((liHiWater / liTotBlocks) * 100)
            liFreeB  = liTotBlocks - liHiWater.

         PUT STREAM sArea UNFORMATTED
            UPPER(lcDB)                AT 1 
            lcAreaName                 AT 12 
            liTotBlocks                AT 36 
            liFreeB  format ">>>>>>>9" AT 50 
            ldeFree% format ">>9.99"   AT 65 
            CHR(10).
            
      END.
      
      IF VALID-HANDLE(lhTable)  THEN DELETE OBJECT lhTable.
      IF VALID-HANDLE(lhField)  THEN DELETE OBJECT lhField.
      IF VALID-HANDLE(lhQuery)  THEN DELETE OBJECT lhQuery.
   
   END.
   
   OUTPUT STREAM sArea CLOSE.

END PROCEDURE.
