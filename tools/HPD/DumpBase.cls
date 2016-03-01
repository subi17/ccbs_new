/*------------------------------------------------------------------------
    File        : DumpBase
    Purpose     : Base class for dumps
    Syntax      : 
    Description : High performance data file dump
    Author(s)   : tlamminmaki
    Created     : Tue Oct 28 14:35:00 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpHandler.

{HPD/HPDConst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpBase: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
   DEFINE PRIVATE VARIABLE lcNumericFormat         AS CHARACTER     NO-UNDO.
   DEFINE PRIVATE VARIABLE lcOriginalNumericFormat AS CHARACTER     NO-UNDO.

   DEFINE PUBLIC  VARIABLE lcDelimiter             AS CHARACTER     NO-UNDO.
   DEFINE PUBLIC  VARIABLE lcKeyDelimiter          AS CHARACTER     NO-UNDO.
   
   DEFINE PUBLIC  VARIABLE objHandler              AS CLASS DumpHandler NO-UNDO.
        
   DEFINE PROTECTED BUFFER lbDumpFile FOR DumpFile.
   
   DEFINE PRIVATE TEMP-TABLE ttDumpElement NO-UNDO
      FIELD OrderNum    AS INTEGER
      FIELD TableName   AS CHARACTER
      FIELD FieldName   AS CHARACTER
      FIELD ElemValue   AS CHARACTER
      FIELD ElemFormat  AS CHARACTER
      FIELD Delim       AS CHARACTER
      FIELD FieldHandle AS HANDLE
      FIELD ExtentPos   AS INTEGER
      FIELD DoQuote     AS LOGICAL
      INDEX OrderNum OrderNum.
   
	
   CONSTRUCTOR PUBLIC DumpBase
      ( iiDumpID    AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):

      objHandler = iobjHandler.

      FIND lbDumpFile NO-LOCK WHERE lbDumpFile.DumpID = iiDumpID NO-ERROR.
      
      IF NOT AVAILABLE lbDumpFile
      THEN RETURN ERROR NEW Progress.Lang.AppError("Dump ID " + STRING(iiDumpID) + " doesn't exist.", 1).
      
      lcOriginalNumericFormat = SESSION:NUMERIC-FORMAT.
      IF lbDumpFile.DecimalPoint = "." 
      THEN lcNumericFormat = "AMERICAN".
      ELSE lcNumericFormat = "EUROPEAN".

      SESSION:NUMERIC-FORMAT = lcNumericFormat.
      
      IF lbDumpFile.DumpDelimiter <> ""
      THEN lcDelimiter = ENTRY(1,lbDumpFile.DumpDelimiter).
      ELSE lcDelimiter = {&HPDDumpDelimiter}.
      IF NUM-ENTRIES(lbDumpFile.DumpDelimiter) = 2
      THEN lcKeyDelimiter = ENTRY(2,lbDumpFile.DumpDelimiter).
      ELSE lcKeyDelimiter = {&HPDKeyDelimiter}.

      mCreateDumpSchema().
      
   END CONSTRUCTOR.
   

   DESTRUCTOR PUBLIC DumpBase( ):

      IF lcOriginalNumericFormat <> ""
      THEN SESSION:NUMERIC-FORMAT = lcOriginalNumericFormat.

      EMPTY TEMP-TABLE ttDumpElement.
      
      IF AVAILABLE lbDumpFile
      THEN RELEASE lbDumpFile.       
            
   END DESTRUCTOR.


   METHOD PRIVATE INTEGER mGetExtentPos
      ( icFieldName AS CHARACTER ):

      DEFINE VARIABLE liReturnValue AS INTEGER NO-UNDO.

      IF NUM-ENTRIES(icFieldName, "[") = 2 AND
         NUM-ENTRIES(icFieldName, "]") = 2 AND
         ENTRY(2, icFieldName, "]") = ""
      THEN DO:

         liReturnValue = INTEGER(RIGHT-TRIM(SUBSTRING(icFieldName, R-INDEX(icFieldName, "[") + 1),"]")) NO-ERROR.

         IF ERROR-STATUS:ERROR = YES
         THEN DO:
            ERROR-STATUS:ERROR = NO.
            RETURN 0.
         END.

         RETURN liReturnValue.

      END.

      RETURN 0.

   END METHOD.


   METHOD PRIVATE CHARACTER mRemoveExtentPart
      ( icFieldName AS CHARACTER ):

      DEFINE VARIABLE liPosition AS INTEGER NO-UNDO.

      liPosition = R-INDEX(icFieldName,"[").

      IF liPosition = 0
      THEN RETURN icFieldName.

      RETURN SUBSTRING(icFieldName,1,liPosition - 1).

   END METHOD.
   
  
   METHOD PRIVATE VOID mCreateDumpSchema(): 

      DEFINE VARIABLE lcDFField AS CHARACTER NO-UNDO.
      DEFINE VARIABLE llQuote   AS LOGICAL   NO-UNDO.

      FOR EACH DFField OF lbDumpFile NO-LOCK WHERE
         DFField.ToDate   >= TODAY AND
         DFField.FromDate <= TODAY
         BY DFField.OrderNbr:

         IF DFField.DFField BEGINS "¤"
         THEN ASSIGN
                lcDFField = SUBSTRING(DFField.DFField, 2)
                llQuote   = YES
                .
         ELSE ASSIGN
                lcDFField = DFField.DFField
                llQuote   = NO
                .

         CREATE ttDumpElement.
         ASSIGN
            ttDumpElement.OrderNum   = DFField.OrderNbr + 5
            ttDumpElement.TableName  = DFField.DFTable WHEN ((NOT lcDFField BEGINS "@") AND
                                                             (NOT lcDFField BEGINS "#"))
            ttDumpElement.FieldName  = IF lcDFField BEGINS "@"
                                       THEN ""
                                       ELSE DFField.DFLabel
            ttDumpElement.ElemValue  = DFField.DFLabel WHEN lcDFField BEGINS "@"
            ttDumpElement.Delim      = lcDelimiter
            ttDumpElement.DoQuote    = llQuote
            .

         /*
            Store format for tablename.fieldname if it is specified.
            Example 1: TimeStart@HH:MM:SS
            Example 2: InvAmt@->>>>>.9<
            Example 3: DueDate@99.99.99
          */
         IF NUM-ENTRIES(ttDumpElement.FieldName,"@") > 1
         THEN ASSIGN
                 ttDumpElement.ElemFormat = ENTRY(2,ttDumpElement.FieldName,"@")
                 ttDumpElement.FieldName  = ENTRY(1,ttDumpElement.FieldName,"@").

         IF ttDumpElement.TableName <> "" 
         THEN DO:
            /* Take tablename from ttDumpElement.FieldName if it is there */
            IF NUM-ENTRIES(ttDumpElement.FieldName,".") = 2
            THEN ASSIGN
                     ttDumpElement.TableName = ENTRY(1,ttDumpElement.FieldName,".")
                     ttDumpElement.FieldName = ENTRY(2,ttDumpElement.FieldName,".")
                     .

            /* Check if there is an extent and store it */
            ttDumpElement.ExtentPos  = mGetExtentPos(ttDumpElement.FieldName).

            /* If there was an extent we need to remove the extent part of the
               fieldname */
            IF ttDumpElement.ExtentPos > 0
            THEN ttDumpElement.FieldName = mRemoveExtentPart(ttDumpElement.FieldName).

         END.                     
      END.

      FIND LAST ttDumpElement NO-ERROR.
      IF AVAILABLE ttDumpElement
      THEN ttDumpElement.Delim = "".

   END METHOD.


   METHOD PUBLIC VOID mUpdateDumpSchema
      (iiOrderNbr  AS INTEGER,
       icFieldName AS CHARACTER,
       icElemValue AS CHARACTER):

      FIND FIRST ttDumpElement WHERE
         ttDumpElement.OrderNum = iiOrderNbr
      NO-ERROR.

      IF NOT AVAILABLE ttDumpElement
      THEN CREATE ttDumpElement.

      ASSIGN
         ttDumpElement.OrderNum   = iiOrderNbr
         ttDumpElement.FieldName  = icFieldName
         ttDumpElement.ElemValue  = icElemValue
         ttDumpElement.Delim      = lcDelimiter
         .

   END METHOD.


   METHOD PUBLIC CHARACTER mGetConfigParam():

      IF NOT AVAILABLE lbDumpFile
      THEN RETURN "".

      RETURN lbDumpFile.ConfigParam.

   END METHOD.


   METHOD PUBLIC CHARACTER mUseTable 
      ( INPUT ihBufferHandle AS HANDLE ):

      IF NOT VALID-HANDLE(ihBufferHandle)
      THEN RETURN ERROR NEW Progress.Lang.AppError("Not valid buffer handle", 1).

      DEFINE VARIABLE lhBufferField AS HANDLE NO-UNDO.
      
      FOR EACH ttDumpElement WHERE
         ttDumpElement.TableName = ihBufferHandle:TABLE:

         ttDumpElement.FieldHandle = ?.
          
         lhBufferField = ihBufferHandle:BUFFER-FIELD(ttDumpElement.FieldName) NO-ERROR.
      
         IF NOT VALID-HANDLE(lhBufferField)
         THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot find field " +
                                                      ttDumpElement.FieldName + " in table " +
                                                      ihBufferHandle:TABLE, 1).
         
         ttDumpElement.FieldHandle = lhBufferField.
      END.
    
      RETURN "OK".

   END METHOD.
   

   METHOD PUBLIC CHARACTER mVerifyDumpFileData():

      FIND FIRST ttDumpElement WHERE
         ttDumpElement.TableName > "" AND
         ttDumpElement.FieldHandle = ?
      NO-ERROR.
      
      IF AVAILABLE ttDumpElement
      THEN RETURN ERROR NEW Progress.Lang.AppError("Cannot process all fields. For example " +
                                                   ttDumpElement.TableName + "." + ttDumpElement.FieldName, 1).
      
      FOR EACH ttDumpElement WHERE
         ttDumpElement.TableName = "" AND
         ttDumpElement.FieldName > "":
            
         IF objHandler:mSpecialHandling(ttDumpElement.FieldName, YES) <> "OK"
         THEN RETURN ERROR NEW
                 Progress.Lang.AppError("In table " + lbDumpFile.MainTable + 
                                        " special handling for field #" +
                                        ttDumpElement.FieldName +
                                        " is not defined.", 1).   
      END.
   
      RETURN "OK".
   
   END METHOD.
   

   /* 
      Method goes through temp-table ttDumpElement. It populates and returns
      one line.
      
      The element written in a line is determined from field ttDumpElement.FieldName:
      
      If ttDumpElement.FieldName is empty "" it indicates that
      we use directly a value ttDumpElement.ElemValue (that is DFField.DFLabel).
      
      If ttDumpElement.TableName is empty "" it indicates that
      we use a character value that is returned by method named mSpecialHandling
      inside object objHandler of class HPDHandler.
      
      The method uses a parameter that is a value specified in DDField.DFLabel
      and returns a value linked to this parameter value.
      
      Otherwise we find a value by using ttDumpElement.FieldHandle.
      
      The field handle is stored by calling method mUseTable. The method must be called
      for each needed default buffers. For example:
      
      mUseTable(BUFFER Invoice:HANDLE).
      mUseTable(BUFFER InvRow:HANDLE).
   */
   METHOD PUBLIC CHARACTER mGetData():

      DEFINE VARIABLE lcValue       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.         

      FOR EACH ttDumpElement:
         
         IF ttDumpElement.FieldName = ""
         THEN lcValue = ttDumpElement.ElemValue.
         
         ELSE IF ttDumpElement.TableName = ""
         THEN lcValue = objHandler:mSpecialHandling(ttDumpElement.FieldName, NO).
         
         ELSE DO:
   
            /* There is specific format that we need to use */
            IF ttDumpElement.ElemFormat <> ""
            THEN lcValue = TRIM(STRING(ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos), ttDumpElement.ElemFormat)) NO-ERROR.
            
            /* We are using default formats */
            ELSE CASE ttDumpElement.FieldHandle:DATA-TYPE:
                  
                  WHEN "CHARACTER"
                  THEN lcValue = ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos) NO-ERROR.
                  WHEN "DECIMAL"
                  THEN lcValue = TRIM(STRING(ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos),"->>>>>>>>>>>>>>>>>>>>>>9.9<<<<<<<<<")) NO-ERROR.
                  WHEN "INTEGER" OR WHEN "INT64"
                  THEN lcValue = TRIM(STRING(ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos),"->>>>>>>>>>>>>>>>>>>>>>9")) NO-ERROR.
                  WHEN "DATE" OR WHEN "DATETIME" OR WHEN "DATETIME-TZ"
                  THEN lcValue = ISO-DATE(ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos)) NO-ERROR.
                  WHEN "LOGICAL"
                  THEN lcValue = IF ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos) THEN "yes" ELSE "no" NO-ERROR.
                  OTHERWISE lcValue = STRING(ttDumpElement.FieldHandle:BUFFER-VALUE(ttDumpElement.ExtentPos)) NO-ERROR.
                  
            END CASE.
         END.

         IF ttDumpElement.DoQuote AND lcValue NE ""
         THEN lcValue = QUOTER(lcValue).

         lcReturnValue =  lcReturnValue + (IF lcValue = ? THEN "" ELSE lcValue) + ttDumpElement.Delim.
   
      END.
   
      RETURN lcReturnValue.

   END METHOD.   
   
END CLASS.
