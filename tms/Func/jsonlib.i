/*------------------------------------------------------------------------
    File        : jsonlib.i
    Purpose     : The modupe provides functions for making json messages.

    Syntax      :

    Description : 

    Author(s)   : ilsavola
    Created     : 1.12.2015
    Notes       : The initial version is very limited. Please update 
                  functionality here if you make json formatting functions. 
  ----------------------------------------------------------------------*/


/*Example of array format: "documents": [ "1", "5", "9" ]*/
/*How to make array:
1. Init fInitJsonArray("documents").
2. Add entries by using fAddToJsonArray(lcArray, "1").
3. Add array to struct*/

FUNCTION fInitJsonArray RETURNS CHAR
   (icArrayName AS CHAR):
   RETURN "~"" + icArrayName + "~"" + ":" + "[]".
END.

/*Note! The function is totally simple. It adds allways end ot the array.
No verifications implemented.*/
/*Function adds single char values to array*/
FUNCTION fAddToJsonArray RETURNS CHAR
   (INPUT-OUTPUT ocArray AS CHAR,
    INPUT icAddItem AS CHAR):
   DEF VAR liWritePos AS INT NO-UNDO.
   DEF VAR lcPart1 AS CHAR NO-UNDO.
   DEF VAR lcPart2 AS CHAR NO-UNDO.
   DEF VAR lcNewEntry AS CHAR NO-UNDO.

   liWritePos = R-INDEX(ocArray, "]").
   IF liWritePos EQ 0 THEN RETURN "Structure is broken: " + ocArray.
   lcPart1 = SUBSTRING(ocArray,1,liWritePos  - 1 ).
   lcPart2 = SUBSTRING(ocArray,liWritePos).
   IF lcPart1 MATCHES("*~"") THEN lcPart1 = lcPart1 + ",".
   lcNewEntry = "~"" + icAddItem +  "~"".

   ocArray = lcPart1 + lcNewEntry + lcPart2.

   RETURN "".
END.

/*Note! The function is totally simple. It adds allways end ot the array.
No verifications implemented.*/
/*Function adds objects {OBJECT_DATA} to array.*/
FUNCTION fObjectToJsonArray RETURNS CHAR
   (INPUT-OUTPUT ocArray AS CHAR,
    INPUT icAddItem AS CHAR):
   DEF VAR liWritePos AS INT NO-UNDO.
   DEF VAR lcPart1 AS CHAR NO-UNDO.
   DEF VAR lcPart2 AS CHAR NO-UNDO.
   DEF VAR lcNewEntry AS CHAR NO-UNDO.

   liWritePos = R-INDEX(ocArray, "]").
   IF liWritePos EQ 0 THEN RETURN "Structure is broken: " + ocArray.
   lcPart1 = SUBSTRING(ocArray,1,liWritePos  - 1 ).
   lcPart2 = SUBSTRING(ocArray,liWritePos).
   IF NOT lcPart1  MATCHES("*[") THEN lcPart1 = lcPart1 + ",".
   lcNewEntry =  icAddItem .

   ocArray = lcPart1 + lcNewEntry + lcPart2.

   RETURN "".
END.


