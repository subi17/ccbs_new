&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE VARIABLE ghXMLRoot       AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLFirst      AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLFieldName  AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLFieldValue AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLRecord     AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLText       AS HANDLE     NO-UNDO.
DEFINE VARIABLE ghXMLField      AS HANDLE     NO-UNDO.

/* CREATE X-DOCUMENT ghXMLRoot. */
CREATE X-NODEREF  ghXMLFirst.
CREATE X-NODEREF  ghXMLRecord.
CREATE X-NODEREF  ghXMLFieldName.
CREATE X-NODEREF  ghXMLFieldValue.
CREATE X-NODEREF  ghXMLText.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLBuffer2Node Include 
FUNCTION StarXMLBuffer2Node RETURNS HANDLE
  ( ihDocument      AS HANDLE     ,
    ihRoot          AS HANDLE     ,
    ihBuffer        AS HANDLE     ,
    icElement       AS CHARACTER  ,
    icFields        AS CHARACTER  ,
    icNames         AS CHARACTER  ,
    icAttributes    AS CHARACTER  ,
    icANames        AS CHARACTER  ,
    icCaps          AS CHARACTER  ,
    ilEmpty         AS LOGICAL    ,
    ilFormat        AS LOGICAL
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLCreateNode Include 
FUNCTION StarXMLCreateNode RETURNS HANDLE
  (   ihDoc  AS HANDLE,
      ihRoot AS HANDLE,
      icDocument AS CHARACTER 
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLFindNode Include 
FUNCTION StarXMLFindNode RETURNS HANDLE
  ( ihRoot   AS HANDLE,
    ihNode   AS HANDLE,
    icElement AS CHARACTER,
    icFields AS CHARACTER,
    icValues AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLGetChilds Include 
FUNCTION StarXMLGetChilds RETURNS CHARACTER
  ( ihRoot   AS HANDLE
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLGetFirstElement Include 
FUNCTION StarXMLGetFirstElement RETURNS HANDLE
  ( ihRoot AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLGetNodes Include 
FUNCTION StarXMLGetNodes RETURNS CHARACTER
  ( ihRoot   AS HANDLE,
    icNodeName AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLOpenFromFile Include 
FUNCTION StarXMLOpenFromFile RETURNS HANDLE
  ( icFile AS CHARACTER,
    ilDTDCheck AS LOGICAL 
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLSaveToFile Include 
FUNCTION StarXMLSaveToFile RETURNS LOGICAL
  (   ihDocument AS HANDLE,
      icTablename AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLSaveToMemptr Include 
FUNCTION StarXMLSaveToMemptr RETURNS LOGICAL
  (   ihDocument AS HANDLE,
      iMemptr AS MEMPTR
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StarXMLString2Variable Include 
FUNCTION StarXMLString2Variable RETURNS LOGICAL
  ( ihField AS HANDLE,      /* should be buffer-field handle */
    icValue AS CHARACTER
  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 26.19
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarXMLClean Include 
PROCEDURE StarXMLClean PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DELETE OBJECT ghXMLRoot         NO-ERROR.
    DELETE OBJECT ghXMLFirst        NO-ERROR.
    DELETE OBJECT ghXMLRecord       NO-ERROR.
    DELETE OBJECT ghXMLFieldName    NO-ERROR.
    DELETE OBJECT ghXMLFieldValue   NO-ERROR.
    DELETE OBJECT ghXMLText         NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarXMLCreateDocument Include 
PROCEDURE StarXMLCreateDocument :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  remember create noderefs  
    
    CREATE X-DOCUMENT ihDocument.
    CREATE X-NODEREF  ihRoot.

------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER icDocument  AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER ihDocument  AS HANDLE     NO-UNDO.
    DEFINE INPUT PARAMETER ihRoot      AS HANDLE     NO-UNDO.

    /*Create de Root element with table name as element name*/
    ihDocument:CREATE-NODE(ihRoot, icDocument, "ELEMENT":U).
    ihDocument:APPEND-CHILD(ihRoot).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarXMLCreateDTDDocument Include 
PROCEDURE StarXMLCreateDTDDocument :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
  remember create noderefs  
    
    CREATE X-RootNodeName ihDocument.
    CREATE X-NODEREF  ihRoot.
    
    
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER icNameSpaceUri  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icRootNodeName  AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icPublicId      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icSystemId      AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ihDocument      AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER ihRoot          AS HANDLE     NO-UNDO.

    ihDocument:INITIALIZE-DOCUMENT-TYPE
        (   icNameSpaceUri,
            icRootNodeName,
            icPublicId, 
            icSystemId
         ).
    ihDocument:GET-DOCUMENT-ELEMENT(ihRoot).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarXMLNode2Table Include 
PROCEDURE StarXMLNode2Table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihNode          AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER icElement       AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ihTable         AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER icForeingKeys   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icForeingValues AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icExcept        AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icConvertFrom   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icConvertTo     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ilLabel         AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER icCaps          AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lhBuffer AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lij AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcFields AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lhh AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcd AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE liNode AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lhNode AS HANDLE     NO-UNDO.

    DEFINE VARIABLE lhName  AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhValue AS HANDLE     NO-UNDO.

    DEFINE VARIABLE liRecordCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lhRecordCount AS HANDLE     NO-UNDO.

    CREATE X-NODEREF lhNode.
    CREATE X-NODEREF lhName.
    CREATE X-NODEREF lhValue.

    lhBuffer = ihTable:DEFAULT-BUFFER-HANDLE.
    lhRecordCount = lhBuffer:BUFFER-FIELD('XMLNodeRecordNumber') NO-ERROR.

    /* name conversion */
    DO lii = 1 TO lhBuffer:NUM-FIELDS:
        ASSIGN
            lhh = lhBuffer:BUFFER-FIELD(lii)
            lcc = lhh:NAME
            lcd = (IF ilLabel THEN lhh:LABEL ELSE lcc)
            lij = LOOKUP(lcd,icConvertFrom)  
            lcd = (IF lij = 0 
                   THEN lcd
                   ELSE ENTRY(lij,icConvertTo)
                  )
            lcd = ( IF icCaps = "CAPS"
                    THEN CAPS(lcd)
                    ELSE
                    IF icCaps = "LC"
                    THEN LC(lcd)
                    ELSE lcd
                  ).
        IF LOOKUP(lcd,icExcept) > 0 THEN NEXT.
        ASSIGN
            lcFields = lcFields + lcc + ","
            lcLabels = lcLabels + lcd + ",".
    END.
    ASSIGN
        lcFields = TRIM(lcFields,",")
        lcLabels = TRIM(lcLabels,",").

    DO liNode = 1 TO ihNode:NUM-CHILDREN:

        IF NOT ihNode:GET-CHILD(lhNode,liNode) THEN NEXT.

        IF lhNode:NAME <> icElement THEN NEXT.

        IF lhNode:SUBTYPE = "ELEMENT" THEN
        DO:

            /* create buffer */
            lhBuffer:BUFFER-CREATE().
            liRecordCount = liRecordCount + 1.

            /* foreing keys, NOTE: no label name conversion */
            DO lii = 1 TO NUM-ENTRIES(icForeingKeys):
                lhh = lhBuffer:BUFFER-FIELD(ENTRY(lii,icForeingKeys)) NO-ERROR.
                IF VALID-HANDLE(lhh) 
                THEN StarXMLString2Variable(lhh,ENTRY(lii,icForeingValues,CHR(1))).
            END.

            /* take attributes */
            lcc = lhNode:ATTRIBUTE-NAMES.
            DO lii = 1 TO NUM-ENTRIES(lcc):
                lij = LOOKUP(ENTRY(lii,lcc),lcLabels).
                IF lij = 0 THEN NEXT.
                ASSIGN 
                    lcd = ENTRY(lij,lcFields)
                    lhh = lhBuffer:BUFFER-FIELD(lcd) 
                    NO-ERROR.
                IF VALID-HANDLE(lhh) 
                THEN DO:
                    IF LOOKUP(lcd,icExcept) > 0 THEN NEXT.
                    StarXMLString2Variable(lhh,lhNode:GET-ATTRIBUTE(ENTRY(lii,lcc))).
                END.
            END.

            /* take normal nodes */
            DO lii = 1 TO lhNode:NUM-CHILDREN:
                IF NOT lhNode:GET-CHILD(lhName,lii) THEN NEXT.
                IF LOOKUP(lhName:NAME,icExcept) > 0 THEN NEXT.
                IF lhName:NUM-CHILDREN = 0 THEN NEXT.
                lhName:GET-CHILD(lhValue,1).
                IF lhValue:SUBTYPE <> "ELEMENT" 
                THEN DO: 
                    lij = LOOKUP(lhName:NAME,lcLabels).
                    IF lij = 0 THEN NEXT.
                    lcd = ENTRY(lij,lcFields).
                    lhh = lhBuffer:BUFFER-FIELD(lcd) NO-ERROR.
                    IF VALID-HANDLE(lhh) 
                    THEN StarXMLString2Variable(lhh,lhValue:NODE-VALUE).
                END.
            END.              

            /* special fields */
            IF VALID-HANDLE(lhRecordCount) 
            THEN lhRecordCount:BUFFER-VALUE = liRecordCount.

        END.

    END.

    DELETE OBJECT lhNode    NO-ERROR.
    DELETE OBJECT lhName    NO-ERROR.
    DELETE OBJECT lhValue   NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StarXMLQuery2Node Include 
PROCEDURE StarXMLQuery2Node :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT  PARAMETER ihDocument      AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER ihRoot          AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER ihQuery         AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER ihBuffer        AS HANDLE     NO-UNDO.
    DEFINE INPUT  PARAMETER icElement       AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icExcept        AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icConvertFrom   AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icConvertTo     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER icKeys          AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ilLabel         AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER icCaps          AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ilEmpty         AS LOGICAL    NO-UNDO.
    DEFINE INPUT  PARAMETER ilFormat        AS LOGICAL    NO-UNDO.

    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lij AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcFieldName AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE liExtent AS INTEGER    NO-UNDO.
    
    DEFINE VARIABLE lhField  AS HANDLE     NO-UNDO.

    DEFINE VARIABLE lhRecord AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhFieldName AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhFieldValue AS HANDLE     NO-UNDO.

    CREATE X-NODEREF lhRecord.
    CREATE X-NODEREF lhFieldName.
    CREATE X-NODEREF lhFieldValue.

    ihQuery:GET-FIRST().             

    IF NOT ihQuery:QUERY-OFF-END 
    THEN REPEAT:

        /*Create a 'Record' element.*/
        ihDocument:CREATE-NODE(lhRecord, icElement,"ELEMENT":U).
        ihRoot:APPEND-CHILD(lhRecord).

        ihQuery:PRIVATE-DATA = 
            ( IF ihQuery:PRIVATE-DATA = ?
              THEN ""
              ELSE ihQuery:PRIVATE-DATA
            )
            + STRING(lhRecord) + ",".

        /* KeyValues like attributes */
        DO lii = 1 TO NUM-ENTRIES(icKeys):
            lhField = ihBuffer:BUFFER-FIELD(ENTRY(lii,ickeys)).
            /* xml-name ? */
            ASSIGN
                lcFieldName = ENTRY(lii,icKeys)
                lij         = ( IF icConvertFrom = ""
                                THEN 0
                                ELSE LOOKUP(lcFieldName,icConvertFrom)
                              )
                lcFieldName = REPLACE
                              ( ( IF lij = 0 
                                  THEN ( IF ilLabel 
                                         THEN lhField:LABEL 
                                         ELSE lcFieldName
                                       ) 
                                  ELSE ENTRY(lij,icConvertTo)
                                ),
                                " ",
                                "_"
                              )
                lcFieldName = ( IF icCaps = "CAPS"
                                THEN CAPS(lcFieldName)
                                ELSE
                                IF icCaps = "LC"
                                THEN LC(lcFieldName)
                                ELSE lcFieldName
                              ).
            lhRecord:SET-ATTRIBUTE
                ( lcFieldName,
                  TRIM(IF lhField:STRING-VALUE = ? THEN "" ELSE lhField:STRING-VALUE)
                ).
        END.

        DO lii = 1 TO ihBuffer:NUM-FIELDS:

            lhField = ihBuffer:BUFFER-FIELD(lii).

            DO liExtent =
                ( IF lhField:EXTENT = 0 THEN 0 ELSE 1 )
                TO
                ( IF lhField:EXTENT = 0 THEN 0 ELSE lhField:EXTENT):
                           
                /* db fieldname + extent */
                lcFieldName = lhField:NAME + ( IF liExtent = 0 THEN "" ELSE "." + STRING(liExtent) ).

                IF LOOKUP(lcFieldName,icKeys) > 0 THEN NEXT. /*key field*/
                IF LOOKUP(lcFieldName,icExcept) > 0 THEN NEXT. /*except field*/
                IF lcFieldname BEGINS "XML" THEN NEXT.

                /*In order to improve performance decrease the .xml file size
                 the fields with a empty value will be not added as element*/
                IF ilEmpty THEN
                DO:                   
                    IF lhField:BUFFER-VALUE[liExtent] = ? THEN NEXT.
                    CASE lhField:DATA-TYPE:
                        WHEN "CHARACTER" THEN IF lhField:BUFFER-VALUE[liExtent] = "" THEN NEXT.
                        WHEN "INTEGER"   THEN IF lhField:BUFFER-VALUE[liExtent] = 0  THEN NEXT.
                        WHEN "DECIMAL"   THEN IF lhField:BUFFER-VALUE[liExtent] = 0  THEN NEXT.    
                        WHEN "DATE"      THEN IF lhField:BUFFER-VALUE[liExtent] = ?  THEN NEXT.
                    END CASE.
                END.
    
                /* xml-name ? */
                ASSIGN
                    lij         = ( IF icConvertFrom = ""
                                    THEN 0
                                    ELSE LOOKUP(lcFieldName,icConvertFrom)
                                  )
                    lcFieldName = REPLACE
                                  ( ( IF lij = 0 
                                      THEN ( IF ilLabel 
                                             THEN lhField:LABEL + ( IF liExtent = 0 THEN "" ELSE "." + STRING(liExtent) )
                                             ELSE lcFieldName
                                           ) 
                                      ELSE ENTRY(lij,icConvertTo)
                                    ),
                                    " ",
                                    "_"
                                  )
                    lcFieldName = ( IF icCaps = "CAPS"
                                    THEN CAPS(lcFieldName)
                                    ELSE
                                    IF icCaps = "LC"
                                    THEN LC(lcFieldName)
                                    ELSE lcFieldName
                                  ).
                    
                /*Create the field name as element*/
                ihDocument:CREATE-NODE(lhFieldName,lcFieldName,'ELEMENT':U).
                lhRecord:APPEND-CHILD(lhFieldName).
                
                /*Create the field value as text*/
                ihDocument:CREATE-NODE(lhFieldValue, "text":U, 'TEXT':U).
                lhFieldName:APPEND-CHILD(lhFieldValue).
                lcc = lhField:BUFFER-VALUE[liExtent].
                lhFieldValue:NODE-VALUE = ( IF lcc = ?
                                            THEN ""
                                            ELSE 
                                                (   IF ilFormat
                                                    THEN TRIM(STRING(lhField:BUFFER-VALUE[liExtent],lhField:FORMAT))
                                                    ELSE lhField:BUFFER-VALUE[liExtent]
                                                )

                                          ).
    

            END.
        
        END. 

        ihQuery:GET-NEXT().
        IF ihQuery:QUERY-OFF-END THEN LEAVE.

    END.

    DELETE OBJECT lhRecord      NO-ERROR.
    DELETE OBJECT lhFieldName   NO-ERROR.
    DELETE OBJECT lhFieldValue  NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLBuffer2Node Include 
FUNCTION StarXMLBuffer2Node RETURNS HANDLE
  ( ihDocument      AS HANDLE     ,
    ihRoot          AS HANDLE     ,
    ihBuffer        AS HANDLE     ,
    icElement       AS CHARACTER  ,
    icFields        AS CHARACTER  ,
    icNames         AS CHARACTER  ,
    icAttributes    AS CHARACTER  ,
    icANames        AS CHARACTER  ,
    icCaps          AS CHARACTER  ,
    ilEmpty         AS LOGICAL    ,
    ilFormat        AS LOGICAL
  ) :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lij AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcFieldName AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE liExtent AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcFormat  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE lcFormats AS CHARACTER  NO-UNDO.

    DEFINE VARIABLE lhField  AS HANDLE     NO-UNDO.

    DEFINE VARIABLE lhRecord AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhFieldName AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhFieldValue AS HANDLE     NO-UNDO.

    CREATE X-NODEREF lhRecord.
    CREATE X-NODEREF lhFieldName.
    CREATE X-NODEREF lhFieldValue.

    IF icFields = '' THEN 
    DO lii = 1 TO ihBuffer:NUM-FIELDS:

        lhField = ihBuffer:BUFFER-FIELD(lii).

        DO liExtent =
            ( IF lhField:EXTENT = 0 THEN 0 ELSE 1 )
            TO
            ( IF lhField:EXTENT = 0 THEN 0 ELSE lhField:EXTENT):
                       
            /* db fieldname + extent */
            icFields    = icFields 
                        + ',' 
                        + lhField:NAME + ( IF liExtent = 0 THEN "" ELSE "#" + STRING(liExtent) )
                        + '¤'
                        + lhField:FORMAT.

        END.

    END.
    icFields = TRIM(icFields,',').

    ASSIGN
        icNames     = icNames   + FILL(",",NUM-ENTRIES(icFields))
        icANames    = icANames  + FILL(",",NUM-ENTRIES(icAttributes) + 1) /* + 1, if no attributes */
        lcFormats   = lcFormats + FILL(",",NUM-ENTRIES(icFields))
        .

    DO lii = 1 TO NUM-ENTRIES(icFields):
        lcc = ENTRY(lii,icFields).
        IF NUM-ENTRIES(lcc,"¤") > 1
        THEN ASSIGN
            ENTRY(lii,icFields)     = ENTRY(1,lcc,"¤")
            ENTRY(lii,lcFormats)    = ENTRY(2,lcc,"¤").
        IF ENTRY(lii,icNames) = '' 
        THEN ENTRY(lii,icNames) = REPLACE(ENTRY(lii,icFields),'#','.').
    END.               

    /*Create a 'Record' element.*/
    ihDocument:CREATE-NODE(lhRecord, icElement,"ELEMENT":U).
    ihRoot:APPEND-CHILD(lhRecord).

    /*  attributes */
    DO lii = 1 TO NUM-ENTRIES(icAttributes):
        lhField = ihBuffer:BUFFER-FIELD(ENTRY(lii,icAttributes)).
        /* xml-name ? */
        ASSIGN
            lcFieldName = ( IF ENTRY(lii,icANames) = ""
                            THEN ENTRY(lii,icAttributes)
                            ELSE ENTRY(lii,icANames)
                          )
            lcFieldName = ( IF icCaps = "CAPS"
                            THEN CAPS(lcFieldName)
                            ELSE
                            IF icCaps = "LC"
                            THEN LC(lcFieldName)
                            ELSE lcFieldName
                          ).
        lhRecord:SET-ATTRIBUTE
            ( lcFieldName,
               IF lhField:BUFFER-VALUE[liExtent] = ?
                                    THEN ""
                                    ELSE lhField:BUFFER-VALUE[liExtent]
            ).
    END.

    DO lii = 1 TO NUM-ENTRIES(icFields):

        ASSIGN 
            lcFieldName     = ENTRY(lii,icFields)
            lhField         = ihBuffer:BUFFER-FIELD(ENTRY(1,lcFieldName,"#"))
            liExtent        = ( IF NUM-ENTRIES(lcFieldname,"#") = 1
                                THEN 0
                                ELSE INTEGER(ENTRY(2,lcFieldname,"#"))
                              )
            lcFieldName     = ( IF ENTRY(lii,icNames) = ""
                                THEN ( IF NUM-ENTRIES(lcFieldname,"#") = 1
                                       THEN lcFieldName
                                       ELSE ENTRY(1,lcFieldName,'#') + '.' + STRING(liExtent)
                                     )
                                ELSE ENTRY(lii,icNames)
                              )
            lcFormat        = ENTRY(lii,lcFormats)
            lcFieldName = ( IF icCaps = "CAPS"
                THEN CAPS(lcFieldName)
                ELSE
                IF icCaps = "LC"
                THEN LC(lcFieldName)
                ELSE lcFieldName
              ).

        /*In order to improve performance decrease the .xml file size
         the fields with a empty value will be not added as element*/
        IF ilEmpty THEN
        DO:                   
            IF lhField:BUFFER-VALUE[liExtent] = ? THEN NEXT.
            CASE lhField:DATA-TYPE:
                WHEN "CHARACTER" THEN IF lhField:BUFFER-VALUE[liExtent] = "" THEN NEXT.
                WHEN "INTEGER"   THEN IF lhField:BUFFER-VALUE[liExtent] = 0  THEN NEXT.
                WHEN "DECIMAL"   THEN IF lhField:BUFFER-VALUE[liExtent] = 0  THEN NEXT.    
                WHEN "DATE"      THEN IF lhField:BUFFER-VALUE[liExtent] = ?  THEN NEXT.
            END CASE.
        END.
                
        /*Create the field name as element*/
        ihDocument:CREATE-NODE(lhFieldName,lcFieldName,'ELEMENT':U).
        lhRecord:APPEND-CHILD(lhFieldName).
        
        /*Create the field value as text*/
        ihDocument:CREATE-NODE(lhFieldValue, "text":U, 'TEXT':U).
        lhFieldName:APPEND-CHILD(lhFieldValue).

        lhFieldValue:NODE-VALUE = ( IF lhField:BUFFER-VALUE[liExtent] = ?
                                    THEN ""
                                    ELSE 
                                    ( IF ilFormat AND lcFormat <> ''
                                      THEN TRIM(STRING(lhField:BUFFER-VALUE[liExtent],lcFormat))
                                      ELSE lhField:BUFFER-VALUE[liExtent]
                                     )
                                  ).

    END.

    DELETE OBJECT lhFieldName   NO-ERROR.
    DELETE OBJECT lhFieldValue  NO-ERROR.

    RETURN lhRecord.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLCreateNode Include 
FUNCTION StarXMLCreateNode RETURNS HANDLE
  (   ihDoc  AS HANDLE,
      ihRoot AS HANDLE,
      icDocument AS CHARACTER 
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lhNode AS HANDLE     NO-UNDO.
    
    CREATE X-NODEREF lhNode.

    ihDoc:CREATE-NODE(lhNode, icDocument, "ELEMENT":U).
    ihRoot:APPEND-CHILD(lhNode).

    RETURN lhNode.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLFindNode Include 
FUNCTION StarXMLFindNode RETURNS HANDLE
  ( ihRoot   AS HANDLE,
    ihNode   AS HANDLE,
    icElement AS CHARACTER,
    icFields AS CHARACTER,
    icValues AS CHARACTER
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE liRecord AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    
    LOOP:
    DO liRecord = 1 TO ihRoot:NUM-CHILDREN:
        IF NOT ihRoot:GET-CHILD(ihNode,liRecord) THEN LEAVE.
        IF ihNode:SUBTYPE <> 'ELEMENT' THEN NEXT.
        IF ihNode:NAME <> icElement THEN NEXT.
        DO lii = 1 TO NUM-ENTRIES(icFields):
            IF ihNode:GET-ATTRIBUTE(ENTRY(lii,icFields)) <> 
                ENTRY(lii,icValues,CHR(1)) 
            THEN NEXT LOOP.
        END.
/*         DELETE OBJECT ihNode. */
        RETURN ihNode.
    END.

    DELETE OBJECT ihNode.
    RETURN ?.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLGetChilds Include 
FUNCTION StarXMLGetChilds RETURNS CHARACTER
  ( ihRoot   AS HANDLE
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lhNode AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhValue AS HANDLE     NO-UNDO.

    DEFINE VARIABLE liRecord AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE lcResponce AS CHARACTER  NO-UNDO.
    
    CREATE X-NODEREF lhNode.
    CREATE X-NODEREF lhValue.

    DO liRecord = 1 TO ihRoot:NUM-CHILDREN:
        IF NOT ihRoot:GET-CHILD(lhNode,liRecord) THEN LEAVE.
        lhNode:GET-CHILD(lhValue,1).
        IF lhValue:SUBTYPE <> "ELEMENT" 
        THEN DO: 
            lcResponce  = lcResponce 
                        + CHR(7) 
                        + lhNode:NAME 
                        + "="
                        + lhValue:NODE-VALUE.
        END.
    END.

    DELETE OBJECT lhNode.
    DELETE OBJECT lhValue.
  
    RETURN LEFT-TRIM
        (   lcResponce,
            CHR(7)
        ).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLGetFirstElement Include 
FUNCTION StarXMLGetFirstElement RETURNS HANDLE
  ( ihRoot AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lhNode AS HANDLE     NO-UNDO.

    CREATE X-NODEREF lhNode.

    ihRoot:GET-DOCUMENT-ELEMENT(lhNode).

    RETURN lhNode.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLGetNodes Include 
FUNCTION StarXMLGetNodes RETURNS CHARACTER
  ( ihRoot   AS HANDLE,
    icNodeName AS CHARACTER
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lhNode AS HANDLE     NO-UNDO.
    DEFINE VARIABLE lhValue AS HANDLE     NO-UNDO.

    DEFINE VARIABLE liRecord AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lii AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE lcResponce AS CHARACTER  NO-UNDO.
    
    CREATE X-NODEREF lhNode.
    CREATE X-NODEREF lhValue.

    DO liRecord = 1 TO ihRoot:NUM-CHILDREN:
        IF NOT ihRoot:GET-CHILD(lhNode,liRecord) THEN LEAVE.
        IF lhNode:NAME <> icNodeName THEN NEXT.
        lhNode:GET-CHILD(lhValue,1).
        IF lhValue:SUBTYPE = "ELEMENT" 
        THEN DO: 
            ASSIGN
                lcc         = lhNode:ATTRIBUTE-NAMES
                lcResponce  = lcResponce 
                            + CHR(1) 
                            + lcc 
                            + "=".
            DO lii = 1 TO NUM-ENTRIES(lcc):
                lcResponce = lcResponce
                           + ( IF lii = 1 
                               THEN ""
                               ELSE ","
                             )
                           + TRIM(lhNode:GET-ATTRIBUTE(ENTRY(lii,lcc)))
                           .
            END.
        END.
    END.

    DELETE OBJECT lhNode.
    DELETE OBJECT lhValue.
  
    RETURN LEFT-TRIM
        (   lcResponce,
            CHR(1)
        ).  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLOpenFromFile Include 
FUNCTION StarXMLOpenFromFile RETURNS HANDLE
  ( icFile AS CHARACTER,
    ilDTDCheck AS LOGICAL 
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE lhDocument AS HANDLE     NO-UNDO.
    
    CREATE X-DOCUMENT lhDocument.

    lhDocument:LOAD("file",icFile,ilDTDCheck).
    
    RETURN lhDocument.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLSaveToFile Include 
FUNCTION StarXMLSaveToFile RETURNS LOGICAL
  (   ihDocument AS HANDLE,
      icTablename AS CHARACTER
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    RETURN ihDocument:SAVE("FILE":U, icTablename).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLSaveToMemptr Include 
FUNCTION StarXMLSaveToMemptr RETURNS LOGICAL
  (   ihDocument AS HANDLE,
      iMemptr AS MEMPTR
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lcc AS CHARACTER  NO-UNDO.

    ASSIGN
        SET-SIZE(iMemptr)   = 0
        SET-SIZE(iMemptr)   = 16384.

    ihDocument:SAVE("MEMPTR",iMemptr).

    lcc = GET-STRING(iMemptr,1).

    MESSAGE 
        LENGTH(lcc) SKIP
        GET-SIZE(iMemptr) SKIP
        lcc
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StarXMLString2Variable Include 
FUNCTION StarXMLString2Variable RETURNS LOGICAL
  ( ihField AS HANDLE,      /* should be buffer-field handle */
    icValue AS CHARACTER
  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    IF icValue = "<Null>" THEN icValue = ?.

    CASE ihField:DATA-TYPE:
        WHEN "CHARACTER" THEN ihField:BUFFER-VALUE = RIGHT-TRIM(icValue).
        WHEN "INTEGER"   THEN ihField:BUFFER-VALUE = INTEGER(icValue).
        WHEN "DECIMAL"   THEN ihField:BUFFER-VALUE = DECIMAL(icValue).
        WHEN "LOGICAL"   THEN ihField:BUFFER-VALUE = (TRIM(icValue) = "YES").
        WHEN "DATE"      THEN ihField:BUFFER-VALUE = DATE(icValue).
    END CASE.                                        
  
    RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

