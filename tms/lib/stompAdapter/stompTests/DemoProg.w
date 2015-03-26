&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

USING javax.jms.*.
ROUTINE-LEVEL ON ERROR UNDO, THROW.
/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{javax/jms/Session.i}

/* Parameters Definitions ---                                           */


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cQueueFile AS CHARACTER   NO-UNDO INIT "http://mqhost:8161/admin/xml/queues.jsp".
DEFINE VARIABLE cAction AS CHARACTER   NO-UNDO.
DEFINE VARIABLE rsMessageType AS CHARACTER   NO-UNDO.

/* Temp-tables used for ready queue's used in ActiveMQ */
DEFINE TEMP-TABLE Queues NO-UNDO
    FIELD NAME AS CHARACTER.

DEFINE TEMP-TABLE Queue NO-UNDO
  FIELD NAME AS CHARACTER 
  .

DEFINE DATASET dsQueue FOR Queues, Queue.

/* Temp-table used for defining Message properties */
DEFINE TEMP-TABLE ttProperty NO-UNDO
  FIELD PropName   AS CHARACTER  FORMAT 'X(30)' COLUMN-LABEL "Property"  
  FIELD PropValue  AS CHARACTER  FORMAT 'X(30)' COLUMN-LABEL "Value"     
  FIELD PropFile   AS BLOB 
  .

ASSIGN cAction = "{&ACTION}".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brProperty

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttProperty

/* Definitions for BROWSE brProperty                                    */
&Scoped-define FIELDS-IN-QUERY-brProperty ttProperty.PropName ttProperty.PropValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brProperty ttProperty.PropName ttProperty.PropValue   
&Scoped-define ENABLED-TABLES-IN-QUERY-brProperty ttProperty
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brProperty ttProperty
&Scoped-define SELF-NAME brProperty
&Scoped-define QUERY-STRING-brProperty FOR EACH ttProperty BY ttProperty.PropName
&Scoped-define OPEN-QUERY-brProperty OPEN QUERY {&SELF-NAME} FOR EACH ttProperty BY ttProperty.PropName.
&Scoped-define TABLES-IN-QUERY-brProperty ttProperty
&Scoped-define FIRST-TABLE-IN-QUERY-brProperty ttProperty


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brProperty}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnReceive edText brProperty 
&Scoped-Define DISPLAYED-OBJECTS edText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDelProp 
     LABEL "&Delete Properties" 
     SIZE 19 BY 1.14 TOOLTIP "Delete Properties".

DEFINE BUTTON btnProperty 
     LABEL "&Add Property" 
     SIZE 15 BY 1.14 TOOLTIP "Add Property".

DEFINE BUTTON btnReceive 
     IMAGE-UP FILE "receive.bmp":U
     LABEL "&Receive" 
     SIZE 14 BY 1.43.

DEFINE BUTTON btnSend 
     IMAGE-UP FILE "send.bmp":U
     LABEL "&Send" 
     SIZE 14 BY 1.43 TOOLTIP "Send Message to Queue".

DEFINE VARIABLE cbQueues AS CHARACTER FORMAT "X(256)":U 
     LABEL "Queues" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 TOOLTIP "List of ActiveMQ Queues" NO-UNDO.

DEFINE VARIABLE edText AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 70.8 BY 2.38 TOOLTIP "Type here your message" NO-UNDO.

DEFINE VARIABLE flQueue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Queue" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tlPersistent AS LOGICAL INITIAL no 
     LABEL "Persistent" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 TOOLTIP "Is the message persistent?" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brProperty FOR 
      ttProperty SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brProperty C-Win _FREEFORM
  QUERY brProperty DISPLAY
      ttProperty.PropName  
      ttProperty.PropValue

      ENABLE ttProperty.PropName  ttProperty.PropValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS NO-VALIDATE DROP-TARGET SIZE 70.8 BY 11.91 FIT-LAST-COLUMN TOOLTIP "Add properties to your message".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     flQueue AT ROW 1.67 COL 18 COLON-ALIGNED WIDGET-ID 26
     cbQueues AT ROW 1.62 COL 18 COLON-ALIGNED WIDGET-ID 4
     btnReceive AT ROW 1.48 COL 55.8 WIDGET-ID 20
     tlPersistent AT ROW 2.76 COL 20 WIDGET-ID 12
     edText AT ROW 3.71 COL 19.8 NO-LABEL WIDGET-ID 6
     brProperty AT ROW 7.95 COL 19.8 WIDGET-ID 200
     btnSend AT ROW 20.29 COL 20 WIDGET-ID 14
     btnProperty AT ROW 6.57 COL 20 WIDGET-ID 22
     btnDelProp AT ROW 6.57 COL 38.2 WIDGET-ID 24
     "Your message:" VIEW-AS TEXT
          SIZE 14.6 BY .62 AT ROW 3.67 COL 5 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.4 BY 21.19 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Demo Stomp Adapter"
         HEIGHT             = 21.19
         WIDTH              = 95.4
         MAX-HEIGHT         = 31.33
         MAX-WIDTH          = 148.2
         VIRTUAL-HEIGHT     = 31.33
         VIRTUAL-WIDTH      = 148.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 2
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brProperty edText DEFAULT-FRAME */
ASSIGN 
       brProperty:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR BUTTON btnDelProp IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnDelProp:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnProperty IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnProperty:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       btnReceive:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSend IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnSend:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX cbQueues IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cbQueues:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN flQueue IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       flQueue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tlPersistent IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tlPersistent:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brProperty
/* Query rebuild information for BROWSE brProperty
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttProperty BY ttProperty.PropName.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brProperty */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Demo Stomp Adapter */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Demo Stomp Adapter */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brProperty
&Scoped-define SELF-NAME brProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brProperty C-Win
ON DROP-FILE-NOTIFY OF brProperty IN FRAME DEFAULT-FRAME
DO:
  RUN FileDropped.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brProperty C-Win
ON MOUSE-SELECT-DBLCLICK OF brProperty IN FRAME DEFAULT-FRAME
DO:
  RUN OpenFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brProperty C-Win
ON ROW-ENTRY OF brProperty IN FRAME DEFAULT-FRAME
DO:
  IF AVAILABLE ttProperty
  AND ttProperty.PropName = "FileName" THEN
    RUN OpenFile.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brProperty C-Win
ON ROW-LEAVE OF brProperty IN FRAME DEFAULT-FRAME
DO:
    /* No-Assign Browser */  
  /* If new row, create record and assign values in browse. */  
  IF brProperty:NEW-ROW THEN DO:    
    CREATE ttProperty.    
    ASSIGN INPUT BROWSE brProperty ttProperty.PropName ttProperty.PropValue .    
    DISPLAY ttProperty.PropName ttProperty.PropValue WITH BROWSE brProperty.   
    brProperty:CREATE-RESULT-LIST-ENTRY().    
    RETURN NO-APPLY.  
  END.  
  /* If record exists and was changed in browse, update it. */  
  IF BROWSE brProperty:CURRENT-ROW-MODIFIED THEN DO:    
    GET CURRENT brProperty.    
    IF CURRENT-CHANGED ttProperty THEN DO:      
        DISPLAY ttProperty.PropName ttProperty.PropValue
        WITH BROWSE brProperty.      
        RETURN NO-APPLY.    
    END.    
    ELSE /* Record is the same, so update it with exclusive-lock */      
      ASSIGN INPUT BROWSE brProperty ttProperty.PropValue .     
       /* Downgrade the lock to a no-lock. */      
    GET CURRENT brProperty NO-LOCK.  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelProp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelProp C-Win
ON CHOOSE OF btnDelProp IN FRAME DEFAULT-FRAME /* Delete Properties */
DO:
  RUN DeleteProperties.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnProperty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnProperty C-Win
ON CHOOSE OF btnProperty IN FRAME DEFAULT-FRAME /* Add Property */
DO:
  brProperty:INSERT-ROW("AFTER").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReceive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReceive C-Win
ON CHOOSE OF btnReceive IN FRAME DEFAULT-FRAME /* Receive */
DO:
  RUN ReceiveMessage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend C-Win
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send */
DO:
  RUN SendMessage.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbQueues
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbQueues C-Win
ON F5 OF cbQueues IN FRAME DEFAULT-FRAME /* Queues */
DO:
  RUN GetQueues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN GetQueues.

  ASSIGN c-Win:TITLE = c-Win:TITLE + " - " + cAction.
  RUN enableUI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateProperty C-Win 
PROCEDURE CreateProperty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipPropName AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipPropValue AS CHARACTER   NO-UNDO.
  
  CREATE ttProperty.
  ASSIGN ttProperty.PropName = ipPropName
         ttProperty.PropValue = ipPropValue
         .
         
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteProperties C-Win 
PROCEDURE DeleteProperties :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    EMPTY TEMP-TABLE ttProperty.
    {&OPEN-QUERY-brProperty}
  END.
  
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enableUI C-Win 
PROCEDURE enableUI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY tlPersistent WHEN cAction = "SEND" 
            edText 
            .
  
    IF cAction = "SEND" THEN
      ASSIGN btnSend:ROW    = btnReceive:ROW
             btnSend:COLUMN = btnReceive:COLUMN
             .
      
    ENABLE brProperty 
           cbQueues     WHEN cAction = "RECEIVE"
           btnReceive  WHEN cAction = "RECEIVE"
           flQueue      WHEN cAction = "SEND"
           tlPersistent WHEN cAction = "SEND"
           edText       WHEN cAction = "SEND"
           btnSend     WHEN cAction = "SEND"
           btnProperty WHEN cAction = "SEND"
           btnDelProp  WHEN cAction = "SEND"
           .
    
  END.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY edText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnReceive edText brProperty 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FileDropped C-Win 
PROCEDURE FileDropped :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bytesMessage AS MEMPTR NO-UNDO.
  DEFINE VARIABLE cFile        AS CHARACTER   NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
  
      cFile = brProperty:GET-DROPPED-FILE(1).
      COPY-LOB FILE cFile TO OBJECT bytesMessage.
      
      cFile = ENTRY(NUM-ENTRIES(cFile,"\") ,cFile,"\").
      
      CREATE ttProperty.
      ASSIGN ttProperty.PropName = "FileName"
             ttProperty.PropValue = cFile
             .
      
      COPY-LOB FILE brProperty:GET-DROPPED-FILE(1) TO OBJECT ttProperty.PropFile.
             
      CREATE ttProperty.
      ASSIGN ttProperty.PropName = "FileSize"
             ttProperty.PropValue = STRING (GET-SIZE(bytesMessage))
             .
     
     {&OPEN-QUERY-brProperty}
     
  END.
  
  RETURN.
  
  FINALLY:
    SET-SIZE(BytesMessage) = 0.       
  END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetQueues C-Win 
PROCEDURE GetQueues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hDocument AS HANDLE      NO-UNDO.
    
    CREATE X-DOCUMENT hDocument.
    hDocument:LOAD("file",cQueueFile,FALSE).
    
    DATASET dsQueue:READ-XML("HANDLE",hDocument,"empty",?,?,?,?).
    
    DELETE OBJECT hDocument.                          
    
    DO WITH FRAME {&FRAME-NAME}:

      cbQueues:LIST-ITEMS = "".
      
      FOR EACH Queue:
        cbQueues:ADD-LAST(Queue.NAME).
      END.
      
      cbQueues:SCREEN-VALUE = cbQueues:ENTRY(1).

    END.
    
    RETURN.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenFile C-Win 
PROCEDURE OpenFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFile AS CHARACTER   NO-UNDO.
  
  IF NOT AVAILABLE ttProperty THEN RETURN.
  
  IF ttProperty.PropName = "FileName" THEN
  DO:
    cFile = SESSION:TEMP-DIR + ttProperty.PropValue.
    COPY-LOB FROM OBJECT ttProperty.PropFile TO FILE cFile.
    OS-COMMAND NO-WAIT VALUE(cFile).
  END.

  RETURN.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReceiveMessage C-Win 
PROCEDURE ReceiveMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE factory         AS QueueConnectionFactory NO-UNDO.
  DEFINE VARIABLE connection      AS QueueConnection        NO-UNDO.
  DEFINE VARIABLE queueSession    AS QueueSession           NO-UNDO.
  DEFINE VARIABLE queue           AS queue                  NO-UNDO.
  DEFINE VARIABLE queueReceiver   AS QueueReceiver          NO-UNDO.
  DEFINE VARIABLE receiveQueue    AS Queue                  NO-UNDO.
  DEFINE VARIABLE receivedMessage AS Message                NO-UNDO.
  DEFINE VARIABLE bytesMessage    AS BytesMessage           NO-UNDO.
  DEFINE VARIABLE textMessage     AS textMessage            NO-UNDO.
                                                            
  DEFINE VARIABLE longcharText    AS LONGCHAR               NO-UNDO.
  DEFINE VARIABLE cProperties     AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE lcFile          AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE iEntry          AS INTEGER                NO-UNDO.
  DEFINE VARIABLE cSeparator      AS CHARACTER              NO-UNDO INIT "|".
  
  EMPTY TEMP-TABLE ttProperty.
  
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
  
    /* Initialising screen */
    ASSIGN edText = "".
    DISPLAY edText.
    {&OPEN-QUERY-brProperty}
           
    ASSIGN factory       = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613)
           connection    = factory:createQueueConnection(?, ?)
           queueSession  = connection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE})
           queue         = queueSession:createQueue(cbQueues:SCREEN-VALUE)
           queueReceiver = queueSession:createReceiver(queue)
           receivedMessage = queueReceiver:receive()
           . 
    
    /* Get all properties of the message */
    ASSIGN cProperties = receivedMessage:getStringProperty(?).
       
    DO iEntry = 1 TO NUM-ENTRIES(cProperties,cSeparator):
      IF NUM-ENTRIES(ENTRY(iEntry,cProperties,cSeparator),";") = 2 THEN
        RUN CreateProperty (ENTRY(1,ENTRY(iEntry,cProperties,cSeparator),";"),
                            ENTRY(2,ENTRY(iEntry,cProperties,cSeparator),";")).
    END.
    
    /* TextMessage ? */
    textMessage  = CAST(receivedMessage, TextMessage) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN
      ASSIGN edText = STRING(textMessage:TEXT)
             .

    /* BytesMessage */
    bytesMessage = CAST(receivedMessage,BytesMessage) NO-ERROR.
    IF ERROR-STATUS:GET-MESSAGE(1) = "" THEN
    DO:
      ASSIGN lcFile = receivedMessage:getstringProperty("FileName")
             edText = STRING (bytesMessage:TEXT)
             .
      
      IF NOT CAN-FIND(FIRST ttProperty 
                      WHERE ttProperty.PropName = "Filename") THEN
      DO:
        RUN CreateProperty ("FileName", lcFile).
        RUN CreateProperty ("FileSize", receivedMessage:getstringProperty("FileSize")).
      END.
      
      FOR FIRST ttProperty 
        WHERE ttProperty.PropName = "Filename":
          
          COPY-LOB FROM OBJECT bytesMessage:bytes TO OBJECT ttProperty.PropFile .

          ASSIGN ttProperty.PropFile = bytesMessage:bytes.
      END.
    END.

    receivedMessage:acknowledge().

  END.
  
  RETURN.
  
  FINALLY:
    DELETE OBJECT receivedMessage NO-ERROR.
    DELETE OBJECT queueReceiver   NO-ERROR.
    DELETE OBJECT receiveQueue    NO-ERROR.
    DELETE OBJECT bytesMessage    NO-ERROR.
    DELETE OBJECT textMessage     NO-ERROR.
    
    DELETE OBJECT factory.
    DELETE OBJECT connection.   
    DELETE OBJECT queueSession. 
    
    RUN enableUI.                   
    
  END FINALLY.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendMessage C-Win 
PROCEDURE SendMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE factory         AS QueueConnectionFactory NO-UNDO.
  DEFINE VARIABLE connection      AS QueueConnection        NO-UNDO.
  DEFINE VARIABLE queueSession    AS QueueSession           NO-UNDO.
  DEFINE VARIABLE queueSender     AS QueueSender            NO-UNDO.
  DEFINE VARIABLE myMessage       AS TextMessage            NO-UNDO.
  DEFINE VARIABLE myBytesMessage  AS BytesMessage           NO-UNDO.
  DEFINE VARIABLE queue           AS queue                  NO-UNDO.
  
  DEFINE VARIABLE lcMessage       AS LONGCHAR               NO-UNDO.
  DEFINE VARIABLE cFile           AS CHARACTER              NO-UNDO.
  DEFINE VARIABLE bytesMessage    AS MEMPTR                 NO-UNDO.
  DEFINE VARIABLE cQueue          AS CHARACTER              NO-UNDO.
  
  
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE ALL.
    
    factory      = NEW nl.flusso.stomp.ConnectionFactory('mqhost':U, 61613).
    connection   = factory:createQueueConnection(?, ?).
    queueSession = connection:createQueueSession(FALSE, {&CLIENT_ACKNOWLEDGE}).

    IF cAction = "RECEIVE" THEN
      ASSIGN cQueue = cbQueues:SCREEN-VALUE.
    ELSE 
      ASSIGN cQueue = flQueue:SCREEN-VALUE.
    
    ASSIGN  queue        = queueSession:createQueue(cQueue )
            queueSender  = queueSession:createSender(queue)
            rsMessageType = "Text"
            tlPersistent
            edText
            .
    
    IF CAN-FIND(FIRST ttProperty WHERE ttProperty.PropName = "FileName") THEN
      ASSIGN rsMessageType = "Byte".

    FOR EACH ttProperty
      WHERE ttProperty.PropName = "Message-ID":
      DELETE ttProperty.
    END.
      
    /* When in a message a file is attached, 
       then create a bytemessage */
    CASE rsMessageType:
      WHEN "Text" THEN
      DO:
        ASSIGN lcMessage = edText
               myMessage = queueSession:createTextMessage(lcMessage)
               myMessage:JMSDeliveryMode = STRING(tlPersistent,"true/false")
               .  
        
        FOR EACH ttProperty:
          myMessage:setStringProperty(ttProperty.PropName,ttProperty.PropValue).
        END.
        
        queueSender:SEND(myMessage).
      
      END.
      WHEN "Byte" THEN
      DO:
        FOR FIRST ttProperty 
          WHERE ttProperty.PropName = "FileName":
    
          COPY-LOB ttProperty.PropFile TO OBJECT bytesMessage.
          ASSIGN myBytesMessage = queueSession:createBytesMessage(bytesMessage)
                 .
                 
          myBytesMessage:setStringProperty("FileName",ttProperty.PropValue).
          myBytesMessage:setLongProperty("FileSize",GET-SIZE(bytesMessage)).
        
        END.
    
        FOR EACH ttProperty
          WHERE  CAN-DO("FileName,FileSize",ttProperty.PropName) = FALSE:
          myBytesMessage:setStringProperty(ttProperty.PropName,ttProperty.PropValue).
        END.
        
        myBytesMessage:JMSDeliveryMode = STRING(tlPersistent,"true/false").
        myBytesMessage:Text = edText. 

        queueSender:SEND(myBytesMessage).
      END.
    END CASE.
    
    EMPTY TEMP-TABLE ttProperty.
    ASSIGN edText = "". 
  
  END.

  RETURN.
    
  FINALLY:
    
    SET-SIZE(bytesMessage) = 0.
    
    IF VALID-OBJECT(myMessage) THEN
      DELETE OBJECT myMessage.
    IF VALID-OBJECT(myBytesMessage) THEN
      DELETE OBJECT myBytesMessage.
    IF VALID-OBJECT(queueSender) THEN
      DELETE OBJECT queueSender.
    IF VALID-OBJECT(queue) THEN
      DELETE OBJECT queue.
    
    DELETE OBJECT factory.
    DELETE OBJECT connection.   
    DELETE OBJECT queueSession. 
    RUN enableUI.
  END FINALLY.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

