DEFINE VARIABLE vcHost     AS CHARACTER    INITIAL "localhost"    NO-UNDO.
DEFINE VARIABLE vcPort     AS CHARACTER    INITIAL "8161"         NO-UNDO.
DEFINE VARIABLE vhSocket   AS HANDLE                                NO-UNDO.

DEFINE TEMP-TABLE Queues NO-UNDO
    FIELD NAME AS CHAR.
DEFINE TEMP-TABLE queue NO-UNDO
  FIELD NAME AS CHAR 
  .

DEFINE DATASET dsQueue FOR queues, queue.

CREATE SOCKET vhSocket.

vhSocket:CONNECT('-H ' + vcHost + ' -S ' + vcPort) NO-ERROR.
   
IF vhSocket:CONNECTED() = FALSE THEN
DO:
    MESSAGE "Connection failure" VIEW-AS ALERT-BOX.
   
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.
    RETURN.
END.
ELSE
   MESSAGE "Connect" VIEW-AS ALERT-BOX.

vhSocket:SET-READ-RESPONSE-PROCEDURE('getResponse').
/* supposes there is an webspeed app called yourapp.w that receives param1, param2, param3 */

RUN PostRequest (INPUT 'http://mqhost:8161/admin/xml/queues.jsp', '').

WAIT-FOR READ-RESPONSE OF vhSocket.
vhSocket:DISCONNECT() NO-ERROR.

DELETE OBJECT vhSocket.

QUIT.

PROCEDURE getResponse:
   
    DEFINE VARIABLE vcWebResp    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lSucess      AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE mResponse    AS MEMPTR           NO-UNDO.
    DEFINE VARIABLE lcData       AS LONGCHAR   NO-UNDO.
    DEFINE VARIABLE cRegel AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hDocument AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hNode AS HANDLE      NO-UNDO.
    
    IF vhSocket:CONNECTED() = FALSE THEN do:
        MESSAGE 'Not Connected' VIEW-AS ALERT-BOX.
        RETURN.
    END.
    lSucess = TRUE.
       
    DO WHILE vhSocket:GET-BYTES-AVAILABLE() > 0:
         SET-SIZE(mResponse) = vhSocket:GET-BYTES-AVAILABLE() + 1.
         SET-BYTE-ORDER(mResponse) = BIG-ENDIAN.
         vhSocket:READ(mResponse,1,1,vhSocket:GET-BYTES-AVAILABLE()).
         vcWebResp = vcWebResp + GET-STRING(mResponse,1).
    END.
  
    lcData = vcWebResp.
    
    CREATE X-DOCUMENT hDocument.
    hDocument:LOAD("file",'http://mqhost:8161/admin/xml/queues.jsp',FALSE).
    hDocument:SAVE("file", "c:\tmp\output3.xml"). 
    
    DATASET dsQueue:READ-XML("HANDLE",hDocument,"empty",?,?,?,?).
    
    DELETE OBJECT hDocument.                          
    
    FOR EACH queue:
    MESSAGE queue.NAME
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    /*                                     
    OUTPUT TO c:\tmp\OUTPUT.txt.
    PUT UNFORMATTED vcWebResp.
    OUTPUT CLOSE.                                       
    
    INPUT FROM c:\tmp\OUTPUT.txt.
    REPEAT:
      IMPORT UNFORMATTED cRegel.
      IF INDEX(cregel,"queue name") > 0 THEN
      DO:
        CREATE Queue.
        ASSIGN  cRegel = REPLACE(cRegel,'"',"")
                cRegel = REPLACE(cRegel,">","")
                Queue.NAME = entry(2,cRegel,"=")
                .
        MESSAGE queue.NAME
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
        
    END.
    
    INPUT CLOSE.
    */    
    /*
    *
    *
    *PUT HERE THE CODE TO MANIPULATE THE ANSWER
    */
   

   
END.

PROCEDURE PostRequest:
   
    DEFINE VARIABLE vcRequest      AS CHARACTER.
    DEFINE VARIABLE mRequest       AS MEMPTR.
    DEFINE INPUT PARAMETER postUrl AS CHAR. /* URL that will send the data. It must be all the path after the server. IE: /scripts/cgiip.exe/WService=wsbroker1/myApp.htm */
    DEFINE INPUT PARAMETER postData AS CHAR. /* Parameters to be sent in the format paramName=value&paramName=value&paramName=value */
 
    vcRequest = 'POST ' + postUrl + ' HTTP/1.0~r~n' +
            'Content-Type: application/x-www-form-urlencoded~r~n' +        
            'Content-Length:' + string(LENGTH(postData)) + '~r~n' +
             '~r~n' +
             postData + '~r~n'.

    MESSAGE vcREquest VIEW-AS ALERT-BOX.
    SET-SIZE(mRequest)            = 0.
    SET-SIZE(mRequest)            = LENGTH(vcRequest) + 1.
    SET-BYTE-ORDER(mRequest)      = BIG-ENDIAN.
    PUT-STRING(mRequest,1)        = vcRequest .
   
    vhSocket:WRITE(mRequest, 1, LENGTH(vcRequest)).
END PROCEDURE.
