
/*------------------------------------------------------------------------
    File        : create_topup_tmsparams.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Jul 25 14:17:52 IST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FUNCTION fCreateTMSParam RETURN CHARACTER (INPUT icParamGroup AS CHARACTER , 
                                           INPUT icParamCode  AS CHARACTER ,
                                           INPUT icParamName  AS CHARACTER , 
                                           INPUT icCharVal   AS CHARACTER) :
                             
                             
    DEFINE BUFFER bfTMSParam FOR TMSParam.
    
    CREATE bfTMSParam.
    ASSIGN bfTMSParam.Brand         =   Syst.Var:gcBrand
           bfTMSParam.ParamGroup    =   icParamGroup
           bfTMSParam.ParamCode     =   icParamCode
           bfTMSParam.ParamName     =   icParamName
           bfTMSParam.CharVal       =   icCharVal
           bfTMSParam.ParamType     =   "C"           
           .                      
    
    RETURN "".
    
END FUNCTION.

FUNCTION fCreateTMSParamDE RETURN CHARACTER (INPUT icParamGroup AS CHARACTER ,
                                           INPUT icParamCode  AS CHARACTER ,
                                           INPUT icParamName  AS CHARACTER ,
                                           INPUT idValue      AS DECIMAL) :


    DEFINE BUFFER bfTMSParam FOR TMSParam.

    CREATE bfTMSParam.
    ASSIGN bfTMSParam.Brand         =   Syst.Var:gcBrand
           bfTMSParam.ParamGroup    =   icParamGroup
           bfTMSParam.ParamCode     =   icParamCode
           bfTMSParam.ParamName     =   icParamName
           bfTMSParam.DecVal        =   idValue
           bfTMSParam.ParamType     =   "DE"
           .

    RETURN "".

END FUNCTION.

fCreateTMSParam("PC.Interface","PC.Queue.ip","Messaging Broker IP Address","albireo.int.asp.qvantel.net"). 
fCreateTMSParam("PC.Interface","PC.Queue.port","Port number","61617").
fCreateTMSParam("PC.Interface","PC.Queue.Name","Queue Name", "rbs-messages").
fCreateTMSParam("PC.Interface","PC.User.Name","User name","qvantel"). 
fCreateTMSParam("PC.Interface","PC.Passcode","Password","qvantel").  
fCreateTMSParam("PC.Interface","UTF8_to_Scandics", "Scandics UTF-8","\u0142,\u2013,\u011d|l,-,g").
fCreateTMSParam("PC.Interface","PC.Ack.QueueName", "Default queue for acknowledgments","tms-errors").
fCreateTMSParamDE("PC.Interface","PC.Ack.Queue.Sleep", "Sleep time for process",1).

MESSAGE "TMSParam created successfully."
VIEW-AS ALERT-BOX.

















