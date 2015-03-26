/* xmlpura2.i      26.08.2003/aam 

  create an XML-file for nnpura2-report  

*/

{fxmlfile.i}


/* all fields as char so that they can be empty in xml */
DEF TEMP-TABLE ttCallRep NO-UNDO
    FIELD CustHeader  AS CHAR
    FIELD CustTotHead AS CHAR
    FIELD CustTotQty  AS CHAR
    FIELD CustTotDur  AS CHAR
    FIELD CustTotAmt  AS CHAR
    FIELD CLIHeader   AS CHAR
    FIELD CLITotHead  AS CHAR
    FIELD CLITotQty   AS CHAR
    FIELD CLITotDur   AS CHAR
    FIELD CLITotAmt   AS CHAR
    FIELD ProdHeader  AS CHAR
    FIELD ProdTotHead AS CHAR
    FIELD ProdTotQty  AS CHAR
    FIELD ProdTotDur  AS CHAR
    FIELD ProdTotAmt  AS CHAR
    FIELD CCNHeader   AS CHAR
    FIELD CCNTotHead  AS CHAR
    FIELD CCNTotQty   AS CHAR
    FIELD CCNTotDur   AS CHAR
    FIELD CCNTotAmt   AS CHAR
    FIELD Date        AS CHAR
    FIELD TimeStart   AS CHAR
    FIELD BSub        AS CHAR
    FIELD Duration    AS CHAR
    FIELD Amount      AS CHAR.
    
DEF TEMP-TABLE ttHeaders NO-UNDO
    FIELD Logo      AS CHAR
    FIELD MainHead  AS CHAR
    FIELD SubHead1  AS CHAR
    FIELD SubHead2  AS CHAR
    FIELD SubHead3  AS CHAR
    FIELD SubHead4  AS CHAR
    FIELD ColHead1  AS CHAR
    FIELD ColHead2  AS CHAR
    FIELD ColHead3  AS CHAR
    FIELD ColHead4  AS CHAR
    FIELD ColHead5  AS CHAR.
    
/* buffers for temp-tables */
DEFINE VARIABLE lbCallRep    AS HANDLE  NO-UNDO.
DEFINE VARIABLE lbHeaders    AS HANDLE  NO-UNDO. 

DEFINE VARIABLE ghDoc        AS HANDLE  NO-UNDO.
DEFINE VARIABLE ghRoot       AS HANDLE  NO-UNDO.

/* nodes */
DEFINE VARIABLE ghCallMain   AS HANDLE  NO-UNDO.
DEFINE VARIABLE ghCallHeader AS HANDLE  NO-UNDO.
DEFINE VARIABLE ghCallRep    AS HANDLE  NO-UNDO.

DEFINE VARIABLE lcCallRepFields AS CHAR NO-UNDO.
DEFINE VARIABLE lcHeaderFields  AS CHAR NO-UNDO.
                            
DEFINE VARIABLE llSkipEmpty     AS LOG  NO-UNDO INIT FALSE. 
DEFINE VARIABLE lcLogo          AS CHAR NO-UNDO.

ASSIGN lcCallRepFields = 'CustHeader¤X(60)'
                         + ',CustTotHead¤X(50)'
                         + ',CustTotQty¤X(15)'
                         + ',CustTotDur¤X(15)'
                         + ',CustTotAmt¤X(15)'
                         + ',CLIHeader¤X(60)'
                         + ',CLITotHead¤X(50)'
                         + ',CLITotQty¤X(15)'
                         + ',CLITotDur¤X(15)'
                         + ',CLITotAmt¤X(15)'
                         + ',ProdHeader¤X(60)'
                         + ',ProdTotHead¤X(50)'
                         + ',ProdTotQty¤X(15)'
                         + ',ProdTotDur¤X(15)'
                         + ',ProdTotAmt¤X(15)'
                         + ',CCNHeader¤X(60)'
                         + ',CCNTotHead¤X(50)'
                         + ',CCNTotQty¤X(15)'
                         + ',CCNTotDur¤X(15)'
                         + ',CCNTotAmt¤X(15)'
                         + ',Date¤X(10)'
                         + ',TimeStart¤X(12)'
                         + ',BSub¤X(25)'
                         + ',Duration¤X(12)'
                         + ',Amount¤X(15)'
 
       lcHeaderFields =  'MainHead¤x(40)'
                         + ',SubHead1¤x(40)'
                         + ',SubHead2¤x(40)'
                         + ',SubHead3¤x(40)'
                         + ',SubHead4¤x(40)'
                         + ',ColHead1¤x(40)'
                         + ',ColHead2¤x(40)'
                         + ',ColHead3¤x(40)'
                         + ',ColHead4¤x(40)'
                         + ',ColHead5¤x(40)'
                         + ',Logo¤x(60)'.
                         
ASSIGN lbCallRep = BUFFER ttCallRep:HANDLE
       lbHeaders = BUFFER ttHeaders:HANDLE.

{xml.i}


FUNCTION fXMLInit RETURNS LOGIC
   (icMainHeader AS CHAR,
    icSubHeader1 AS CHAR,
    icSubHeader2 AS CHAR,
    icSubHeader3 AS CHAR,
    icSubHeader4 AS CHAR,
    icColHeader1 AS CHAR,
    icColHeader2 AS CHAR,
    icColHeader3 AS CHAR,
    icColHeader4 AS CHAR,
    icColHeader5 AS CHAR).

   CREATE X-DOCUMENT ghDoc.
   CREATE X-NODEREF  ghRoot.

   CREATE X-NODEREF  ghCallMain.
   CREATE X-NODEREF  ghCallHeader.
   CREATE X-NODEREF  ghCallRep.

   ghDoc:ENCODING = 'utf-8'.
        
   /* root node */
   RUN StarXMLCreateDocument ("callspecDocument",ghDoc,ghRoot).

   /* logo-file */
   lcLogo = OS-GETENV("XMLIMAGEDIR").
   lcLogo = lcLogo + "/" + fCParamC("LogoSL"). 
    
   lbCallRep:EMPTY-TEMP-TABLE().
   lbHeaders:EMPTY-TEMP-TABLE().
   
   /* main node for report */ 
   ghCallMain = StarXMLCreateNode(ghDoc,ghRoot,"callspec").
        
   CREATE ttHeaders.
   ASSIGN ttHeaders.MainHead = icMainHeader
          ttHeaders.SubHead1 = icSubHeader1
          ttHeaders.SubHead2 = icSubHeader2
          ttHeaders.SubHead3 = icSubHeader3
          ttHeaders.SubHead4 = icSubHeader4
          ttHeaders.ColHead1 = icColHeader1
          ttHeaders.ColHead2 = icColHeader2
          ttHeaders.ColHead3 = icColHeader3
          ttHeaders.ColHead4 = icColHeader4
          ttHeaders.ColHead5 = icColHeader5
          ttHeaders.Logo     = lcLogo.
       
   /* headers */
   ghCallHeader = StarXMLBuffer2Node (ghDoc,
                                      ghCallMain,
                                      lbHeaders,
                                      'specheader',
                                      lcHeaderFields,
                                      '',
                                      '',
                                      '',
                                      '',
                                      llSkipEmpty,
                                      YES
                                      ).         
         

END FUNCTION.

FUNCTION fXMLAddLine RETURNS LOGICAL.

   ghCallRep  = StarXMLBuffer2Node (ghDoc,
                                    ghCallMain,
                                    lbCallRep,
                                    'specline',
                                    lcCallRepFields,
                                    '',
                                    '',
                                    '',
                                    '',
                                    llSkipEmpty,
                                    YES
                                    ).    

END FUNCTION.
       
FUNCTION fXMLCLIHeader RETURNS LOGIC
   (icHeader AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.CLIHeader = icHeader.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLProdHeader RETURNS LOGIC
   (icHeader AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.ProdHeader = icHeader.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLCCNHeader RETURNS LOGIC
   (icHeader AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.CCNHeader = icHeader.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLCallLine RETURNS LOGIC
   (icDate  AS CHAR,
    icTime  AS CHAR,
    icBSub  AS CHAR,
    icDur   AS CHAR,
    icAmt   AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.Date      = icDate
          ttCallRep.TimeStart = icTime
          ttCallRep.BSub      = icBSub
          ttCallRep.Duration  = icDur
          ttCallRep.Amount    = icAmt.
          
   fXMLAddLine().

END FUNCTION.

FUNCTION fXMLCCNTotal RETURNS LOGIC
   (icHeader AS CHAR,
    icTotQty AS CHAR,
    icTotDur AS CHAR,
    icTotAmt AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.CCNTotHead = icHeader
          ttCallRep.CCNTotQty  = icTotQty
          ttCallRep.CCNTotDur  = icTotDur
          ttCallRep.CCNTotAmt  = icTotAmt.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLProdTotal RETURNS LOGIC
   (icHeader AS CHAR,
    icTotQty AS CHAR,
    icTotDur AS CHAR,
    icTotAmt AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.ProdTotHead = icHeader
          ttCallRep.ProdTotQty  = icTotQty
          ttCallRep.ProdTotDur  = icTotDur
          ttCallRep.ProdTotAmt  = icTotAmt.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLCLITotal RETURNS LOGIC
   (icHeader AS CHAR,
    icTotQty AS CHAR,
    icTotDur AS CHAR,
    icTotAmt AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.CLITotHead = icHeader
          ttCallRep.CLITotQty  = icTotQty
          ttCallRep.CLITotDur  = icTotDur
          ttCallRep.CLITotAmt  = icTotAmt.

   fXMLAddLine().
   
END FUNCTION.

FUNCTION fXMLCustTotal RETURNS LOGIC
   (icHeader AS CHAR,
    icTotQty AS CHAR,
    icTotDur AS CHAR,
    icTotAmt AS CHAR).

   CREATE ttCallRep.
   ASSIGN ttCallRep.CustTotHead = icHeader
          ttCallRep.CustTotQty  = icTotQty
          ttCallRep.CustTotDur  = icTotDur
          ttCallRep.CustTotAmt  = icTotAmt.

   fXMLAddLine().
   
END FUNCTION.


FUNCTION fXMLFinish RETURNS LOGICAL
   (iiCustNum        AS INT,
    OUTPUT ocXMLFile AS CHAR). 
   
   ocXMLFile = fXMLRepFileName(iiCustNum,
                               "pura2",
                               ".xml").
              
   StarXMLSaveToFile(ghDoc,
                     ocXMLFile).

   DELETE OBJECT ghCallRep. 
   DELETE OBJECT ghCallHeader.
   DELETE OBJECT ghCallMain.

   DELETE OBJECT ghRoot.
   DELETE OBJECT ghDoc.

   RETURN TRUE.
   
END.

