/* fmsreqval.i   14.02.06/aam  

   get values from pending requests

   changes:      22.03.06/aam 
*/

DEF BUFFER bReqValue FOR MsRequest.

/* customer address changes */
FUNCTION fAddressChangeValues RETURNS LOGICAL
   (INPUT   iiCustNum  AS INT,   /* target    */
    OUTPUT  ocName     AS CHAR,  /* lastname;firstname;coname */
    OUTPUT  ocAddress  AS CHAR,  /* address;zipcode;postoffice;country */
    OUTPUT  ocContact  AS CHAR,  /* email;tel */
    OUTPUT  ocOthers   AS CHAR,  /* category  */
    OUTPUT  odChgStamp AS DEC).  /* when      */

   /* is there a pending request */
   FIND FIRST bReqValue WHERE
              bReqValue.Brand     = gcBrand   AND
              bReqValue.ReqType   = 6         AND
              bReqValue.CustNum   = iiCustNum AND
              bReqValue.ReqStatus = 0 NO-LOCK NO-ERROR.

   IF NOT AVAILABLE bReqValue THEN RETURN FALSE.
  
   ASSIGN ocName     = bReqValue.ReqCParam1
          ocAddress  = bReqValue.ReqCParam2
          ocContact  = bReqValue.ReqCParam3
          ocOthers   = bReqValue.ReqCParam4
          odChgStamp = bReqValue.ActStamp.
          
   RETURN TRUE.
          
END FUNCTION.
 
FUNCTION fOwnerChangeValues RETURNS LOGICAL
   (INPUT   iiMsSeq    AS INT,   /* subscription     */
    OUTPUT  oiNewCust  AS INT,   /* new agr.customer */
    OUTPUT  ocName     AS CHAR,  /* lastname;firstname;coname */
    OUTPUT  ocAddress  AS CHAR,  /* address;zipcode;postoffice;country */
    OUTPUT  ocContact  AS CHAR,  /* email;tel */
    OUTPUT  ocOthers   AS CHAR,  /* category  */
    OUTPUT  odChgStamp AS DEC).  /* when      */

   /* is there a pending request */
   FIND FIRST bReqValue WHERE
              bReqValue.MsSeq     = iiMsSeq   AND
              bReqValue.ReqType   = 10        AND
              (bReqValue.ReqStatus = 0 OR
               bReqValue.ReqStatus > 4) NO-LOCK NO-ERROR.

   IF NOT AVAILABLE bReqValue THEN RETURN FALSE.
  
   ASSIGN oiNewCust  = bReqValue.ReqIParam1
          ocName     = ENTRY(1,bReqValue.ReqCParam1,";") + ";" +
                       ENTRY(2,bReqValue.ReqCParam1,";") + ";" + 
                       ENTRY(3,bReqValue.ReqCParam1,";")
          ocAddress  = ENTRY(4,bReqValue.ReqCParam1,";") + ";" +
                       ENTRY(5,bReqValue.ReqCParam1,";") + ";" +
                       ENTRY(6,bReqValue.ReqCParam1,";") + ";" +
                       ENTRY(7,bReqValue.ReqCParam1,";")
          ocContact  = ENTRY(8,bReqValue.ReqCParam1,";") + ";" +
                       ENTRY(9,bReqValue.ReqCParam1,";")
          ocOthers   = ENTRY(10,bReqValue.ReqCParam1,";")
          odChgStamp = bReqValue.ActStamp.
          
   RETURN TRUE.

END FUNCTION.

