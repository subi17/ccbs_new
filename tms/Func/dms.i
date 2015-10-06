/* dms.i         03.09.15/ivekov 

*/

{commali.i}
{tmsparam4.i}

ASSIGN
   katun   = "Cron"
   gcBrand = "1".
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDMS AS HANDLE NO-UNDO.
   lhDMS = BUFFER DMS:HANDLE.
   RUN StarEventInitialize(lhDMS).

   DEFINE VARIABLE lhDMSDoc AS HANDLE NO-UNDO.
   lhDMSDoc = BUFFER DMSDoc:HANDLE.
   RUN StarEventInitialize(lhDMSDoc).
END.

DEF TEMP-TABLE ttDocs NO-UNDO
   FIELD DocTypeID      AS CHAR
   FIELD DocTypeDesc    AS CHAR
   FIELD DocStatusCode  AS CHAR
   FIELD DocStatusDesc  AS CHAR
   FIELD DocStatusTS    AS DEC FORMAT "99999999.99999"
   FIELD Comment        AS CHAR.


FUNCTION fGetOrderStatusDMS RETURNS CHAR
   (icContractID AS CHAR):
   FIND FIRST DMS EXCLUSIVE-LOCK WHERE
              DMS.ContractID = icContractID
              NO-ERROR.
   IF AVAIL DMS THEN RETURN DMS.OrderStatus.
   RETURN "".      
END.
   

FUNCTION fUpdateDMS RETURNS LOGICAL
   (icDmsExternalID  AS CHAR,
    icCaseTypeID     AS CHAR,
    icContractID     AS CHAR,
    icHostTable      AS CHAR,
    iiHostId         AS INT,
    icStatusCode     AS CHAR,
    icStatusDesc     AS CHAR,
    icOrderStatus    AS CHAR,
    idStatusTS       AS DEC,
    icDocList        AS CHAR,
    icDocListSep     AS CHAR):

   DEF VAR i AS INT NO-UNDO.

   FIND FIRST DMS EXCLUSIVE-LOCK WHERE
              DMS.ContractID = icContractID 
              NO-ERROR.

   IF NOT AVAIL DMS THEN DO:
      CREATE DMS.
      ASSIGN DMS.DMSID = NEXT-VALUE(DMS).
   END.
   ELSE IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDMS).

   ASSIGN DMS.DmsExternalID = icDmsExternalID WHEN icDmsExternalID NE ""
          DMS.CaseTypeID    = icCaseTypeID
          DMS.ContractID    = icContractID
          DMS.HostTable     = icHostTable
          DMS.HostId        = iiHostId
          DMS.StatusCode    = icStatusCode
          DMS.StatusDesc    = icStatusDesc
          DMS.StatusTS      = idStatusTS.
   /*Change order status if value is given.*/       
   IF icOrderStatus NE "" THEN DMS.OrderStatus = icOrderstatus.

   IF llDoEvent THEN DO:
      IF NEW DMS THEN RUN StarEventMakeCreateEvent(lhDMS).
      ELSE RUN StarEventMakeModifyEvent(lhDMS).
   END.

   IF icDocList <> "" THEN
      DO i = 1 TO NUM-ENTRIES(icDocList,icDocListSep) BY 3:

      FIND FIRST DMSDoc EXCLUSIVE-LOCK WHERE
                 DMSDoc.DMSID     = DMS.DMSID AND
                 DMSDoc.DocTypeID = ENTRY(i,icDocList,icDocListSep)
                 NO-ERROR.

      IF NOT AVAIL DMSDoc THEN DO:
         CREATE DMSDoc.
         ASSIGN DMSDoc.DMSID = DMS.DMSID.
      END.
      ELSE IF lldoevent THEN RUN StarEventSetOldBuffer(lhDMSDoc).

      ASSIGN DMSDoc.DocStatusTS   = DMS.StatusTS
             DMSDoc.DocTypeID     = ENTRY(i,icDocList,icDocListSep)
             DMSDoc.DocStatusCode = ENTRY(i + 1,icDocList,icDocListSep)
             DMSDoc.DocRevComment = ENTRY(i + 2,icDocList,icDocListSep).

      CASE DMSDoc.DocTypeID:
         WHEN "1"  THEN DMSDoc.DocTypeDesc = "CIF EMPRESA".
         WHEN "2"  THEN DMSDoc.DocTypeDesc = "ESCRITURAS".
         WHEN "3"  THEN DMSDoc.DocTypeDesc = "NIF ADMINISTRADOR".
         WHEN "4"  THEN DMSDoc.DocTypeDesc = "NIF/NIE".
         WHEN "5"  THEN DMSDoc.DocTypeDesc = "RECIBO BANCARIO".
         WHEN "6"  THEN DMSDoc.DocTypeDesc = "RECIBO BANCARIO op DONANTE".
         WHEN "7"  THEN DMSDoc.DocTypeDesc = "FACTURA op DONANTE".
         WHEN "8"  THEN DMSDoc.DocTypeDesc = "COMPROBANTE DEPOSITO <X>".
         WHEN "9"  THEN DMSDoc.DocTypeDesc = "OTROS".
         WHEN "10" THEN DMSDoc.DocTypeDesc = "REQUERIDO BO".
      END CASE.

      IF llDoEvent THEN DO:
         IF NEW DMSDoc THEN RUN StarEventMakeCreateEvent(lhDMSDoc).
         ELSE RUN StarEventMakeModifyEvent(lhDMSDoc).
      END.

   END.

   RELEASE DMS.
   RELEASE DMSDoc.

   RETURN TRUE.

END.

FUNCTION fCollectDocs RETURNS LOGICAL
   (INPUT iiDMSID AS INT).
   FOR EACH DMSDoc NO-LOCK WHERE
            DMSDoc.DMSID = iiDMSID:
      CREATE ttDocs.
      ASSIGN ttDocs.DocTypeID     = DMSDoc.DocTypeID
             ttDocs.DocTypeDesc   = DMSDoc.DocTypeDesc
             ttDocs.DocStatusCode = DMSDoc.DocStatusCode
             ttDocs.DocStatusTS   = DMSDoc.DocStatusTS
             ttDocs.Comment       = DMSDoc.DocRevComment.

      CASE DMSDoc.DocStatusCode:
         WHEN "A" THEN ttDocs.DocStatusDesc = "Pending".
         WHEN "B" THEN ttDocs.DocStatusDesc = "Sent".
         WHEN "C" THEN ttDocs.DocStatusDesc = "Error".
         WHEN "D" THEN ttDocs.DocStatusDesc = "OK".
      END CASE.
   END.
   
END FUNCTION. 

FUNCTION fDMSOnOff RETURNS LOGICAL:
   DEF VAR liOnOff AS INT NO-UNDO.
   liOnOff = INT(fCParamI4("1","DMS","DmsOnOff")).
   IF liOnOff EQ 1 THEN RETURN TRUE.
   ELSE RETURN FALSE.
END.
