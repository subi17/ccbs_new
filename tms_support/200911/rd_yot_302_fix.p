
/* update ordercustomer and order data from a  list */



DEFINE VAR pcInputFile AS CHARACTER NO-UNDO.
DEFINE VAR pcOutputFile AS CHARACTER NO-UNDO.
DEFINE VAR llSimulated AS LOGICAL NO-UNDO. 
DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOut AS CHARACTER NO-UNDO. 


DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrdererIdType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOrdererId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCustomerIdType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCustomerId AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCrStamp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcActStamp AS CHARACTER NO-UNDO. 

DEFINE STREAM sin.
DEFINE STREAM sout.

{commpaa.i}
katun = "rafaeldv".
gcBrand = "1".
{eventval.i}
IF llDoEvent THEN DO:
  &GLOBAL-DEFINE STAR_EVENT_USER katun
  {lib/eventlog.i}
   DEFINE VARIABLE lhCustContact AS HANDLE NO-UNDO.
END.


pcInputFile = "yot_302.input".
pcOutputFile = "yot_302.log". 
llSimulated = FALSE.

INPUT STREAM sin FROM VALUE(pcInputFile).
OUTPUT STREAM sout TO VALUE(pcOutputFile).

DEF BUFFER bCustNIF FOR Customer .
REPEAT:
   IMPORT STREAM sin UNFORMATTED cLine.
   liCustNum = INT(ENTRY(2,cLine)).
   lcCLI  = ENTRY(3,cLine).
   lcCrStamp = ENTRY(4,cLine).
   lcActStamp = ENTRY(5,cLine).
   lcOrdererIdType = ENTRY(6,cLine).
   lcOrdererId = ENTRY(7,cLine).
   lcCustomerIdType = "CIF".
   lcCustomerId = ENTRY(8,cLine).
   
   lcOut = cLine + " => " .

   /* find corporate customer */
   FIND Customer NO-LOCK WHERE
        Customer.brand = gcbrand and 
        Customer.CustNum = liCustNum  NO-ERROR. 

    IF NOT AVAIL Customer THEN DO:
       lcOut = lcOut +  " ERROR: Customer not found  with CustNum : " + STRING(liCustNum).
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.


   /* customer number agree with CIF  */
   IF Customer.OrgId NE lcCustomerId THEN DO:
       lcOut = lcOut +  "  ERROR: CustNum do not correspond to CIF OrgId, Customer.OrgId =  " + Customer.OrgId .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.

   IF Customer.CustIdType NE "CIF" THEN DO: 
       lcOut = lcOut +  "  ERROR: CustNum is not Customer.CustIdType CIF, Customer.CustidType =  " + Customer.CustidType .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.


   /* let find the mobsub */
   FIND FIRST Mobsub WHERE
      Mobsub.Brand = gcBrand AND
      Mobsub.CLI = lcCLI NO-LOCK NO-ERROR. 

   IF NOT AVAIL MobSub THEN DO:
       lcOut = lcOut +   
              " ERROR: Mobsub not found with that CLI" .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
     NEXT.
   END.

   /* check activation date */
   IF SUBSTRING(STRING(MobSub.ActivationTS),1,8,"CHARACTER") NE lcActStamp THEN DO:
         lcOut = lcOut +  " ERROR: Current Mobsub with that CLI has different activation date." .
       PUT STREAM sout UNFORMATTED lcOut SKIP.
       NEXT.
   END.

   /* find nif customer */
   FIND bCustNIF NO-LOCK WHERE
        bCustNIF.Brand = gcBrand AND
        bCustNIF.OrgId = lcOrdererId NO-ERROR. 

   IF AVAIL bCustNIF  THEN DO:
       IF MobSub.CustNum EQ bCustNIF.CustNum THEN DO:
           lcOut = lcOut +  " ERROR: MobSub below NIF Customer not CIF Customer (it is not Category 20)." .
           PUT STREAM sout UNFORMATTED lcOut SKIP.
           NEXT.
       END.
   END.
  
   IF MobSub.CustNum EQ Customer.CustNum THEN DO:
           

          /* check customer contact */ 
          FIND CustContact WHERE
               CustContact.Brand = gcBrand AND
               CustContact.Custnum = Customer.Custnum AND
               CustContact.CustType = 1 NO-LOCK NO-ERROR.

          IF AVAIL CustContact AND  
                   CustContact.CustIdType EQ lcOrdererIdType AND
                   CustContact.OrgId EQ lcOrdererId THEN DO:

                   lcOut = lcOut + " DONE: nothing has to be updated".
                   PUT STREAM sout UNFORMATTED lcOut SKIP.
                   NEXT.
          END.

          /* Update/Create CustContact */
          IF NOT llSimulated THEN DO:
             IF lldoevent then do:
                lhcustcontact = buffer custcontact:handle.
               RUN StarEventInitialize(lhCustContact).
             END.

             IF NOT AVAIL CustContact THEN DO:
                CREATE CustContact.
             END.
             ELSE DO:
                FIND CURRENT CustContact EXCLUSIVE-LOCK NO-ERROR.
                IF lldoevent THEN RUN StarEventSetOldBuffer(lhCustContact).
             END.
             ASSIGN
                CustContact.Brand          = gcBrand
                CustContact.Custnum        = Customer.CustNum
                CustContact.CustType       = 1
                CustContact.CustIdType     = lcOrdererIDType
                CustContact.OrgId          = lcOrdererID.
                       
             IF llDoEvent THEN DO:
                IF NEW CustContact THEN RUN StarEventMakeCreateEvent (lhCustContact).
                ELSE RUN StarEventMakeModifyEvent (lhCustContact).
             END.

             fCleanEventObjects().
             RELEASE CustContact.
          END.
          lcOut = lcOut + " DONE: Contact person has been update/created using NIF info !".
          PUT STREAM sout UNFORMATTED lcOut SKIP. 
          NEXT.
                            
   END.

   /* at this point this mobsub is below other customer */
   lcOut = lcOut + " ERROR: A MobSub with that CLI has same activation date but is not below CIF either NIF customer".
   PUT STREAM sout UNFORMATTED lcOut SKIP.
  
END.

OUTPUT STREAM sout CLOSE.
INPUT STREAM sin CLOSE.

