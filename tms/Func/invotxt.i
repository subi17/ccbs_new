/******************************************************
 MODULE .......: invotxt.i
 FUNCTION .....: FUNCTIONS FOR INVOTXT2.P
 APPLICATION ..: MASTER
 CREATED ......: 14-05-02 PT
 CHANGED ......: 21.05.02/tk  Added InvSect
                 24.09.02/jr  Modified lists and program
                 18.03.03/aam fRepType,
                              Product & Prodpack removed temporarily
                 07.04.03/jp  mobsub, orderconfirmation             
                 05.09.03/aam brand,
                              "General"

******************************************************/
DEFINE VARIABLE targets AS C  NO-UNDO.
DEFINE VARIABLE tables  AS C  NO-UNDO.
DEFINE VARIABLE expls   AS C  NO-UNDO.
DEFINE VARIABLE ppos    AS C  NO-UNDO.
DEFINE VARIABLE aaa     AS I  NO-UNDO.
DEFINE VARIABLE prog    AS C  NO-UNDO.

targets = "Customer,Invgroup,BillItem,CustGroup," +
          "Salesman,InvSect,Mobile,OrderConf,PrintCont,SMS,EMAIL,EKIRJE," +
          "Invoice,General"

          /* ,Product,ProdPack" */.

tables  = "Customer,Invgroup,BillItem,CustGroup," +
          "Salesman,InvSect,MobSub,orderconf,PrintCont,SMS,EMAIL,EKIRJE," +
          "Invoice,General"

          /* ProdPack,Product, */.

Expls   = "Customer,Invoicing Group,Billing Item," +
          "Customer Group,Salesman,Invoice Section,Mobile Subscription," +
          "Order Confirmation,PrintCont,SMS,EMAIL,EKIRJE," +
          "Invoice,General (all customers)"
          /* Product Package,Product, */.

Prog    = "nnasel,nnigse,nntuse,nncgse,nnmyse,h-invsect,mobsub,,,,,nnlase,,," 
            /* h-prodpack,h-product, */.

ppos    = "Beg of Invoice,End of Invoice,Beg of Section,End of Section," + 
          "Beg of Invoice Row,End of Invoice Row,End of MobSubs" .

/* FOR programmer's eyes:

Target      db table    Key field(s)               Program F9
----------- ---------------------- ------------------------------------
Customer    Customer    CustNum                    nnasel
Invgroup    InvGroup    InvGroup                   nnigse
BillItem    BillItem    BillCode                   nntuse
ProdPack    ProdPack    ProdPack                   h-prodpack
Product     Product     Product                    h-product
CustGroup   CustGroup   CustGroup                  nncgse
Salesman    Salesman    Salesman                   nnmyse
Invrunlog   InvRunLog   Date + InvCode + InvGroup
InvSect     InvSect     nvSect                     h-invsect
mobsub
orderconf
PrintCont
----------------------------------------------------------------------- */

DEF TEMP-TABLE t-target
   FIELD Target LIKE InvText.Target
   FIELD expl     AS C FORMAT "x(30)"
   FIELD ttable   AS C FORMAT "x(16)" 
   FIELD tprogram AS C FORMAT "X(15)" 
INDEX Target IS PRIMARY UNIQUE
   Target.

DO aaa = 1 TO NUM-ENTRIES(targets):
   CREATE t-target.
   ASSIGN
      t-target.Target   = ENTRY(aaa,targets)
      t-target.expl     = ENTRY(aaa,expls)
      t-target.ttable   = ENTRY(aaa,tables)
      t-target.tprogram = ENTRY(aaa,prog).

END.

FUNCTION fValKeyValue RETURNS CHARACTER
   (Target   AS CHARACTER, 
    KeyValue AS CHARACTER, 
    SILENT   AS LOG):

   CASE Target:

      WHEN "Customer" THEN DO:
         FIND Customer WHERE Customer.CustNum = INT(KeyValue)
         NO-LOCK NO-ERROR.
         IF NOT AVAIL Customer THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN Customer.CustName.   

      END.

      WHEN "salesman" THEN DO:
         FIND Salesman WHERE        
             Salesman.Salesman = gcBrand AND
             Salesman.Salesman = KeyValue
         NO-LOCK NO-ERROR.
         IF NOT AVAIL Salesman THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN Salesman.SmName.  

      END.

      WHEN "InvGroup" THEN DO:
         FIND InvGroup WHERE 
            InvGroup.Brand    = gcBrand AND
            InvGroup.InvGroup = KeyValue
         NO-LOCK NO-ERROR.
         IF NOT AVAIL InvGroup THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN InvGroup.IGName.  
      END.

      WHEN "BillItem" THEN DO:
         FIND BillItem WHERE 
            BillItem.Brand    = gcBrand AND
            BillItem.BillCode = KeyValue
         NO-LOCK NO-ERROR.
         IF NOT AVAIL BillItem THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN BillItem.BIName.  
      END.

      WHEN "CustGroup" THEN DO:
         FIND CustGroup WHERE 
            CustGroup.Brand     = gcBrand AND
            CustGroup.CustGroup = KeyValue
         NO-LOCK NO-ERROR.
         IF NOT AVAIL CustGroup THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN CustGroup.CGName.  
      END.

      WHEN "InvSect" THEN DO:
         FIND InvSect WHERE 
            InvSect.Brand   = gcBrand AND
            InvSect.InvSect = KeyValue
         NO-LOCK NO-ERROR.
         IF NOT AVAIL InvSect THEN DO:
            IF NOT silent THEN MESSAGE "Unknown " Target "!".
            RETURN ?.
         END.
         ELSE RETURN InvSect.ISName.
      END.

      WHEN "printCont" or
      WHEN "orderconf" THEN RETURN "".

      WHEN "EMAIL" THEN DO:
           FIND tmscodes WHERE
                TMSCodes.TableName = "Invtext" AND
                TMSCodes.FieldName = "KEYVALUE" AND
                TMSCodes.CodeGroup = "EMAIL"    AND
                TMSCodes.CodeValue = keyvalue
           NO-LOCK NO-ERROR.
           IF NOT AVAIL tmscodes THEN DO:
              IF NOT silent THEN MESSAGE "Unknown " target "!".
              RETURN ?.
           END.
           ELSE RETURN tmscodes.codevalue.
      END.

      WHEN "EKIRJE" THEN DO:
         FIND tmscodes WHERE
              TMSCodes.TableName = "Invtext"  AND
              TMSCodes.FieldName = "KEYVALUE" AND
              TMSCodes.CodeGroup = "EKIRJE"   AND
              TMSCodes.CodeValue = keyvalue
         NO-LOCK NO-ERROR.
            IF NOT AVAIL tmscodes THEN DO:
               IF NOT silent THEN MESSAGE "Unknown " target "!".
               RETURN ?.
            END.
            ELSE RETURN tmscodes.codevalue.
       END.

       WHEN "SMS" THEN DO:
          RETURN "". 
       END.

       WHEN "invoice" THEN DO:
          RETURN "".
       END. 

       WHEN "General"
       THEN RETURN "All customers".

   END.

END FUNCTION.

FUNCTION fGetProgram RETURNS CHARACTER
   (icTarget  AS CHARACTER):

   FIND t-target NO-LOCK WHERE t-target.Target = icTarget NO-ERROR.
   IF AVAIL t-target THEN RETURN t-target.tProgram.

END FUNCTION.

FUNCTION fRepType RETURNS CHARACTER
   (iiReport AS INT).

   DEF VAR lcRepType AS CHAR NO-UNDO.

   CASE iiReport:
   WHEN 0 THEN lcRepType = "Invoice".
   WHEN 1 THEN lcRepType = "Reminder".
   WHEN 2 THEN lcRepType = "eMail".
   WHEN 3 THEN lcRepType = "Order Confirmation".
   WHEN 4 THEN lcRepType = "SALDO TXT".
   WHEN 5 THEN lcRepType = "General".
   OTHERWISE lcRepType = "".
   END CASE. 

   RETURN lcRepType.

END FUNCTION.
