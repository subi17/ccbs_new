/*---------------------------------------------------------------------------
  MODULE .......: CGBATCH.p
  TASK .........: Gather members into a Customer Group
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 20.03.2003
  CHANGED ......: 16.09.03/aam brand
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commpaa.i}
{Syst/eventval.i}
{Func/fecgtask.i}

DEF buffer new-cgmember for cgmember.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCGMember AS HANDLE NO-UNDO.
   lhCGMember = BUFFER CGMember:HANDLE.
   RUN StarEventInitialize(lhCGMember).

   DEFINE VARIABLE lhnew-CGMember AS HANDLE NO-UNDO.
   lhnew-CGMember = BUFFER new-CGMember:HANDLE.
   RUN StarEventInitialize(lhnew-CGMember).


   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCGMember).
   END.

END.

DEF VAR ProdPack   AS CHAR                       NO-UNDO. 
DEF VAR xCustGroup LIKE CustGroup.CustGroup      NO-UNDO.
DEF VAR Salesman   LIKE Salesman.SalesMan        NO-UNDO.
DEF VAR Reseller   LIKE Reseller.reseller          NO-UNDO.
DEF VAR cbdate1    AS DA                         NO-UNDO.
DEF VAR cbdate2    AS DA                         NO-UNDO.
DEF VAR bidate1    AS DA                         NO-UNDO.
DEF VAR bidate2    AS DA                         NO-UNDO.
DEF VAR abbrev     LIKE Customer.SearchName          NO-UNDO.
DEF VAR Size       LIKE Customer.Size            NO-UNDO.
DEF VAR ConnType   LIKE Customer.conntype             NO-UNDO.
DEF VAR InvGroup   LIKE InvGroup.InvGroup        NO-UNDO.
DEF VAR PriceList  LIKE PriceList.PriceList      NO-UNDO.
DEF VAR rd-cust-nr LIKE Customer.InvCust        NO-UNDO.
DEF VAR Category   LIKE Customer.Category        NO-UNDO.
def var as-ponro    as c  format "x(6)"           NO-UNDO.
def var sz-s       as lo format "S/-"            NO-UNDO.
def var sz-m       as lo format "M/-"            NO-UNDO.
def var sz-l       as lo format "L/-"            NO-UNDO.
def var sz-xl      as lo format "XL/-"           NO-UNDO.
def var co-d       as lo format "Direct/-"       NO-UNDO.
def var co-i       as lo format "Indirect/-"     NO-UNDO.
def var ok         as lo format "Yes/No"         NO-UNDO.
DEF VAR sizes      AS c                          NO-UNDO.
DEF VAR conns      AS c                          NO-UNDO.
DEF VAR Comma      AS C                          NO-UNDO INIT ",".
def var bicheck    as lo format "Check/Omit"     NO-UNDO.
DEF VAR updatemode AS LO                         NO-UNDO.
DEF VAR lcTask     AS CHAR                       NO-UNDO.


FOR EACH  custgroup NO-LOCK:

   IF CustGroup.PrevCrit = ""  THEN NEXT.

   ASSIGN
   xCustGroup = ENTRY(1,Custgroup.PrevCrit,",")
   Category   = ENTRY(2,Custgroup.PrevCrit,",")
   InvGroup   = ENTRY(3,Custgroup.PrevCrit,",")
   PriceList  = ENTRY(4,Custgroup.PrevCrit,",")
   rd-cust-nr = INT(ENTRY(5,Custgroup.PrevCrit,","))
   ProdPack   = ENTRY(6,Custgroup.PrevCrit,",")
   Salesman   = ENTRY(7,Custgroup.PrevCrit,",")
   Reseller   = ENTRY(8,Custgroup.PrevCrit,",")
   abbrev     = ENTRY(9,Custgroup.PrevCrit,",")
   sz-s       = IF ENTRY(10,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   sz-m       = IF ENTRY(11,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   sz-l       = IF ENTRY(12,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   sz-xl      = IF ENTRY(13,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   co-d       = IF ENTRY(14,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   co-i       = IF ENTRY(15,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   cbdate1    = DATE(INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(16,Custgroup.PrevCrit,","),7,4)))
   cbdate2    = DATE(INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(17,Custgroup.PrevCrit,","),7,4)))
   bidate1    = DATE(INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(18,Custgroup.PrevCrit,","),7,4)))
   bidate2    = DATE(INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),4,2)),
                  INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),1,2)),
                  INT(Substring(ENTRY(19,Custgroup.PrevCrit,","),7,4)))
   bicheck    = IF ENTRY(20,Custgroup.PrevCrit,",") = "YES" THEN TRUE ELSE FALSE
   as-ponro    = ENTRY(21,Custgroup.PrevCrit,",")  .

   sizes = "".
   if sz-s then sizes =         "S,".
   if sz-m then sizes = sizes + "M,".
   if sz-l then sizes = sizes + "L,".
   if sz-M then sizes = sizes + "XL".

   conns = "".
   if co-i then conns =         "I,".
   if co-d then conns = conns + "D".

   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand = CustGroup.Brand:

      IF NOT SESSION:BATCH THEN DO:
         DISP customer.custnum custgroup.custgroup with frame aa. pause 0.
      END.

      if Category     ne "" AND Customer.Category   NE Category    
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      if abbrev     ne "" AND Customer.SearchName  NE abbrev    
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      if Salesman    ne "" AND Customer.SalesMan  NE Salesman   
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      if Reseller    ne "" AND Customer.Reseller  NE Reseller   
      THEN DO:
         RUN pRemoveCGMember.
           NEXT.
      END.   
      if PriceList    ne "" AND Customer.PriceList  NE PriceList   
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      if InvGroup    ne "" AND Customer.InvGroup  NE InvGroup   
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      IF rd-cust-nr NE 0  AND Customer.InvCust NE rd-cust-nr 
      THEN DO:
           RUN pRemoveCGMember.
           NEXT.
      END.
      IF lookup(Customer.Size,sizes) = 0                     
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.
      if lookup(string(Customer.conntype,"D/I"),conns) = 0       
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.
      IF Customer.ContrBeg < cbdate1                          
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.   
      IF Customer.ContrEnd > cbdate2                          
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.
      IF bicheck THEN DO:
         /* has this Customer any Invoices buring desired
            InstDuePeriod of time ? */
         FIND FIRST Invoice WHERE 
                      Invoice.CustNum = Customer.CustNum AND
                      Invoice.InvDate >= bidate1  AND
                      Invoice.InvDate <= bidate2
         no-lock no-error.
         IF NOT AVAIL Invoice                              
         THEN DO:

            RUN pRemoveCGMember.
            NEXT.          
         END.   
      END. 

      if as-ponro   ne "" AND (
           Customer.ZipCode = ? OR
         NOT Customer.ZipCode BEGINS as-ponro)                 
      THEN DO:
         RUN pRemoveCGMember.
         NEXT.
      END.

      /***********************************************
      * This Customer matched WITH given parameters. *
      * NEXT we CREATE a NEW member record FOR it.   *
      ***********************************************/
      FIND CGMember where
           CGMember.CustNum   = Customer.CustNum AND
           CGMember.CustGroup = custgroup.CustGroup NO-LOCK NO-ERROR.

      IF NOT AVAIL cgmember THEN DO:
         CREATE new-cgmember.          
         ASSIGN
         new-CGMember.Brand      = CustGroup.Brand
         new-CGMember.CustNum    = Customer.CustNum
         new-CGMember.CustName   = Customer.CustName
         new-CGMember.CustGroup  = custgroup.CustGroup.

         lcTask = fECGEnterTask(TRUE). 

         RUN StarEventMakeCreateEvent(lhnew-cgmember).
      END.

   END. /* Customer */
END.

QUIT.

PROCEDURE pRemoveCGMember.
/* check FIRST whether this Customer is already a member */
   FIND        CGMember where
               CGMember.CustNum   = Customer.CustNum AND
               CGMember.CustGroup = CustGroup EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL CGMember THEN DO:
      RUN StarEventMakeDeleteEvent(lhCGMember).
      lcTask = fECGLeaveTask(TRUE). 
      DELETE CGMember.
   END.

END PROCEDURE.

