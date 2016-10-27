/* xxMobsub.frm

   frame lis for xxMobsub.p
   
   changes:         30.12.04/aam llSecret, lcNumberInquiry, liCreditType,   
                                           lcRepCode
                    07.02.06/aam lcPerContr
                    15.02.06/vk  longer format for notify nbr
                    27.11.06/aam Reseller
                    07.11.07/jt  layout changes and barring status
   
*/

DEF VAR liSaldoLimit        AS INT     NO-UNDO FORMAT "->>>>>>9".

DEF VAR ldaPortingDate      AS DATE                 NO-UNDO FORMAT "99-99-99".
DEF VAR liPortingTime       AS DEC                   NO-UNDO FORMAT "99.99".
DEF var disp-def-sp-code    AS CHAR   NO-UNDO.
DEF VAR lcStatus            AS CHAR    NO-UNDO FORMAT "X(25)" .
DEF VAR lcSaldoType         AS CHAR    NO-UNDO FORMAT "X(13)" .
DEF VAR liSaldoAmount       AS DEC     No-UNDO.
DEF VAR ldeSaldoSCounter    AS DEC     NO-UNDO.
DEF VAR lcNotifyNumber      AS CHAR    NO-UNDO FORMAT "X(12)".
DEF VAR ldExtraLimit        AS DEC     NO-UNDO.
DEF VAR lcServiceChanges    AS CHAR    NO-UNDO FORMAT "X(16)" .
DEF VAR lcInportTime        AS CHAR    NO-UNDO FORMAT "X(49)" .
DEF VAR lcNumberInquiry     AS CHAR    NO-UNDO FORMAT "X(12)" .
DEF VAR lcActivated         AS CHAR    NO-UNDO FORMAT "X(32)" .
DEF VAR lcBillTarget        AS CHAR    NO-UNDO FORMAT "X(12)" .
DEF VAR lcMultiSIM          AS CHAR    NO-UNDO FORMAT "X(28)". 
DEF VAR lcClitext           AS CHAR    NO-UNDO FORMAT "X(16)".

FORM
"SubscriptionID:"   Mobsub.msseq   "CliType...:" AT 50 lcCliText NO-LABEL  SKIP
"MSISDN........:"   Mobsub.cli FORMAT "x(30)" "BillTarget:" AT 50 lcBillTarget      SKIP
"ICC/SIM.......:"   Mobsub.icc      "PUK1......:" AT 50 Imsi.Puk1         SKIP
"Subscr.Status.:"   lcStatus FORMAT "x(30)" "PUK2......:" AT 50 Imsi.Puk2         SKIP
"Barring Mask..:"   lcBarrStat FORMAT "x(62)"                             SKIP 
lcPCLB FORMAT "x(47)" "PIN1......:" AT 50 IMSI.PIN1   SKIP
"Number Inquiry:"   lcNumberInquiry "PIN2......:" AT 50 IMSI.PIN2         SKIP 
"Agr.Customer..:"    Mobsub.AgrCust  AgrCustomer.CustName FORMAT "X(22)"  lcDSSInfo FORMAT "x(25)" AT 50 SKIP
"PersID/Comp.ID:"    AgrCustomer.OrgID  lcMNP FORMAT "x(28)" AT 50        SKIP
"Inv.Customer..:"    Mobsub.InvCust  InvCustomer.CustName FORMAT "X(22)"  SKIP
"User .........:"   Mobsub.Custnum  UserCustomer.CustName FORMAT "X(22)" 
   lcMultiSim SKIP

 lcLine  /* _________________________ divided ___________________*/   
SKIP(0)
"CreationDate..:"  Mobsub.CreationDate FORMAT "99-99-9999"
"PayType.......:" AT 46 mobsub.paytype                                    
SKIP

"Activation....:" MObsub.ActivationDate FORMAT "99-99-9999"     
"Reseller .....:" AT 46 MobSub.Reseller FORMAT "x(15)" 
SKIP

lcInPortTime FORMAT "x(40)" 
"Salesman......:" AT 46 Mobsub.SalesMan FORMAT "x(15)"
SKIP(1)
WITH  OVERLAY ROW 1 centered SIDE-LABELS
TITLE COLOR VALUE(ctc) " " + ynimi +
 "  MOBILE SUBSCRIPTION  "
 + string(pvm,"99-99-99") + " "
 NO-LABELS 
FRAME lis.


form
SKIP(1)
"NOTE: Default service profile for this subscription "            SKIP
"      is " disp-def-sp-code servpac.spname                      SKIP(1)
"      Insert new profile code OR accept default     "            SKIP
"      profile with enter."                                       skip(1)
"      Activated profile can be checked from SoLog."              skip(1)
"      Service Profile Code:"  def-sp-code NO-LABEL
 HELP "Press F9 for help"
 validate(CAN-FIND(FIRST servpac where
                         servpac.Brand = gcBrand AND 
          servpac.servpac = def-sp-code),"Unknown service profile")
                   servpac.spname                                       SKIP
 
 WITH OVERLAY CENTERED row 5  TITLE COLOR VALUE(ctc) " SERVICE PROFILE "
 COLOR VALUE(cfc) NO-labels  FRAME profi.
 
 
FORM
   "Set new porting time for this subscription?" SKIP(1)
   "         Date :" ldaPortingDate            SKIP
   "         Time :" liPortingTime            SKIP(1)

   WITH OVERLAY CENTERED ROW 10 TITLE " PORTING TIME " NO-LABELS
   FRAME portframe.
   
         
