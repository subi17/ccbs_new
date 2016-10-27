/* xxMobsub.frm

   frame lis for xxMobsub.p
   
   changes:         30.12.04/aam llSecret, lcNumberInquiry, liCreditType,   
                                           lcRepCode
                    07.02.06/aam lcPerContr
                    15.02.06/vk  longer format for notify nbr
                    27.11.06/aam Reseller
   
*/
                                                                     /*
DEF VAR ldaPortingDate      AS DATE                 NO-UNDO FORMAT "99-99-99".
DEF VAR liPortingTime       AS DEC                   NO-UNDO FORMAT "99.99".
DEF VAR lcorderstamp        AS CHAR    No-UNDO FORMAT "X(19)".
DEF VAR liAgreementCustomer AS INT     NO-UNDO FORMAT ">>>>>>>>9".
DEF VAR lcAgreementname     AS CHAR    NO-UNDO FORMAT "X(25)".
DEF VAR ldsaldoacc          AS DEC     NO-UNDO FORMAT ">>>9.99". 
DEF VAR llSecret            AS LOG     NO-UNDO.
DEF VAR liCreditType        AS INT     NO-UNDO FORMAT "9". 
DEF VAR liSaldoLimit        AS INT     NO-UNDO FORMAT "->>>>>>9".
DEF VAR lcRepCode           AS CHAR    NO-UNDO.                        */

DEF VAR liSaldoLimit        AS INT     NO-UNDO FORMAT "->>>>>>9".

DEF VAR ldaPortingDate      AS DATE                 NO-UNDO FORMAT "99-99-99".
DEF VAR liPortingTime       AS DEC                   NO-UNDO FORMAT "99.99".
DEF var disp-def-sp-code    AS CHAR   NO-UNDO.
DEF VAR lcStatus            AS CHAR    NO-UNDO FORMAT "X(25)" .
DEF VAR llFatime            AS LOG     No-UNDO FORMAT "YES/NO" .
DEF VAR llEGift             AS LOG     NO-UNDO FORMAT "YES/NO".
DEF VAR llCallSpec          AS LOG     NO-UNDO FORMAT "YES/NO".
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
DEF VAR lcPerContr          AS CHAR    NO-UNDO FORMAT "X(3)".
DEF VAR lcTerminated        AS CHAR    NO-UNDO FORMAT "X(48)" .

FORM
"SubscriptionID:"   TermMobSub.msseq   "CliType...:" AT 50 TermMobSub.clitype     SKIP
"MSISDN........:"   TermMobSub.cli FORMAT "x(30)" "BillTarget:" AT 50 lcBillTarget      SKIP
"ICC/SIM.......:"   TermMobSub.icc     "PUK1......:" AT 50 Imsi.Puk1         SKIP
"Subscr.Status.:"   lcStatus           "PUK2......:" AT 50 Imsi.Puk2             SKIP
"Number Inquiry:"   lcNumberInquiry    "PIN1......:" AT 50 IMSI.PIN1         SKIP
                                       "PIN2......:" AT 50 IMSI.PIN2         SKIP 
"Agr.Customer..:"    TermMobSub.AgrCust  AgrCustomer.CustName FORMAT "X(22)"        SKIP
"PersID/Comp.ID:"    AgrCustomer.OrgID                                    
                                       "FAT.......:" AT 50  llFatime        SKIP
"Inv.Customer..:"    TermMobSub.InvCust  InvCustomer.CustName FORMAT "X(22)"  
                                        "eGift ....:" AT 50  lleGift         SKIP
"User .........:"   TermMobSub.Custnum  UserCustomer.CustName FORMAT "X(22)"   
                                        "CallSpec..:" AT 50  llCallSpec      SKIP
"ServiceChanges:"    TermMobSub.servicechanges 
   HELP "0=Denied 1=Allowed"
   lcServiceChanges              
                                        "Per.Contr.:" AT 50 lcPerContr     SKIP
lcTerminated                                                               SKIP
"Reseller .....:"    TermMobSub.Reseller  Reseller.RsName FORMAT "X(15)"   SKIP
"Salesman......:"    TermMobSub.SalesMan  Salesman.smname FORMAT "X(15)"   SKIP
"CreationDate..:"    TermMobSub.CreationDate FORMAT "99-99-9999" 
                                                                           SKIP
"Activation....:"    TermMobSub.ActivationDate      FORMAT "99-99-9999"    SKIP
lcInportTime 
WITH  OVERLAY ROW 1 centered SIDE-LABELS
TITLE COLOR VALUE(ctc) " " + ynimi +
 "  TERMINATED MOBILE SUBSCRIPTION  "
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
   
         
