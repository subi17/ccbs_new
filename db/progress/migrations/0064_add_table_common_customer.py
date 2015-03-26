from gearbox.migrations import Migration

class AddCustomer(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Customer', area='Sta_Data_32',
                       label='Customer',
                       dump_name='custome1',
                       desc='Customer')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('SearchName', 'character', initial='',
                 label='Abbrev',
                 column_label='Abbrev',
                 help='Alphanumeric key for browsing')
        t.column('InvCust', 'integer', format='ZZZZZZZZ9', initial='0',
                 label='Invoice Cust.',
                 column_label='Inv.Cust',
                 help='Customer being billed')
        t.column('RateCust', 'integer', format='ZZZZZZZZ9', initial='0',
                 label='DiscCust',
                 column_label='DiscCust',
                 help='Customer whose rates and discounts shall be used')
        t.column('PaymCust', 'integer', format='ZZZZZZZZ9', initial='0',
                 label='Paym.resp.',
                 column_label='Paymresp',
                 help='Number of customer who is responsible for payment')
        t.column('RepCust', 'integer', format='ZZZZZZZZ9', initial='0',
                 label='RapCust',
                 column_label='RapCust',
                 help='Customer who receives call report(s)')
        t.column('CustName', 'character', format='x(30)', initial='',
                 label='Customer\'s name',
                 column_label='Customer\'s name')
        t.column('Contact', 'character', format='x(30)', initial='',
                 column_label='Contact',
                 help='Contact name')
        t.column('Phone', 'character', format='x(16)', initial='',
                 label='PhoneNo',
                 column_label='PhoneNo',
                 help='Customer\'s phone number')
        t.column('Fax', 'character', format='x(16)', initial='',
                 column_label='Fax',
                 help='Customer\'s fax no.')
        t.column('Email', 'character', format='x(60)', initial='',
                 column_label='Email',
                 help='Customer\'s Email ID')
        t.column('COName', 'character', format='x(30)', initial='',
                 label='Addt\'l name',
                 column_label='Addt\'l name',
                 help='Additional name of a customer')
        t.column('Address', 'character', format='x(30)', initial='',
                 column_label='Address',
                 help='Customer\'s mailing address (street, p.o. box)')
        t.column('ZipCode', 'character', initial='',
                 label='Postal code',
                 column_label='Postcd',
                 help='Customer\'s postal code')
        t.column('PostOffice', 'character', format='x(24)', initial='',
                 label='Postal Addr.',
                 column_label='Postaddr',
                 help='Customer\'s postal address')
        t.column('Country', 'character', format='x(3)', initial='',
                 column_label='Country',
                 help='Customer\'s country code')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman who is responsible for customer')
        t.column('RepCodes', 'character', format='x(30)', initial='',
                 label='Report Codes',
                 column_label='Reports',
                 help='Report definition')
        t.column('Language', 'integer', format='>>9', initial='0',
                 column_label='Language',
                 help='Customer\'s language code (1 ...999)',
                 valexp='Language > 0',
                 valmsg='Must be 1 ... 9 !')
        t.column('PaymTerm', 'integer', format='>9', initial='0',
                 label='Payment Terms',
                 column_label='Payment Terms',
                 help='Terms of payment (days)')
        t.column('OrgId', 'character', format='x(11)', initial='',
                 label='Pers/Comp.ID',
                 help='Customer\'s organisation ID or personal ID')
        t.column('Category', 'character', format='x(4)', initial='',
                 label='Cat',
                 column_label='Cat',
                 help='Category code, max 4 characters')
        t.column('ContrBeg', 'date', format='99-99-9999',
                 label='Cstart',
                 column_label='Cstart',
                 help='Day when contract was begun')
        t.column('ContrEnd', 'date', format='99-99-9999',
                 label='End Date',
                 column_label='End Date',
                 help='Day when contract was ended')
        t.column('xxMemo', 'character', extent=15, format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo',
                 help='Memo (text)')
        t.column('ClaimPerm', 'logical', initial='yes',
                 label='DueCond',
                 column_label='DueCond',
                 help='Send a payment reminder of the invoice (Y/N)')
        t.column('InterestPerm', 'logical', initial='yes',
                 label='IntCond',
                 column_label='IntCond',
                 help='Charge a overtime interest to customer (Y/N)')
        t.column('CashDiscDate', 'integer', format='z9', initial='0',
                 label='CashDisc',
                 column_label='CashDisc',
                 help='Cash discount')
        t.column('CashDiscPerc', 'decimal', decimals=2, format='z9.9', initial='0',
                 label='CashDisc-%',
                 column_label='CashDisc-%',
                 help='Cash discount percent')
        t.column('PaymMethod', 'integer', format='zzz9-', initial='0',
                 label='MethOfPaym',
                 column_label='MethOfPaym',
                 help='Customer\'s payment behavior (days +/- dueday)')
        t.column('PaymQty', 'integer', format='zzzz9', initial='0',
                 label='AmoOfPay',
                 column_label='AmoOfPay',
                 help='Give payment to invoice')
        t.column('LatestPaym', 'date', format='99-99-99',
                 label='LastPaymDay',
                 column_label='LastPaymDay',
                 help='Previous day when customer has paid ')
        t.column('PriceList', 'character', initial='',
                 label='Price List',
                 column_label='Price List',
                 help='Code (identifier) for a Price List')
        t.column('InvGroup', 'character', initial='',
                 column_label='InvGroup',
                 help='Alphanumeric code for Invoicing Group')
        t.column('CreUser', 'character', initial='',
                 label='Created by',
                 column_label='Created by',
                 help='User who created this customer')
        t.column('Reseller', 'character', initial='',
                 column_label='Reseller',
                 help='An unique code for a reseller; maximum 8 characters')
        t.column('CreDate', 'date', format='99-99-99',
                 label='Created',
                 column_label='Created',
                 help='Date of creation')
        t.column('UpdDate', 'date', format='99-99-99',
                 label='Updated',
                 column_label='Updated',
                 help='Date of latest updation')
        t.column('UpdUser', 'character', initial='',
                 label='Updated by',
                 column_label='Updated by',
                 help='User who updated this customer latest')
        t.column('Size', 'character', mandatory=True, format='x(2)', initial='M',
                 column_label='Size',
                 help='Size of customer: (XL)arge, (L)arge, (M)edium, (S)mall',
                 valexp='lookup(size,"XL,L,M,S,") > 0',
                 valmsg='Answer XL/L/M/S !')
        t.column('ConnType', 'logical', format='Direct/Indirect', initial='no',
                 label='Connection',
                 column_label='Connection',
                 help='Type of subscriber: (D)irect, (I)ndirect')
        t.column('StartCharge', 'logical', format='A/P', initial='Yes',
                 label='Start charge',
                 column_label='Start charge',
                 help='Allow / Prohibit starting charges for this customer (A/P)')
        t.column('VolDisc', 'logical', format='Allow/Prohibit', initial='no',
                 column_label='VolDisc',
                 help='Is volume discount allowed after net rates (A/P) ?')
        t.column('VATUsage', 'integer', format='9', initial='1',
                 label='VAT Usage',
                 column_label='VAT',
                 help='How VAT is calculated for this customer')
        t.column('Currency', 'character', format='x(5)', initial='',
                 column_label='Currency',
                 help='Currency code')
        t.column('VATId', 'character', format='x(16)', initial='',
                 label='VAT Id',
                 column_label='VAT Id')
        t.column('ChgStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Created',
                 column_label='Created',
                 help='Date and Time When Customer Record was last changed',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('ExpStamp', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='Xferred',
                 column_label='Xferred',
                 help='Date and Time when this record was last exported to',
                 description='Time Stamp yyyymmdd.time (sec)')
        t.column('LatestInv', 'integer', format='999999', initial='0',
                 label='InvPeriod',
                 column_label='InvPeriod',
                 help='Period of latest invoice')
        t.column('InvInterval', 'integer', format='>9', initial='0',
                 column_label='InvInterval',
                 help='Interval for invoicing (months)')
        t.column('Pocket', 'integer', format='999999',
                 column_label='Pocket')
        t.column('WInvDisp', 'logical', initial='no',
                 label='Web',
                 column_label='Web',
                 help='Allow displaying in WEB Phone Book')
        t.column('BankAcct', 'character', format='x(16)', initial='',
                 label='BankAccount',
                 column_label='BankAccount',
                 help='Customer\'s Bank  Account No  (for DirectDebit purposes)')
        t.column('AccGrp', 'integer', format='>>9', initial='1',
                 label='Account Group',
                 column_label='AccGrp',
                 help='Sales: 1:inside own country, 2:inside EU, 3:outside EU')
        t.column('DirMark', 'character', format='x(12)', initial='',
                 label='DirectMarketing',
                 column_label='DirectMarketing',
                 help='Direct marketing codes')
        t.column('NewCust', 'logical', initial='no',
                 label='NewCustomer',
                 column_label='NewCustomer',
                 help='New customer')
        t.column('CcLang', 'character', initial='',
                 label='Conv. language',
                 column_label='Lang.',
                 help='Conversation language')
        t.column('MobeMail', 'character', format='x(40)', initial='',
                 label='eMail (Mobile)',
                 column_label='Mob eMail',
                 help='eMail for person in charge of mobile related things')
        t.column('NeteMail', 'character', format='x(40)', initial='',
                 label='eMail (Internet)',
                 column_label='Net eMail',
                 help='eMail for the person in charge of internet related things')
        t.column('DDStatus', 'integer', format='9', initial='0',
                 label='Debit status',
                 column_label='Debit status',
                 help='DirDebitStatus: 0:No DD  1:New  2:Sent to bank  3:Confirmed')
        t.column('BankId', 'character', format='X(10)', initial='',
                 column_label='BankId',
                 help='Identification code for bank')
        t.column('ChargeType', 'integer', format='9', initial='1',
                 label='Charge Type',
                 column_label='Charge',
                 help='Customers default charging type',
                 description='3=netgiro')
        t.column('HonTitle', 'character', format='x(16)', initial='',
                 label='Title',
                 column_label='Title',
                 help='Customer honorary title (printed f.ex. into invoice)')
        t.column('DelType', 'integer', format='9', initial='0',
                 label='Delivery Type',
                 column_label='Del.Type',
                 help='Customers invoice delivery type')
        t.column('InvCode', 'integer', format='99', initial='0',
                 label='Invoice Code',
                 column_label='Invoice Code',
                 help='Code for week / day of a month for used invoice run (99)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CustClass', 'integer', format='9', initial='0',
                 label='Class',
                 column_label='Class',
                 help='Customer Class (depend on avg. amount of invoices)')
        t.column('VATIncl', 'logical', format='Included/Excluded', initial='yes',
                 label='VAT Included',
                 column_label='VATIncl',
                 help='Format for invoices; is VAT included or excluded in amounts')
        t.column('AgrCust', 'integer', format='>>>>>>>9', initial='0',
                 label='Agreement Customer',
                 column_label='Agr.Cust',
                 help='Agreement customer')
        t.column('IDelName', 'character', format='x(30)', initial='',
                 label='Delivery Name',
                 column_label='Del.Name',
                 help='Invoice delivery name')
        t.column('CreditLimit', 'decimal', decimals=2, format='->>>>>>>9.99', initial='0',
                 label='Credit Limit',
                 column_label='CredLimit',
                 help='Credit limit')
        t.column('IDelAddr', 'character', format='x(30)', initial='',
                 label='Delivery Address',
                 column_label='Del.Address',
                 help='Invoice delivery address')
        t.column('SpecDel', 'integer', format='9', initial='0',
                 label='Specification Delivery',
                 column_label='Spec Del.',
                 help='Specification report delivery type')
        t.column('IDelZipCode', 'character', initial='',
                 label='Delivery Zip Code',
                 column_label='Del.ZipCode',
                 help='Invoice delivery zip code')
        t.column('IDelPost', 'character', format='x(30)', initial='',
                 label='Delivery Post Office',
                 column_label='Del.Post',
                 help='Invoice delivery post office')
        t.column('IDelCountry', 'character', format='x(30)', initial='',
                 label='Delivery Country',
                 column_label='Del.Country',
                 help='Invoice delivery country')
        t.column('IDelCOName', 'character', format='x(30)', initial='',
                 label='Delivery Add.Name',
                 column_label='Del.AName',
                 help='Additional invoice delivery name')
        t.column('SMSNumber', 'character', format='x(15)', initial='',
                 label='SMS Number',
                 column_label='SMS',
                 help='Mobile number for SMS messages')
        t.column('Roles', 'character', format='x(3)', initial='',
                 column_label='Role',
                 help='Customer\'s roles (agreement, invoicing, user)')
        t.column('FirstName', 'character', format='x(20)', initial='',
                 label='Forename',
                 column_label='Forename',
                 help='Customer\'s forename')
        t.column('DirMarkSMS', 'logical', initial='no',
                 label='Direct Marketing Via SMS',
                 column_label='Dir.Mark.SMS',
                 help='Direct marketing using SMS')
        t.column('DirMarkEmail', 'logical', initial='no',
                 label='Direct Marketing Via Email',
                 column_label='Dir.Mark.Email',
                 help='Direct marketing using Email')
        t.column('DirMarkPost', 'logical', initial='no',
                 label='Direct Marketing Via Post',
                 column_label='Dir.Mark.Post',
                 help='Direct marketing using post')
        t.column('OutMarkPost', 'logical', initial='no',
                 label='Out. Marketing Via Post',
                 column_label='Out.Mark.Post',
                 help='3rd part marketing using post')
        t.column('OutMarkSMS', 'logical', initial='no',
                 label='Out. Marketing Via SMS',
                 column_label='Out.Mark.SMS',
                 help='3rd party marketing using SMS')
        t.column('OutMarkEmail', 'logical', initial='no',
                 label='Out. Marketing Via Email',
                 column_label='Out.Mark.Email',
                 help='3rd party marketing using eMail')
        t.column('PContractPIN', 'character', initial='',
                 label='Periodical Contract PIN',
                 column_label='PC PIN',
                 help='PIN for periodical contract')
        t.column('EndInvType', 'integer', format='9', initial='0',
                 label='End Invoice Type',
                 column_label='EndInv.Type',
                 help='End invoice type')
        t.column('EndInvDate', 'date', format='99-99-99',
                 label='End Invoice Date',
                 column_label='EndInvRun',
                 help='Date when end invoice run was performed')
        t.column('CustIdType', 'character', initial='',
                 label='Customer ID Type',
                 column_label='ID Type',
                 help='Customer ID type')
        t.column('Nationality', 'character', initial='',
                 column_label='Nation.')
        t.column('SurName2', 'character', format='x(30)', initial='',
                 label='Second Surname',
                 column_label='2.Surname',
                 help='Second surname')
        t.column('Sex', 'character', initial='')
        t.column('Region', 'character', initial='',
                 help='Region code')
        t.column('RobinsonsLimit', 'character', initial='',
                 label='Robinsons Limit',
                 column_label='Rob.Limit',
                 help='Robinsons limit')
        t.column('MobileNumber', 'character', format='x(16)', initial='',
                 label='Mobile Contact Number',
                 column_label='Mob.Nbr',
                 help='Mobile contact number')
        t.column('Birthday', 'date', format='99-99-99')
        t.column('CompanyName', 'character', format='x(30)', initial='',
                 label='Company Name',
                 column_label='Company',
                 help='Company name')
        t.column('AddressCodC', 'character', initial='',
                 label='Address CodC',
                 column_label='CodC',
                 help='CodC in address validation')
        t.column('AddressCodP', 'character', initial='',
                 label='Address CodP',
                 column_label='CodP',
                 help='CodP in address validation')
        t.column('FoundationDate', 'date', format='99-99-99',
                 label='Foundation Date',
                 column_label='Found.Date')
        t.column('ExtInvRef', 'character', format='X(50)', initial='',
                 label='External InvRef',
                 column_label='ExtInvRef',
                 help='External Invoice Reference',
                 description='External invoice reference for corporate customers')
        t.column('DataProtected', 'logical', initial='No',
                 label='Data Protected',
                 column_label='DataProtected',
                 help='Is the data protection required by customer')
        t.index('AgrCust', ['AgrCust'], area='Sta_Index_1')
        t.index('BankAcct', ['Brand', 'BankAcct'], area='Sta_Index_1')
        t.index('CompanyName', ['Brand', 'CompanyName'], area='Sta_Index_1')
        t.index('CustName', ['Brand', 'CustName', 'FirstName'], area='Sta_Index_1')
        t.index('CustNum', ['Brand', 'CustNum'], area='Sta_Index_1')
        t.index('CustNum_s', ['CustNum'], area='Sta_Index_1',
                primary=True, unique=True)
        t.index('InvCode', ['Brand', 'InvCode', 'CustNum'], area='Sta_Index_1')
        t.index('InvCust', ['InvCust'], area='Sta_Index_1')
        t.index('InvGroup', ['Brand', 'InvGroup', 'CustNum'], area='Sta_Index_1')
        t.index('Orgid', ['Brand', 'OrgId'], area='Sta_Index_1')
        t.index('RateCust', ['RateCust'], area='Sta_Index_1')
        t.index('RepCust', ['RepCust'], area='Sta_Index_1')
        t.index('SearchName', ['Brand', 'SearchName', 'CustName'], area='Sta_Index_1')
        t.index('SurName2', ['Brand', 'SurName2', 'CustName', 'FirstName'], area='Sta_Index_1')
        t.index('ZipCode', ['Brand', 'ZipCode', 'CustNum'], area='Sta_Index_1')

    def down(self):
        self.drop_table('Customer')

