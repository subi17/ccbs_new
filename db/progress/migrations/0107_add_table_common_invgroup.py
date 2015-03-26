from gearbox.migrations import Migration

class AddInvGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('InvGroup', area='Sta_Data_64',
                       label='Invoicing Groups',
                       dump_name='invgroup',
                       desc='Groups for customers which are to be invoiced simultaneously')
        t.column('InvGroup', 'character', initial='',
                 column_label='InvGroup',
                 help='Alphanumeric code for Invoicing Group')
        t.column('IGName', 'character', format='x(40)', initial='',
                 label='Group Name',
                 column_label='Group Name',
                 help='Name of Invoicing Group')
        t.column('xxMemo', 'character', format='x(60)', initial='',
                 label='Memo',
                 column_label='Memo')
        t.column('CompName', 'character', format='x(30)', initial='',
                 label='Company name',
                 column_label='Company name',
                 help='Company name to be printed on call specifications\' headers')
        t.column('Contact', 'character', format='x(30)', initial='',
                 column_label='Contact',
                 help='Name of our contact to be printed on customers\' invoices')
        t.column('InvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='InvNo',
                 column_label='InvNo',
                 help='Group\'s consecutive invoice number')
        t.column('BillPerm', 'logical', initial='Yes',
                 label='Invoiced',
                 column_label='Invoiced',
                 help='Shall this group be invoiced (new calls left unmarked) (Y/N)')
        t.column('CrInvNum', 'integer', format='zzzzzzzz9', initial='0',
                 label='CrInvNo',
                 column_label='CrNo',
                 help='Group\'s consecutive credit invoice number')
        t.column('UpdCustBal', 'logical', initial='Yes',
                 label='Account',
                 column_label='Account',
                 help='Will this group update accounts (Y/N)')
        t.column('MinInvAmt', 'decimal', decimals=2, format='zz9.99', initial='0',
                 label='MinInvoice',
                 column_label='MinInv',
                 help='Billing threshold',
                 description='Minimum amount of invoicing')
        t.column('UnbilledLimit', 'integer', format='>9', initial='0',
                 label='Unpaid months',
                 column_label='Unpaid months',
                 help='After how many months unpaid calls will be invoiced')
        t.column('MinDur', 'integer', format='>9', initial='0',
                 label='MinSec',
                 column_label='MinSec',
                 help='Minimum invoiced call duration in seconds')
        t.column('CollCustNum', 'integer', mandatory=True, format='ZZZZZZZZ9', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer number for collecting too short calls')
        t.column('InvFee', 'decimal', decimals=2, format='zz9.99', initial='0',
                 column_label='InvFee',
                 help='Amount of extra fee for not autogiro customers')
        t.column('InvForm', 'character', format='x(12)', initial='',
                 label='Form Code',
                 column_label='Form Code',
                 help='Invoice Form Code')
        t.column('InvType', 'integer', format='>>>>>9', initial='0',
                 label='Invoice Type',
                 column_label='Type',
                 help='Invoice type')
        t.column('Banned', 'logical', initial='no',
                 column_label='Banned',
                 help='Is group excluded (banned) from invoice run (Yes/No)')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('ddBankAcc', 'character', format='x(30)', initial='',
                 label='Bank Account',
                 column_label='Bank',
                 help='Bank account for direct debit')
        t.column('DDServId', 'character', format='x(12)', initial='',
                 label='DD Service ID',
                 column_label='DD Id',
                 help='Direct debit service id')
        t.column('InvAddress', 'character', format='x(30)', initial='',
                 label='Address',
                 help='Address that is printed to invoice')
        t.column('InvPost', 'character', format='x(30)', initial='',
                 label='Post Office',
                 help='Post office that is printed to invoice')
        t.column('InvPhone1', 'character', format='x(30)', initial='',
                 label='Phone 1',
                 help='Phone number that is printed to invoice')
        t.column('InvPhone2', 'character', format='x(30)', initial='',
                 label='Phone 2',
                 help='Phone number that is printed to invoice')
        t.column('InvPhone3', 'character', format='x(30)', initial='',
                 label='Phone 3',
                 help='Phone number that is printed to invoice')
        t.column('CompanyID', 'character', format='x(20)', initial='',
                 label='Company ID')
        t.column('HomeLocation', 'character', format='x(30)', initial='',
                 label='Home Location',
                 column_label='Home',
                 help='Home location that is printed to invoice')
        t.column('TaxZone', 'character', initial='',
                 label='Tax Zone',
                 column_label='Zone')
        t.index('IGName', ['Brand', 'IGName'], area='Sta_Index_2')
        t.index('InvGroup', ['Brand', 'InvGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('InvGroup')

