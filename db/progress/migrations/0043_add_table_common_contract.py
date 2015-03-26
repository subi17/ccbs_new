from gearbox.migrations import Migration

class AddContract(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Contract', area='Sta_Data_128',
                       label='Contract',
                       dump_name='contract',
                       desc='Customers\' contracts\
')
        t.column('Contract', 'character', initial='',
                 label='Contract ID',
                 column_label='ContrID')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer\'s number')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Contract valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Contract valid to')
        t.column('CommPerm', 'logical', initial='yes',
                 label='Comm. Permission',
                 column_label='CommPerm',
                 help='Permit commission calculation')
        t.column('Salesman', 'character', initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('Memo', 'character', format='x(60)', initial='',
                 help='Information about contract')
        t.column('HistCont', 'character', initial='',
                 label='History Contract',
                 column_label='History',
                 help='Contract which preceeded this contract')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('CloseDate', 'date', format='99-99-99',
                 label='Closing Date',
                 column_label='Closed',
                 help='Actual date when contract was closed')
        t.column('ContrType', 'integer', format='9', initial='0',
                 label='Contract Type',
                 column_label='Type',
                 help='Contract type')
        t.column('FeeModel', 'character', initial='',
                 label='Fee Model',
                 column_label='FModel',
                 help='Fees that are created when contract is closed prematurely')
        t.index('Contract', ['Brand', 'Contract'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ContrType', ['Brand', 'ContrType', ('CloseDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('FromDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', ('FromDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('FromDate', ['Brand', ('FromDate', 'DESCENDING'), 'CustNum'], area='Sta_Index_2')
        t.index('Salesman', ['Brand', 'Salesman', 'CustNum', ('FromDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('Contract')

