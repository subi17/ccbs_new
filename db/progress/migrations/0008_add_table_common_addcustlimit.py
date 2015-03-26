from gearbox.migrations import Migration

class AddAddCustLimit(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AddCustLimit', area='Sta_Data_256',
                       dump_name='addcustl',
                       desc='Increase customer\'s credit limit temporarily')
        t.column('CustNum', 'integer', format='zzzzzzzzz', initial='0',
                 label='Customer',
                 column_label='Cust',
                 help='Customer Number')
        t.column('dTo', 'date', format='99-99-99',
                 label='DateTo',
                 column_label='DateTo',
                 help='Last extra limit day')
        t.column('Amount', 'decimal', decimals=2, format='z,zz9.99', initial='0',
                 help='Amount of Extra Customer limit')
        t.column('Memo', 'character', format='X(50)', initial='',
                 column_label='Memo',
                 help='Reason for Extra Customer Limit')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CustNum', ['Brand', 'CustNum', ('dTo', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', ('dTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('AddCustLimit')

