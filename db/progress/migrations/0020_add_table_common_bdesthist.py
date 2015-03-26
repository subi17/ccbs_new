from gearbox.migrations import Migration

class AddBDestHist(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BDestHist', area='Sta_Data_256',
                       dump_name='bdesthis')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-number')
        t.column('vFrom', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ValidFrom',
                 column_label='ValidFrom',
                 help='Valid from')
        t.column('vTo', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ValidTo',
                 column_label='ValidTo')
        t.column('CustNum', 'integer', format='zzzzzzzz', initial='0',
                 label='CustNo',
                 column_label='CustNo',
                 help='Customer number')
        t.column('BillTarget', 'integer', format='z9', initial='0',
                 label='Bill Target',
                 column_label='BT',
                 help='Customer\'s Billing Target')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('BDest', ['Brand', 'BDest', 'vFrom', 'vTo'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CustNum', ['Brand', 'CustNum', 'BillTarget', 'BDest', 'vFrom', 'vTo'], area='Sta_Index_2')
        t.index('CustNum_s', ['CustNum', 'BillTarget', 'BDest', 'vFrom', 'vTo'], area='Sta_Index_2')

    def down(self):
        self.drop_table('BDestHist')

