from gearbox.migrations import Migration

class AddRateCCN(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RateCCN', area='Sta_Data_256',
                       dump_name='rateccn')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-subNo',
                 column_label='B-subNo',
                 help='B-number')
        t.column('CCN', 'integer', mandatory=True, format='zz9', initial='0',
                 column_label='CCN',
                 help='Call case number')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 label='Dialling Type',
                 column_label='DT',
                 help='Dialling type code')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('BDest', ['Brand', 'BDest', 'DialType'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('CCN', ['Brand', 'BDest', 'CCN'], area='Sta_Index_2')

    def down(self):
        self.drop_table('RateCCN')

