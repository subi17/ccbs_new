from gearbox.migrations import Migration

class AddcfoSer(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('cfoSer', area='Sta_Data_256')
        t.column('CliFrom', 'character', format='x(12)', initial='',
                 label='B-Sub From',
                 column_label='B-Sub From',
                 help='B-Number series lower limit')
        t.column('CliTo', 'character', format='x(12)', initial='',
                 label='B-sub To',
                 column_label='B-sub To',
                 help='B-Number series upper limit')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('Memo', 'character', format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='Valid From',
                 help='The date FROM which this CFO series will be used.')
        t.column('ValidTo', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='Valid To',
                 help='The date TO (until) which this CFO Series will be used.')
        t.index('b-nr', ['Brand', 'CliFrom', 'CliTo', ('ValidTo', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('cfoSer')

