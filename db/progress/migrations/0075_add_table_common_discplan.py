from gearbox.migrations import Migration

class AddDiscPlan(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DiscPlan', area='Sta_Data_128',
                       label='Discount Plan',
                       dump_name='discplan')
        t.column('DiscPlan', 'character', format='x(12)', initial='',
                 label='Discount Plan',
                 column_label='Discount Plan',
                 help='Code of a Discount Plan')
        t.column('DPName', 'character', format='x(40)', initial='',
                 column_label='DPName',
                 help='Name of a Discount Plan')
        t.column('Memo', 'character', extent=10, format='x(60)', initial='',
                 label='DiscPlan memo',
                 column_label='DiscPlan memo',
                 help='Memo of a Discount Plan')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('DiscPlan', ['Brand', 'DiscPlan'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DPName', ['Brand', 'DPName', 'DiscPlan'], area='Sta_Index_2')

    def down(self):
        self.drop_table('DiscPlan')

