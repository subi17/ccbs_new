from gearbox.migrations import Migration

class AddCostCentre(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CostCentre', area='Sta_Data_256',
                       label='Cost Centre',
                       dump_name='costcent',
                       desc='Lowest level of cost accounting\
')
        t.column('CostCentre', 'character', initial='',
                 label='Cost Centre',
                 column_label='Cc')
        t.column('CCName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Name of the cost centre')
        t.column('Department', 'character', initial='',
                 column_label='Dep.',
                 help='Department of the cost centre')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('CCN', ['Brand', 'CCName'], area='Sta_Index_2')
        t.index('CostCentre', ['Brand', 'CostCentre'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CostCentre')

