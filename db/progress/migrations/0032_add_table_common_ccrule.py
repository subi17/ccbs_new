from gearbox.migrations import Migration

class AddCCRule(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CCRule', area='Sta_Data_256',
                       label='Cost Centre Rules',
                       dump_name='ccrule',
                       desc='Rules for determining cost centre\
')
        t.column('Category', 'character', format='x(4)', initial='',
                 column_label='Category',
                 help='Customer category code')
        t.column('BIGroup', 'character', initial='',
                 label='Product group',
                 column_label='Prod.grp')
        t.column('ValidFrom', 'date', format='99-99-99',
                 label='Effective date',
                 column_label='Eff.date',
                 help='Date when rule becomes effective')
        t.column('CostCentre', 'character', initial='',
                 label='Cost Centre',
                 column_label='Cc',
                 help='Cost centre to be used')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Category', ['Brand', 'Category', 'BIGroup', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('Pgcode', ['Brand', 'BIGroup', 'Category', ('ValidFrom', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('CCRule')

