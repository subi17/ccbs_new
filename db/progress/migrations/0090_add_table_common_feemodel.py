from gearbox.migrations import Migration

class AddFeeModel(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('FeeModel', area='Sta_Data_64',
                       label='Billing Event',
                       dump_name='bevent',
                       desc='A group of single and/or periodical fees')
        t.column('FeeModel', 'character', initial='',
                 label='BEvent',
                 column_label='BEvent',
                 help='An unique code for a Billing Event')
        t.column('FeeName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of Billing Case')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.column('FMGroup', 'integer', format='>>9', initial='0',
                 label='Group',
                 column_label='Group',
                 help='FeeModel Group')
        t.index('FeeGroup', ['Brand', 'FMGroup'], area='Sta_Index_2')
        t.index('FeeModel', ['Brand', 'FeeModel'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('FeeName', ['Brand', 'FeeName', 'FeeModel'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('FeeModel')

