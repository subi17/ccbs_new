from gearbox.migrations import Migration

class AddRatePlan(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RatePlan', area='Sta_Data_128',
                       label='Rating Plan',
                       dump_name='rateplan',
                       desc='Rating plan for invoice target')
        t.column('RatePlan', 'character', format='x(12)', initial='',
                 label='Rating Plan',
                 column_label='RatePlan',
                 help='Rating plan code')
        t.column('RPName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Name of rating plan')
        t.column('Memo', 'character', format='x(60)', initial='',
                 help='Memo text')
        t.column('PNPRatePlan', 'character', format='x(12)', initial='',
                 label='PNP Rating',
                 column_label='PNP',
                 help='Rating plan for PNP')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('RatePlan', ['Brand', 'RatePlan'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('RPName', ['Brand', 'RPName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('RatePlan')

