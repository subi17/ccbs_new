from gearbox.migrations import Migration

class AddSIMStat(Migration):

    dumped_on = 'propus'
    database = 'mobile'

    def up(self):
        t = self.table('SIMStat', area='Sta_Data_128',
                       dump_name='simstat',
                       desc='SIM status codes\
')
        t.column('SimStat', 'integer', format='z9', initial='0',
                 label='Sim Status',
                 column_label='Status',
                 help='SIM Status Code')
        t.column('SSName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name (description) of status')
        t.index('SimStat', ['SimStat'], area='Sta_Index_3',
                primary=True, unique=True)
        t.index('SSName', ['SSName', 'SimStat'], area='Sta_Index_3',
                unique=True)

    def down(self):
        self.drop_table('SIMStat')

