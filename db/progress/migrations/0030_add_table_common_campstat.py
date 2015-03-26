from gearbox.migrations import Migration

class AddCampStat(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CampStat', area='Sta_Data_128',
                       label='CampStat',
                       dump_name='campstat',
                       desc='Campaign statistic\
')
        t.column('Campaign', 'character', initial='',
                 label='Campaign ID',
                 column_label='ID')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>9', initial='0',
                 label='Customer',
                 help='Customer\'s number')
        t.column('CLI', 'character', format='x(15)', initial='')
        t.column('CampDate', 'date', format='99-99-99',
                 label='Campaign Date',
                 column_label='Date',
                 help='Date when campaign was used')
        t.index('Campaign', ['Brand', 'Campaign', 'CustNum'], area='Sta_Index_2',
                primary=True)
        t.index('CampCLI', ['Brand', 'Campaign', 'CLI'], area='Sta_Index_2')
        t.index('CLI', ['Brand', 'CLI', ('CampDate', 'DESCENDING')], area='Sta_Index_2')
        t.index('CustNum', ['Brand', 'CustNum', ('CampDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('CampStat')

