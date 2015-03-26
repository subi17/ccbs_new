from gearbox.migrations import Migration

class AddCampPrice(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('CampPrice', area='Sta_Data_256',
                       label='CampPrice',
                       dump_name='camppric',
                       desc='Campaign prices\
\
')
        t.column('Campaign', 'character', initial='',
                 label='Campaign ID',
                 column_label='ID')
        t.column('PriceList', 'character', initial='',
                 column_label='Plist',
                 help='Code (identifier) for a Price List')
        t.column('CLI', 'character', format='x(15)', initial='')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Beginning of campaign')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='End of campaign')
        t.column('Brand', 'character', initial='',
                 help='Code Of Brand')
        t.index('Campaign', ['Brand', 'Campaign', 'CLI'], area='Sta_Index_2')
        t.index('CLI', ['Brand', 'CLI', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('CampPrice')

