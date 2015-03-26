from gearbox.migrations import Migration

class AddAccPeriod(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AccPeriod', area='Sta_Data_256',
                       dump_name='accperio')
        t.column('Period', 'integer', format='999999', initial='0',
                 help='Accounting period')
        t.column('FromDate', 'date', format='99-99-99',
                 label='From',
                 help='Beginning of period')
        t.column('ToDate', 'date', format='99-99-99',
                 label='To',
                 help='End of period')
        t.column('PerLocked', 'logical', format='Locked/Unlocked', initial='no',
                 label='Locked',
                 help='If period is locked then no events can be posted to it.')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('FromDate', ['Brand', 'FromDate'], area='Sta_Index_2',
                unique=True)
        t.index('Period', ['Brand', 'Period'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ToDate', ['Brand', 'ToDate'], area='Sta_Index_2')

    def down(self):
        self.drop_table('AccPeriod')

