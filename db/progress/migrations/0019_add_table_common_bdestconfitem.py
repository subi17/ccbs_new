from gearbox.migrations import Migration

class AddBDestConfItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BDestConfItem', area='Sta_Data_128',
                       label='BDest Configuration Item',
                       dump_name='bdestconfitem',
                       desc='BDest configuration item')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('BDCGroup', 'character', format='x(12)', initial='',
                 label='Config Group',
                 column_label='Group',
                 help='Configuration group ID')
        t.column('BDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='B-nbr Destination',
                 column_label='BDest',
                 help='B-number destination')
        t.column('RateCCN', 'integer', format='>>9', initial='0',
                 label='Rating CCN',
                 column_label='RateCCN')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Last effective day')
        t.column('BDCItemID', 'integer', format='>>>>>>>>>9', initial='0',
                 label='Item ID',
                 column_label='ID',
                 help='Unique item ID')
        t.index('BDCGroup', ['Brand', 'BDCGroup'], area='Sta_Index_2',
                primary=True)
        t.index('BDCItemID', ['BDCItemID'], area='Sta_Index_2',
                unique=True)
        t.index('BDest', ['Brand', 'BDest', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('BDestConfItem')

