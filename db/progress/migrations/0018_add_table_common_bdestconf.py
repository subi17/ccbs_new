from gearbox.migrations import Migration

class AddBDestConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BDestConf', area='Sta_Data_128',
                       label='BDest Configuration',
                       dump_name='bdestconf',
                       desc='BDest configuration')
        t.column('Brand', 'character', initial='',
                 help='Code of brand')
        t.column('BDCGroup', 'character', format='x(12)', initial='',
                 label='Config Group',
                 column_label='Group',
                 help='Configuration group ID')
        t.column('BDCName', 'character', format='x(30)', initial='',
                 label='Config Description',
                 column_label='Name',
                 help='Configuration description')
        t.column('RateBDest', 'character', mandatory=True, format='x(25)', initial='',
                 label='Rating Destination',
                 column_label='Rate BDest',
                 help='B-number destination used in rating')
        t.column('RateCCN', 'integer', format='>>9', initial='0',
                 label='Rating CCN',
                 column_label='RateCCN')
        t.column('ConfigValue1', 'decimal', decimals=5, format='->>>>>>9.99', initial='0',
                 label='Config Value 1',
                 column_label='Value1',
                 help='Configuration value 1')
        t.column('ConfigValue2', 'decimal', decimals=5, format='->>>>>>9.99', initial='0',
                 label='Config Value 2',
                 column_label='Value2',
                 help='Configuration value 2')
        t.column('ConfigValue3', 'decimal', decimals=5, format='->>>>>>9.99', initial='0',
                 label='Config Value 3',
                 column_label='Value3',
                 help='Configuration value 3')
        t.column('ConfigChar', 'character', format='x(12)', initial='',
                 label='Config Value',
                 column_label='CharValue',
                 help='Configuration value')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Last effective day')
        t.column('GroupType', 'integer', format='>9', initial='0',
                 label='Group Type',
                 column_label='Type',
                 help='Configuration group type')
        t.index('BDCGroup', ['Brand', 'BDCGroup'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('GroupType', ['Brand', 'GroupType', ('ToDate', 'DESCENDING')], area='Sta_Index_2')

    def down(self):
        self.drop_table('BDestConf')

