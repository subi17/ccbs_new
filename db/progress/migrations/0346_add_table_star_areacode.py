from gearbox.migrations import Migration

class AddAreaCode(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('AreaCode', area='Sta_Data_256',
                       label='AreaCode',
                       dump_name='areacode',
                       desc='Numbering areas')
        t.column('TrafficArea', 'integer', format='z9', initial='0',
                 label='No ',
                 column_label='Nro',
                 help='Consecutive Number of a Traffic Area')
        t.column('AreaCode', 'character', format='x(4)', initial='',
                 column_label='AreaCode',
                 help='Area Code')
        t.column('POI', 'logical', initial='no',
                 label='AP',
                 column_label='AP',
                 help='Is there an point of interconnection in this area')
        t.column('AreaName', 'character', format='x(30)', initial='',
                 label='Place/City',
                 column_label='Place/City',
                 help='Name of place/city')
        t.column('Local', 'logical', initial='no',
                 label='Local Segment',
                 column_label='Local Segment',
                 help='"Shall calls from/to this area treated as ""local segment"" Y/N"')
        t.column('TrunkCode', 'character', initial='',
                 label='CGR',
                 column_label='CGR',
                 help='Circuit Group Code for this ICP (point of interconnection)')
        t.index('AreaCode', ['AreaCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('AreaName', ['AreaName', 'AreaCode'], area='Sta_Index_2')
        t.index('POI', ['TrafficArea', 'POI'], area='Sta_Index_2')
        t.index('TrafficArea', ['TrafficArea', 'AreaCode'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('AreaCode')

