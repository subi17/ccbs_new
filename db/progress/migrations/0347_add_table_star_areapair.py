from gearbox.migrations import Migration

class AddAreaPair(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('AreaPair', area='Sta_Data_256',
                       label='AreaPair',
                       dump_name='areapair',
                       desc='List of "neighbour" areas of a certain area')
        t.column('AreaCode', 'character', format='x(4)', initial='',
                 column_label='AreaCode',
                 help='A-subscriber\'s areanumber')
        t.column('NeigArea', 'character', format='x(4)', initial='',
                 label='X-!areaNo',
                 column_label='X-!areaNo',
                 help='Surrounding area')
        t.index('AreaCode', ['AreaCode', 'NeigArea'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('NeigArea', ['NeigArea', 'AreaCode'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('AreaPair')

