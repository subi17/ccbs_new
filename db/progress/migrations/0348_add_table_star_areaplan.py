from gearbox.migrations import Migration

class AddAreaPlan(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('AreaPlan', area='Sta_Data_256',
                       label='AreaPlan',
                       dump_name='areaplan',
                       desc='Traffic area plan')
        t.column('TrafficArea', 'integer', format='z9', initial='0',
                 label='No.',
                 column_label='No.',
                 help='Number of a Traffic Area')
        t.column('AreaName', 'character', format='x(30)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of a Traffic Area')
        t.column('GroupCode', 'integer', format='>9', initial='0',
                 label='Group',
                 column_label='Group',
                 help='Group number, calls within one group are local')
        t.index('AreaName', ['AreaName'], area='Sta_Index_2')
        t.index('TrafficArea', ['TrafficArea'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('AreaPlan')

