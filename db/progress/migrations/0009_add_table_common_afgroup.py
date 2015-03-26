from gearbox.migrations import Migration

class AddAFGroup(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('AFGroup', area='Sta_Data_256',
                       dump_name='actiongr')
        t.column('ActionType', 'integer', initial='0',
                 label='Action Type',
                 column_label='Type',
                 help='Action type')
        t.column('Brand', 'character', initial='')
        t.column('CLIType', 'character', format='X(20)', initial='',
                 label='CLI Type',
                 column_label='CLIType',
                 help='CLI type')
        t.column('Limit', 'integer', format='->>>>>>9', initial='0')
        t.column('TimeUnit', 'integer', format='->>>>>>9', initial='0',
                 label='Time Unit',
                 column_label='Unit',
                 help='Time unit')
        t.column('UnitCount', 'integer', initial='0',
                 label='Unit Count',
                 column_label='Count',
                 help='Unit count')
        t.index('ActionType', ['Brand', 'ActionType', 'CLIType'], area='Sta_Index_2',
                primary=True)
        t.index('CLIType', ['Brand', 'CLIType'], area='Sta_Index_2')

    def down(self):
        self.drop_table('AFGroup')

