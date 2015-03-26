from gearbox.migrations import Migration

class AddEventLogConf(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('EventLogConf', area='Sta_Data_64',
                       label='Eventlog Configuration',
                       dump_name='eventlogconf',
                       desc='Eventlog configuration\
')
        t.column('TableName', 'character', mandatory=True, format='X(15)', initial='',
                 label='Table Name',
                 column_label='Table',
                 help='Table name')
        t.column('ConfigType', 'character', format='x(12)', initial='',
                 label='Configuration Type',
                 column_label='Type',
                 help='Configuration type')
        t.column('ConfigValue', 'character', format='x(30)', initial='',
                 label='Configuration Value',
                 column_label='Value',
                 help='Configuration value')
        t.column('FromDate', 'date', format='99-99-99',
                 label='Valid From',
                 column_label='From',
                 help='Valid from')
        t.column('ToDate', 'date', format='99-99-99',
                 label='Valid To',
                 column_label='To',
                 help='Valid to')
        t.index('TableName', ['TableName', 'ConfigType', ('ToDate', 'DESCENDING')], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('EventLogConf')

