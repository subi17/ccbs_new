from gearbox.migrations import Migration

class AddTMSCodes(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('TMSCodes', area='Sta_Data_64',
                       label='Selection Lists',
                       dump_name='tmscodes',
                       desc='General selection list values in TMS application')
        t.column('TableName', 'character', mandatory=True, format='X(15)', initial='',
                 label='Table Name',
                 column_label='Table Name',
                 help='Database Table Name',
                 description='DB table, field of which is using pre-defined values.')
        t.column('FieldName', 'character', mandatory=True, format='X(20)', initial='',
                 label='Field Name',
                 column_label='FieldName',
                 help='Field Name in the table',
                 description='Name of the Field in the table which is using pre-defined values.')
        t.column('CodeGroup', 'character', format='X(10)', initial='',
                 label='Group',
                 column_label='Group',
                 help='Group Name for pre-defined values',
                 description='Group in which pre-defined values can be categorised.')
        t.column('CodeValue', 'character', initial='',
                 label='Value',
                 column_label='Value',
                 help='Possible pre-defined value')
        t.column('CodeName', 'character', format='X(50)', initial='',
                 label='Description',
                 column_label='Description',
                 help='Description/Name for pre-defined value')
        t.column('Memo', 'character', format='X(60)', initial='',
                 column_label='Memo',
                 help='Detailed description for pre-defined values')
        t.column('ConfigValue', 'character', format='x(20)', initial='',
                 label='Configuration Value',
                 column_label='Config',
                 help='Configuration value')
        t.column('InUse', 'integer', format='9', initial='0',
                 label='Code In Use',
                 column_label='Used',
                 help='Is code in use')
        t.index('CodeGroup', ['CodeGroup', 'FieldName'], area='Sta_Index_2')
        t.index('TableName', ['TableName', 'FieldName', 'CodeValue'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('TMSCodes')

