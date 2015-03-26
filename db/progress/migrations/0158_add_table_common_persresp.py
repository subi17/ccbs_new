from gearbox.migrations import Migration

class AddPersResp(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PersResp', area='Sta_Data_256',
                       label='Responsible Personnel',
                       dump_name='persresp',
                       desc='Alternate Personnels responsible for handling the troubles in case main personnel is not available')
        t.column('PersCode', 'character', mandatory=True, initial='',
                 label='Person',
                 column_label='Person')
        t.column('Level', 'character', format='X(10)', initial='',
                 column_label='Level',
                 help='Level of the alternate responsible person',
                 description='Level of the alternate responsible person')
        t.column('PersKey', 'character', initial='',
                 label='Key',
                 column_label='Key',
                 help='Value of Person, Category or Type depending on level',
                 description='Value of Person, Category or Type depending on level')
        t.column('Memo', 'character', format='X(60)', initial='',
                 column_label='Memo',
                 help='verbal explanation if needed',
                 description='verbal explanation if needed')
        t.column('Priority', 'integer', format='9', initial='0',
                 column_label='Priority',
                 help='Priority (1-9)',
                 description='Priority (1-9)')
        t.index('perslvlkey', ['PersCode', 'Level', 'PersKey'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PersResp')

