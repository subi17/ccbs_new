from gearbox.migrations import Migration

class AddASubType(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('ASubType', area='Sta_Data_256',
                       label='ASub Type',
                       dump_name='asubtype',
                       desc='Types of a-subscriber (mobile, fixed, voip)\
')
        t.column('ASubType', 'integer', format='9', initial='0',
                 label='A-Type',
                 column_label='A-Type',
                 help='ASUB (Dialler) type')
        t.column('ASubName', 'character', format='x(30)', initial='',
                 label='Name',
                 help='Description of the a-subscriber type')
        t.index('ASubType', ['ASubType'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('ASubType')

