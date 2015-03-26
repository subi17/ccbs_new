from gearbox.migrations import Migration

class AddPersonnel(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('Personnel', area='Sta_Data_256',
                       label='Personnel User',
                       dump_name='personne',
                       desc='Personnel User')
        t.column('PersCode', 'character', mandatory=True, initial='',
                 label='Person',
                 column_label='Person',
                 help='Person responsible for handling the troubles')
        t.column('PersName', 'character', format='X(30)', initial='',
                 label='Person Name',
                 column_label='Person Name')
        t.column('MobileNbr', 'character', format='X(20)', initial='',
                 label='GSM No.',
                 column_label='GSM No.',
                 help='Mobile No, of person to deliver SMS messages')
        t.column('EMail', 'character', format='X(30)', initial='',
                 label='Email',
                 column_label='Email',
                 help='Email id comtact of person in case of any trouble.')
        t.column('Preferred', 'character', initial='',
                 column_label='Preferred',
                 help='Preferred mode of contact of responsible person',
                 description='Preferred mode of communication\
Valid values may be GSM, Email etc.')
        t.column('Manager', 'character', initial='',
                 column_label='Manager',
                 help='Manger of person responsible for handling the troubles')
        t.column('UserCode', 'character', initial='',
                 label='User',
                 help='User code')
        t.index('perscode', ['PersCode'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('Personnel')

