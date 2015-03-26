from gearbox.migrations import Migration

class AddSMGMember(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('SMGMember', area='Sta_Data_256',
                       label='Members in Salesman Groups',
                       dump_name='smgmembe',
                       desc='Members in Salesman Groups')
        t.column('SmGroup', 'character', mandatory=True, format='x(10)', initial='',
                 label='Group',
                 column_label='Group',
                 help='Salesman Group Code')
        t.column('Salesman', 'character', mandatory=True, initial='',
                 column_label='Salesman',
                 help='Salesman\'s code')
        t.column('SmName', 'character', format='x(30)', initial='',
                 label='Salesman',
                 column_label='Salesman',
                 help='Salesman\'s name')
        t.column('Memo', 'character', format='x(10)', initial='',
                 label='Info',
                 column_label='Info',
                 help='Information about membership')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('Salesman', ['Brand', 'Salesman', 'SmGroup'], area='Sta_Index_2',
                unique=True)
        t.index('SmGroup', ['Brand', 'SmGroup', 'Salesman'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('SmName', ['Brand', 'SmGroup', 'SmName', 'Salesman'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('SMGMember')

