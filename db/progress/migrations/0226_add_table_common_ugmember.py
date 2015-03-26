from gearbox.migrations import Migration

class AddUGMember(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UGMember', area='Sta_Data_128',
                       label='User Group Members',
                       dump_name='ugmember',
                       desc='User Group Member records;  join Users&Usergroups')
        t.column('UserGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode',
                 help='Individual Code for a User Group')
        t.column('UserCode', 'character', mandatory=True, initial='',
                 label='User ID',
                 column_label='User ID',
                 help='User\'s ID')
        t.column('UserName', 'character', format='x(30)', initial='',
                 label='User\'s name',
                 column_label='User\'s name')
        t.column('Memo', 'character', format='x(10)', initial='',
                 label='Info',
                 column_label='Info',
                 help='Additional coded information about membership')
        t.index('UserCode', ['UserCode', 'UserGroup'], area='Sta_Index_2',
                unique=True)
        t.index('UserGroup', ['UserGroup', 'UserCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('UserName', ['UserGroup', 'UserName', 'UserCode'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('UGMember')

