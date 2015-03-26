from gearbox.migrations import Migration

class AddUserGrp(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('UserGrp', area='Sta_Data_64',
                       label='User Groups',
                       dump_name='usergrp',
                       desc='User Groups')
        t.column('UserGroup', 'character', format='x(10)', initial='',
                 label='GroupCode',
                 column_label='GroupCode',
                 help='Individual Code for a User Group')
        t.column('UGName', 'character', format='x(40)', initial='',
                 label='Name',
                 column_label='Name',
                 help='Name of user group')
        t.column('CreDate', 'date', format='99-99-99', initial='today',
                 label='Created',
                 column_label='Created',
                 help='Date When Group Was Created')
        t.column('ChgDate', 'date', format='99-99-99',
                 label='Changed',
                 column_label='Changed',
                 help='Date when Group and/or its members were changed')
        t.column('CreUser', 'character', initial='',
                 label='Created by',
                 column_label='Created by',
                 help='User who created this group')
        t.column('ChgUser', 'character', initial='',
                 label='Changed by',
                 column_label='Changed by',
                 help='User who changed/updated this group latest')
        t.column('Memo', 'character', extent=15, format='x(60)', initial='',
                 column_label='Memo',
                 help='Memo text')
        t.column('ShowTokens', 'character', format='x(50)', initial='',
                 label='Show Tokens',
                 help='Comma separed list of Token codes')
        t.column('ModifyTokens', 'character', format='x(50)', initial='',
                 label='Modify Tokens',
                 help='Comma separed list of Token codes')
        t.column('PasswordExpires', 'logical', initial='Yes',
                 label='Password expires',
                 help='Shall the passwords of this group expire')
        t.index('UGName', ['UGName'], area='Sta_Index_2')
        t.index('UserGroup', ['UserGroup'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('UserGrp')

