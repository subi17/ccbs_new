from gearbox.migrations import Migration

class AddDMarketing(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('DMarketing', area='Sta_Data_256',
                       label='Direct marketing',
                       dump_name='dmarketi',
                       desc='Direct marketing codes')
        t.column('DirMark', 'character', initial='',
                 label='Direct Marketing',
                 column_label='Dir.Market',
                 help='Direct marketing code')
        t.column('DirMarkName', 'character', format='x(30)', initial='',
                 label='DM Name',
                 column_label='DMName',
                 help='Direct marketing name')
        t.column('Brand', 'character', initial='',
                 column_label='Brand',
                 help='Code Of Brand')
        t.index('DirMark', ['Brand', 'DirMark'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('DirMarkName', ['Brand', 'DirMarkName'], area='Sta_Index_2')

    def down(self):
        self.drop_table('DMarketing')

