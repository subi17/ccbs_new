from gearbox.migrations import Migration

class AddPreselErr(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('PreselErr', area='Sta_Data_256',
                       label='Preselection Error Codes',
                       dump_name='preseler',
                       desc='Preselection transaction error codes')
        t.column('PSError', 'integer', format='z9', initial='0',
                 label='Error Code',
                 column_label='Error Code')
        t.column('PSEName', 'character', format='x(40)', initial='',
                 label='Error Name',
                 column_label='Error Name',
                 help='Name of error')
        t.index('PSEName', ['PSEName'], area='Sta_Index_2')
        t.index('PSError', ['PSError'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('PreselErr')

