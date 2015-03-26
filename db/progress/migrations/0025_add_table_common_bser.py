from gearbox.migrations import Migration

class AddBSer(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('BSer', area='Sta_Data_256',
                       dump_name='bser')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer\'s number')
        t.column('CliFrom', 'character', format='x(12)', initial='',
                 label='B-Sub From',
                 column_label='B-Sub From',
                 help='B-Number series lower limit')
        t.column('CliTo', 'character', format='x(12)', initial='',
                 label='B-sub To',
                 column_label='B-sub To',
                 help='B-Number series upper limit')
        t.index('b-asno', ['CustNum', 'CliFrom'], area='Sta_Index_2')
        t.index('b-nr', ['CliFrom', 'CliTo', 'CustNum'], area='Sta_Index_2',
                primary=True)

    def down(self):
        self.drop_table('BSer')

