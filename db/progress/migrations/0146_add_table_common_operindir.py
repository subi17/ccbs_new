from gearbox.migrations import Migration

class AddOperIndir(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('OperIndir', area='Sta_Data_256',
                       label='Operator\'s prefixes',
                       dump_name='operindi',
                       desc='Other operators indirect prefixes')
        t.column('Operator', 'character', mandatory=True, initial='',
                 column_label='Operator',
                 help='Operator\'s code, 1 - 8 characters')
        t.column('Prefix', 'character', format='x(5)', initial='',
                 label='Indirect Prefix',
                 column_label='Indirect Prefix',
                 help='Operators indirect prefix')
        t.column('Billable', 'logical', format='Y/N', initial='no',
                 label='B',
                 column_label='B',
                 help='Will the indirect calls be billed from this operator ?')
        t.column('DestType', 'integer', format='>9', initial='0',
                 label='Type',
                 column_label='Type',
                 help='B-subscriber type, used with FTAM server.')
        t.column('Absolute', 'logical', initial='no',
                 label='Abs.',
                 column_label='Abs.',
                 help='Is the b-number type analyse absolute ?')
        t.index('Operator', ['Operator', 'Prefix'], area='Sta_Index_2',
                primary=True, unique=True)

    def down(self):
        self.drop_table('OperIndir')

