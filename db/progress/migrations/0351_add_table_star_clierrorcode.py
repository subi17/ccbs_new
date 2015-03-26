from gearbox.migrations import Migration

class AddCLIErrorCode(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLIErrorCode', area='Schema Area',
                       label='CLIErrorCode',
                       dump_name='clierror',
                       desc='Contains the errorcodes')
        t.column('ErrCode', 'integer', format='999', initial='0',
                 label='Errorcode',
                 column_label='Errorcode',
                 help='Error code',
                 description='Errorcodes for the responsefile')
        t.column('ErrMsg', 'character', format='X(50)', initial='',
                 label='Errormessage',
                 column_label='Errormessage',
                 help='Error message',
                 description='Explains the errorcode')
        t.column('Action', 'character', format='X(40)', initial='',
                 column_label='Action',
                 description='Action to be taken if error occurs')
        t.index('ErrCode', ['ErrCode'], area='Schema Area',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CLIErrorCode')

