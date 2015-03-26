from gearbox.migrations import Migration

class AddCLIPref(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLIPref', area='Schema Area',
                       label='CLIPref',
                       dump_name='clipref',
                       desc='Partners CLIs\
')
        t.column('Pref', 'character', format='x(4)', initial='',
                 label='CLI Prefix',
                 column_label='CLI Prefix',
                 help='Partners prefix')
        t.column('CLI', 'character', format='x(12)', initial='',
                 column_label='CLI')
        t.column('CLIId', 'character', format='x(20)', initial='',
                 label='CLI ID',
                 column_label='CLI ID',
                 help='CLI id')
        t.column('State', 'integer', format='9', initial='0',
                 column_label='State')
        t.index('CLI', ['CLI', 'Pref'], area='Schema Area',
                unique=True)
        t.index('Pref', ['Pref', 'CLI'], area='Schema Area',
                primary=True, unique=True)

    def down(self):
        self.drop_table('CLIPref')

