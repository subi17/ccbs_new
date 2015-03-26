from gearbox.migrations import Migration

class AddCLIWL(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('CLIWL', area='Schema Area',
                       label='CLIWL',
                       dump_name='cliwl',
                       desc='Used to store the filename of the list that the switch will read.')
        t.column('FileName', 'character', format='X(14)', initial='',
                 label='Filename',
                 column_label='Filename',
                 help='File name',
                 description='Name of the generated file that the switch will read')
        t.column('OrderId', 'character', initial='',
                 column_label='OrderId',
                 help='Order id',
                 description='The OrderId of the requestfile')
        t.column('Processed', 'logical', initial='no',
                 column_label='Processed',
                 description='Indicates whether the logfile from the switch, has been processed, or not.')
        t.column('ExCode', 'character', format='x(6)', initial='',
                 label='Switch',
                 column_label='Switch',
                 help='Name of the switch this queue belongs to')
        t.index('filename', ['FileName', 'ExCode', 'Processed'], area='Schema Area',
                primary=True)

    def down(self):
        self.drop_table('CLIWL')

