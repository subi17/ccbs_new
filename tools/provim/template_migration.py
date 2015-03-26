from structure import Migration

class AddTable(Migration):

    database = ''

    def up(self):
        t = self.table('name', desc='')
        t.id()
        t.char('name', format='x(20)')
        t.index('idxname', ['idxfield'])

    def down(self):
        self.drop_table('name')

