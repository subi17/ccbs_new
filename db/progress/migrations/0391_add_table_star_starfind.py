from gearbox.migrations import Migration

class Addstarfind(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('starfind', area='Sta_Data_256',
                       label='Data to starFind rutine')
        t.column('queryname', 'character', format='X(15)', initial='',
                 label='Query',
                 help='Name of query. Exsample CustomerName')
        t.column('tablename', 'character', format='X(15)', initial='',
                 label='Table',
                 help='Query table name')
        t.column('fieldname', 'character', mandatory=True, format='X(40)', initial='',
                 label='Field',
                 help='Key field names of query. Separed by comma')
        t.column('displayfield', 'character', mandatory=True, format='X(15)', initial='',
                 label='Display',
                 help='Field name of displayfield')
        t.index('querybane', ['queryname'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('tablename', ['tablename'], area='Sta_Index_2')

    def down(self):
        self.drop_table('starfind')

