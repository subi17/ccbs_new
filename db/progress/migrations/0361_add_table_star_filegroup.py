from gearbox.migrations import Migration

class Addfilegroup(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('filegroup', area='Sta_Data_256',
                       dump_name='filegrp')
        t.column('filegroup', 'character', format='X(12)', initial='',
                 label='File Group',
                 help='File group')
        t.column('filetimestamp', 'character', format='X(14)', initial='',
                 label='TimeStamp',
                 help='File TimeStamp YYYYMMDDhhmmss')

    def down(self):
        self.drop_table('filegroup')

