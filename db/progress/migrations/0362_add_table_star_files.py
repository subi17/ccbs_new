from gearbox.migrations import Migration

class Addfiles(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('files', area='Sta_Data_256',
                       label='File Storage',
                       desc='Name and status')
        t.column('filename', 'character', format='X(50)', initial='',
                 label='Filename',
                 help='Filename with directoryname')
        t.column('filegroup', 'character', format='X(20)', initial='',
                 label='File Group',
                 help='Normaly first directory')
        t.column('filetimestamp', 'character', format='X(14)', initial='',
                 label='TimeStamp',
                 help='File TimeStamp YYYYMMDDhhmmss')
        t.column('filecrc', 'character', format='X(30)', initial='',
                 label='CRC',
                 help='File crc')
        t.index('filename', ['filename'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('filetimestamp', ['filegroup', 'filetimestamp'], area='Sta_Index_2')

    def down(self):
        self.drop_table('files')

