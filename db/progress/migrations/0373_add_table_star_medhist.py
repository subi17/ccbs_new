from gearbox.migrations import Migration

class AddMedHist(Migration):

    dumped_on = 'propus'
    database = 'star'

    def up(self):
        t = self.table('MedHist', area='Sta_Data_256',
                       label='MedHist',
                       dump_name='medhist',
                       desc='CDR preprosessor file history')
        t.column('FileName', 'character', format='x(20)', initial='',
                 column_label='FileName',
                 help='Name of the configuration file')
        t.column('Ident', 'character', initial='',
                 column_label='Ident',
                 help='Identification')
        t.column('Date', 'date', format='99-99-9999',
                 column_label='Date',
                 help='Valid from date')
        t.column('FileTime', 'character', initial='',
                 label='Time',
                 column_label='Time',
                 help='Valid from time')
        t.column('FileExt', 'integer', format='>>>9', initial='0',
                 label='Extension',
                 column_label='Extension')
        t.index('Date', ['Date', 'FileTime', 'Ident', 'FileName'], area='Sta_Index_2',
                unique=True)
        t.index('file', ['Ident', 'FileExt'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('ftam-switch', ['Ident', 'FileName', 'Date', 'FileTime'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('MedHist')

