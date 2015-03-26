from gearbox.migrations import Migration

class AddMNPSub(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MNPSub', area='Sta_Data_64',
                       dump_name='mnpsub')
        t.column('CLI', 'character', initial='')
        t.column('MsSeq', 'integer', initial='0')
        t.column('ICC', 'character', initial='')
        t.column('NRN', 'character', initial='')
        t.column('MNPSeq', 'integer', initial='0')
        t.column('PortingTime', 'decimal', decimals=5, format='99999999.99999', initial='0')
        t.index('CLI', ['CLI'], area='Sta_Index_2')
        t.index('MNPSeq', ['MNPSeq'], area='Sta_Index_2',
                primary=True)
        t.index('MsSeq', ['MsSeq'], area='Sta_Index_2')

    def down(self):
        self.drop_table('MNPSub')

