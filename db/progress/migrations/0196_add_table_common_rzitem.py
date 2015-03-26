from gearbox.migrations import Migration

class AddRZItem(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('RZItem', area='Sta_Data_128',
                       desc='Roaming Zone for PLMN')
        t.column('PlmnCode', 'character', initial='',
                 column_label='PLMN Code',
                 help='Code of PLMN')
        t.column('CountryPrefix', 'character', format='x(6)', initial='',
                 column_label='CCode',
                 help='Country Code ()')
        t.column('RoamZone', 'character', format='x(12)', initial='',
                 label='RZItem',
                 column_label='RoamZone',
                 help='RoamZone')
        t.column('DialType', 'integer', format='>>9', initial='0',
                 column_label='RoamZone',
                 help='Dialtype for RZItem')
        t.index('CountryPrefix', ['CountryPrefix'], area='Sta_Index_2')
        t.index('PLMNCode', ['PlmnCode'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('RoamZone', ['RoamZone', 'PlmnCode'], area='Sta_Index_2')

    def down(self):
        self.drop_table('RZItem')

