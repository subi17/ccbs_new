from gearbox.migrations import Migration

class AddRoamGPRS(Migration):

    dumped_on = 'propus'
    database = 'roamcdr'

    def up(self):
        t = self.table('RoamGPRS', area='Dyn_Data_64',
                       dump_name='roamcdr-------1')
        t.column('TSRead', 'decimal', decimals=5, format='99999999.99999', initial='0',
                 label='ReadIn',
                 column_label='ReadIn',
                 help='TimeStamp when EDR was read')
        t.column('DateStart', 'date', format='99-99-99',
                 label='Date',
                 column_label='Date')
        t.column('TimeStart', 'integer', format='99999', initial='0',
                 label='Time',
                 column_label='Time')
        t.column('PLMN', 'character', format='x(5)', initial='',
                 column_label='PLMN')
        t.column('CSV', 'character', initial='',
                 column_label='CSV')
        t.column('Units', 'integer', format='>>>>>>9', initial='0',
                 column_label='Units')
        t.column('Version', 'character', format='x(6)', initial='',
                 column_label='Version')
        t.column('DateRead', 'date', format='99-99-99',
                 column_label='DateRead')
        t.column('EventType', 'character', format='x(4)', initial='',
                 column_label='EventType')
        t.column('RSubType', 'integer', format='>>9', initial='0',
                 column_label='RSubType',
                 description='Roaming subscriber type')
        t.column('Amount', 'decimal', decimals=6, format='->>>>9.999999', initial='0',
                 column_label='Amount')
        t.column('ChargedParty', 'integer', format='>9', initial='0',
                 column_label='ChargedParty')
        t.column('CLI', 'character', format='x(10)', initial='',
                 column_label='CLI')
        t.column('GsmBnr', 'character', format='x(12)', initial='',
                 label='Bnr',
                 column_label='Bnr')
        t.column('RateZone', 'integer', format='>9', initial='0',
                 column_label='RateZone')
        t.column('PartInd', 'integer', format='9', initial='0')
        t.column('PartRecNum', 'integer', format='>>9', initial='0')
        t.column('CallIdNum', 'int64', format='>>>>>>>>>9', initial='0',
                 column_label='CallIdNum',
                 help='Call identification number')
        t.column('GGSNAddr', 'character', format='x(15)', initial='',
                 column_label='GGSNAddr',
                 help='SGSN address')
        t.index('DateStart', ['DateStart', 'TimeStart', 'CLI'], area='Dyn_Index2',
                primary=True)
        t.index('GPRS', ['DateRead', 'DateStart', 'TimeStart', 'CallIdNum', 'PartRecNum', 'GGSNAddr'], area='Dyn_Index1')

    def down(self):
        self.drop_table('RoamGPRS')

