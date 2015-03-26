from gearbox.migrations import Migration

class AddMthCall(Migration):

    dumped_on = 'propus'
    database = 'common'

    def up(self):
        t = self.table('MthCall', area='Sta_Data_256',
                       label='Customer\'s Monthly Calls',
                       dump_name='mthcall',
                       desc='Customer\'s monthly calls')
        t.column('CustNum', 'integer', mandatory=True, format='>>>>>>>>9', initial='0',
                 label='Customer',
                 column_label='Customer',
                 help='Customer number, 1 - 999999')
        t.column('Month', 'integer', format='999999', initial='0',
                 column_label='Month',
                 help='Year and month (YYYYMM)',
                 description='Year and month')
        t.column('Called', 'decimal', decimals=2, format='zz,zz9.99', initial='0',
                 column_label='Called',
                 help='Called so far')
        t.column('Limit', 'integer', format='>,>>>,>>9', initial='0',
                 label='CustLimit',
                 column_label='CustLimit',
                 help='Customer\'s monthly limit',
                 description='Customer\'s monthly limit')
        t.column('CloseDate', 'date', format='99-99-99',
                 label='Closing Date',
                 column_label='Cl.date',
                 help='Closing date',
                 description='Closing date')
        t.column('Printed', 'logical', initial='no',
                 column_label='Pr.',
                 help='Is this record printed')
        t.column('CloseType', 'integer', format='9', initial='0',
                 label='Closing Value',
                 column_label='V',
                 help='Closing value, 1 = Limit, 2 = Invoice, 3 = Both')
        t.column('Alarm', 'logical', initial='no',
                 column_label='Alarm',
                 help='Is alarm given for this month ?')
        t.index('as-nro', ['CustNum', 'Month'], area='Sta_Index_2',
                primary=True, unique=True)
        t.index('closed', ['CloseDate', 'CustNum', 'Month'], area='Sta_Index_2')
        t.index('Month', ['Month', 'CustNum'], area='Sta_Index_2',
                unique=True)

    def down(self):
        self.drop_table('MthCall')

